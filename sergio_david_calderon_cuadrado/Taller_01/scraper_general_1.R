# (El navegador ya NO se fija aqui: se detecta en [0b], y solo en MODO = "linea".)

# =====================================================================
#  SCRAPER UNICO:  EXITO + FARMATODO + LA REBAJA  ->  base comun  ->  productos comparables
#
#  COMO SE USA
#    Ajusta la seccion [0] y corre TODO el archivo de una vez
#    (source("scraper_general.R") o seleccionar todo + ejecutar).
#
#    MODO = "linea"    scrapea cada comercio, estandariza y trabaja la base
#                      EN MEMORIA (objeto `base_general`). Los CSV de raw/ son
#                      solo un RESPALDO: se escriben y NO se leen (solo se mira
#                      su encabezado para no mezclar esquemas distintos).
#    MODO = "respaldo" no scrapea: arma `base_general` leyendo esos CSV. Sirve
#                      para continuar el analisis sin repetir el scraping.
#
#  COMO SE EVITAN LOS CONFLICTOS ENTRE COMERCIOS
#    - [2] NUCLEO COMUN: se define UNA vez. No usa objetos globales de un
#          comercio: recibe `cm` (la ficha del comercio) como argumento.
#    - [3], [4] y [5]: todo lo propio de cada comercio lleva prefijo (ex_ = Exito,
#          fa_ = Farmatodo, lr_ = La Rebaja): selectores, catalogo de busquedas,
#          URL, extraccion.
#    - [6] COMERCIOS: registro que une cada ficha con sus funciones.
#          Un comercio nuevo = copiar un bloque, cambiar el prefijo y
#          agregar su entrada en COMERCIOS (y su nombre en COMERCIOS_A_CORRER).
#    - Mismo esquema (COLUMNAS_FINALES) y mismos tipos en todos: bind_rows directo.
# =====================================================================


# #####################################################################
# [0] CONFIGURACION
# #####################################################################
# MODO: lo define quien llama (el .qmd con params$modo, o la variable de entorno
# TALLER_MODO). Si nadie lo define, el valor por defecto es "respaldo" (no scrapea).
if (!exists("MODO", inherits = TRUE)) MODO <- Sys.getenv("TALLER_MODO", unset = "respaldo")
if (!MODO %in% c("linea", "respaldo")) stop('MODO debe ser "linea" o "respaldo"', call. = FALSE)

COMERCIOS_A_CORRER <- c("exito", "farmatodo", "larebaja")
TERMINOS_A_BUSCAR  <- NULL      # NULL = todas las del catalogo; o c("exfoliante", "estrías")
PAUSA_PAGINA       <- 4         # segundos de espera tras abrir cada URL
RESPALDAR          <- TRUE      # escribir el respaldo CSV por comercio (solo en modo "linea")
COMPLETAR_CON_RESPALDOS <- TRUE # un producto a comparar que no salio en esta corrida se toma
# de los respaldos (solo en modo "linea"; ver [9])

# Carpeta del proyecto (donde estan los .R y el .qmd). Orden de prioridad:
#   1) un DIR_TALLER ya definido por quien llama (el .qmd)
#   2) la variable de entorno TALLER_DIR
#   3) la carpeta de trabajo actual (al renderizar, Quarto la fija en la carpeta del .qmd)
# Las subcarpetas raw/ y processed/ se CREAN solas si no existen.
if (!exists("DIR_TALLER", inherits = TRUE)) {
  DIR_TALLER <- Sys.getenv("TALLER_DIR", unset = getwd())
}
DIR_TALLER <- normalizePath(DIR_TALLER, winslash = "/", mustWork = TRUE)
DIR_RAW            <- file.path(DIR_TALLER, "raw")          # respaldos
DIR_SALIDA         <- file.path(DIR_TALLER, "processed")    # salidas del analisis
RUTA_LOG           <- file.path(DIR_RAW, "log_consultas.csv")

# Exito carga muchisimos scripts (ads, analitica) y el evento "load" de Chrome
# puede tardar mas que el timeout por defecto de chromote.
TIMEOUT_CHROMOTE <- 120
options(chromote.timeout = TIMEOUT_CHROMOTE)


# #####################################################################
# [1] PAQUETES
# #####################################################################
paquetes <- c("rvest", "xml2", "dplyr", "stringr", "purrr", "tibble", "tidyr", "DBI", "RSQLite")
if (MODO == "linea") paquetes <- c(paquetes, "chromote")     # el navegador solo se usa al scrapear
pendientes <- setdiff(paquetes, rownames(installed.packages()))
if (length(pendientes) > 0) {
  repos <- getOption("repos")
  if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) repos <- c(CRAN = "https://cloud.r-project.org")
  install.packages(pendientes, repos = repos)
}
invisible(lapply(paquetes, library, character.only = TRUE))

# ---------------------------------------------------------------------
# [0b] NAVEGADOR (solo MODO = "linea"). Funciona en Windows, macOS y Linux, con o sin Brave:
#   1) si ya existe la variable de entorno CHROMOTE_CHROME, se respeta
#   2) si no, se busca Brave en las rutas habituales del sistema
#   3) si no hay Brave, chromote busca Chrome / Chromium / Edge por su cuenta
#   4) si no hay ninguno, se detiene con un mensaje claro (y se sugiere MODO = "respaldo")
# ---------------------------------------------------------------------
configurar_navegador <- function() {
  if (!nzchar(Sys.getenv("CHROMOTE_CHROME"))) {
    sistema <- Sys.info()[["sysname"]]
    candidatos <- switch(sistema,
      Windows = c(
        file.path(Sys.getenv("ProgramFiles"),        "BraveSoftware", "Brave-Browser", "Application", "brave.exe"),
        file.path(Sys.getenv("ProgramFiles(x86)"),   "BraveSoftware", "Brave-Browser", "Application", "brave.exe"),
        file.path(Sys.getenv("LOCALAPPDATA"),        "BraveSoftware", "Brave-Browser", "Application", "brave.exe")),
      Darwin = "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser",
      c("/usr/bin/brave-browser", "/usr/bin/brave", "/snap/bin/brave", "/opt/brave.com/brave/brave-browser"))
    brave <- candidatos[file.exists(candidatos)]
    if (length(brave) > 0) Sys.setenv(CHROMOTE_CHROME = brave[1])
  }
  ruta <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  if (is.null(ruta) || !nzchar(ruta)) {
    stop("MODO = \"linea\" necesita Brave, Chrome, Chromium o Edge instalado.\n",
         "  Instala uno, o define CHROMOTE_CHROME con la ruta del ejecutable,\n",
         "  o usa MODO = \"respaldo\" (no scrapea: lee los CSV de raw/).", call. = FALSE)
  }
  cat("Navegador para el scraping:", ruta, "\n")
  invisible(ruta)
}

if (MODO == "linea") {
  configurar_navegador()
  # (R6: se modifica en su sitio; no se puede asignar sobre el resultado de una llamada)
  try({
    .navegador <- chromote::default_chromote_object()
    .navegador$default_timeout <- TIMEOUT_CHROMOTE
  }, silent = TRUE)
}


# #####################################################################
# [1b] PRODUCTOS A COMPARAR
#     Productos ya identificados y validados a mano: nombre unico + SKU en cada
#     comercio (como texto). Para agregar uno, agregar una fila con un id nuevo.
#     Estos SKU NUNCA se descartan por las reglas de nombre de las busquedas
#     (p. ej. un nombre que no dice "corporal"), para que siempre queden en la base.
# #####################################################################
CATALOGO_COMPARABLES <- tibble::tribble(
  ~id_producto, ~producto_comparable,                     ~exito,    ~farmatodo,  ~larebaja,
  1L, "Lubriderm humectación diaria 200 ml",              "3155351", "293400550", "3493",
  2L, "Lubriderm humectación diaria 400 ml",              "3155352", "293400592", "2589",
  3L, "Lubriderm humectación diaria 946 ml",              "3155353", "291550571", "9796",
  4L, "Lubriderm humectación delicada 400 ml",            "3179125", "203000123", "2642",
  5L, "Lubriderm reparación intensiva 946 ml",            "869499",  "1046197",   "9795",
  6L, "Lubriderm humectación diaria 120 ml",              "3155350", "293400568", "2588",
  7L, "Lubriderm FPS 15 200 ml",                          "311800",  "1015664",   "3494",
  8L, "Lubriderm FPS 15 400 ml",                          "128396",  "213300197", "4608",
  9L, "Nivea hidratación express 1000 ml",                "283481",  "1043756",   "1431"
)

# mismo catalogo en formato largo: una fila por (producto, comercio, sku)
catalogo_largo <- CATALOGO_COMPARABLES %>%
  tidyr::pivot_longer(-c(id_producto, producto_comparable),
                      names_to = "comercio", values_to = "sku") %>%
  filter(!is.na(sku))

stopifnot(
  !anyDuplicated(CATALOGO_COMPARABLES$id_producto),
  !anyDuplicated(catalogo_largo[, c("comercio", "sku")])
)


# #####################################################################
# [2] NUCLEO COMUN  (identico para todos los comercios)
#
#  CONTRATO: la ficha `cm` de cada comercio es una lista con
#    nombre           texto corto, minusculas ("exito", "farmatodo", "larebaja")
#    catalogo         lista de busquedas (formato mas abajo)
#    url_del_filtro   function(cfg, f)  -> URL de un filtro
#    scrapear_pagina  function(url, pausa, ts) -> tibble con id_ejecucion,
#                     fecha_consulta + las columnas de tabla_productos_vacia()
#  y la extraccion de cada producto debe entregar, como TEXTO CRUDO:
#    precio_cop          precio de LISTA (el "antes"; si no hay descuento, el unico)
#    precio_oferta       precio con descuento (NA si no hay)
#    condicion_promocion textos de la condicion del descuento (NA si no hay)
#
#  FORMATO DE UN CATALOGO:
#    CATALOGO[[clave]] = list(
#       termino = "texto que se envia a la pagina y se guarda en `busqueda`",
#       alias   = c("otras formas de escribirlo"),                # opcional
#       filtros = list( list(etiqueta = "...", <campos propios del comercio>,
#                            regla = list(requerir = c(...), excluir = c(...))) ) )
#    La clave va normalizada (minusculas, sin tildes).
# #####################################################################
REINTENTOS <- 2     # reintentos por URL ante un error o un resultado vacio

# ---------------------------------------------------------------------
# DESCUENTO GENERAL vs CONDICIONAL
#   Solo cuenta como OFERTA GENERAL el descuento que recibe CUALQUIER cliente.
#   Es condicional (NO general) si su condicion menciona alguno de estos
#   terminos: primera compra, Prime, tarjeta, cupon, app/web, membresia, banco...
# ---------------------------------------------------------------------
REGEX_DESCUENTO_CONDICIONAL <- "(?i)\\b(1ra|primera|prime|tarjeta|cup[oó]n|app|web|membres[ií]a|banco|bancolombia|davivienda|afiliad[oa]s?|suscri\\w*|c[oó]digo|club)\\b"

# ---------------------------------------------------------------------
# ESQUEMA FINAL (igual para todos los comercios -> bind_rows directo)
# ---------------------------------------------------------------------
COLUMNAS_FINALES <- c(
  "comercio", "busqueda", "id_ejecucion", "fecha_consulta",
  "sku", "nombres", "marca", "presentacion", "envase",
  "medida", "cantidad_unidad", "unidades", "cantidad_total",
  "precio_lista_cop", "precio_real_cop", "oferta_general", "precio_medida",
  "url", "texto_original_anuncio"
)
TIPOS_COLUMNAS <- c(
  comercio = "character", busqueda = "character", id_ejecucion = "character",
  fecha_consulta = "character", sku = "character", nombres = "character",
  marca = "character", presentacion = "character", envase = "character",
  medida = "character", cantidad_unidad = "numeric", unidades = "numeric",
  cantidad_total = "numeric", precio_lista_cop = "integer", precio_real_cop = "integer",
  oferta_general = "integer", precio_medida = "numeric", url = "character",
  texto_original_anuncio = "character"
)
#  medida           "ml" o "g"
#  cantidad_unidad  contenido de UNA unidad, en `medida`
#  unidades         numero de unidades del paquete
#  cantidad_total   unidades * cantidad_unidad
#  precio_lista_cop precio sin descuento (entero, pesos)
#  precio_real_cop  lo que paga CUALQUIER cliente hoy (con el descuento general si existe)
#  oferta_general   1 = precio_real incluye un descuento general ; 0 = no
#  precio_medida    precio_real_cop por cada ml (o g)

# Lleva cualquier tabla (de la memoria o leida de un CSV) a las columnas y tipos
# del esquema: asi la base queda IGUAL venga de donde venga.
tipificar_base <- function(df) {
  faltan <- setdiff(COLUMNAS_FINALES, names(df))
  if (length(faltan) > 0) {
    stop("Faltan columnas del esquema final: ", paste(faltan, collapse = ", "), call. = FALSE)
  }
  df <- dplyr::select(tibble::as_tibble(df), dplyr::all_of(COLUMNAS_FINALES))
  for (col in COLUMNAS_FINALES) {
    df[[col]] <- switch(TIPOS_COLUMNAS[[col]],
                        character = as.character(df[[col]]),
                        integer   = suppressWarnings(as.integer(df[[col]])),
                        numeric   = suppressWarnings(as.numeric(df[[col]])))
  }
  df
}


# =====================================================================
# UTILIDADES
# =====================================================================
# minusculas, sin tildes, espacios simples ("Estrías  " -> "estrias")
normalizar_texto <- function(x) {
  x <- tolower(x)
  x <- chartr("áéíóúüñ", "aeiouun", x)
  trimws(gsub("\\s+", " ", x))
}

# PRECIO -> ENTERO en pesos, sin "$", sin puntos de miles, sin decimales.
#   "$95.000" -> 95000L ; "$ 1.234.500" -> 1234500L
# (entero a proposito: un 'double' como 100000 se guardaria como 1e+05 en el CSV)
precio_a_cop <- function(x) {
  x <- str_remove_all(x, "[^0-9.,]")
  x <- str_remove(x, ",\\d{1,2}$")
  as.integer(round(as.numeric(str_remove_all(x, "[.,]"))))
}

# "1.000" -> 1000 (miles) ; "1,5" -> 1.5 ; "450" -> 450   (para contenidos: 1,5 lt)
a_numero <- function(x) {
  x <- str_remove_all(x, "\\s")
  miles <- str_detect(x, "^\\d{1,3}([.,]\\d{3})+$")
  x <- if_else(miles, str_remove_all(x, "[.,]"), str_replace(x, ",", "."))
  as.numeric(x)
}

# une varios textos distintos en uno ("A | B"); NA si no hay ninguno
unir_textos <- function(x) {
  x <- unique(str_squish(x[!is.na(x)]))
  x <- x[nzchar(x)]
  if (length(x) == 0) NA_character_ else paste(x, collapse = " | ")
}

marca_tiempo <- function(momento = Sys.time()) {
  fecha <- format(momento, "%Y-%m-%dT%H:%M:%S%z", tz = "America/Bogota")
  list(
    fecha_consulta = sub("(\\d{2})(\\d{2})$", "\\1:\\2", fecha),
    id_ejecucion   = format(momento, "%Y%m%d%H%M%S", tz = "America/Bogota")
  )
}

# Tabla vacia con la estructura CRUDA que devuelve la extraccion
tabla_productos_vacia <- function() {
  tibble(id_ejecucion = character(),
         fecha_consulta = character(),
         nombres = character(),
         presentacion = character(),
         marca = character(),
         precio_cop = character(),
         precio_oferta = character(),
         condicion_promocion = character(),
         url = character(),
         sku = character(),
         texto_original_anuncio = character())
}


# =====================================================================
# CATALOGO: busqueda de la configuracion y descarte por nombre
# =====================================================================
obtener_config_busqueda <- function(cm, termino_busqueda) {
  clave <- normalizar_texto(paste(termino_busqueda, collapse = " "))
  for (k in names(cm$catalogo)) {
    cfg <- cm$catalogo[[k]]
    if (clave %in% c(k, normalizar_texto(cfg$alias))) {
      if (is.null(cfg$termino)) cfg$termino <- k
      return(cfg)
    }
  }
  stop("[", cm$nombre, "] La busqueda '", clave, "' no esta en su catalogo.\n  Disponibles: ",
       paste(names(cm$catalogo), collapse = " | "), call. = FALSE)
}

# `conservar`: SKU que nunca se descartan (los productos a comparar, [1b])
aplicar_regla_descarte <- function(datos, regla, conservar = character(0)) {
  if (is.null(regla) || nrow(datos) == 0) return(datos)
  
  n     <- normalizar_texto(datos$nombres)
  tiene <- function(palabra) grepl(paste0("\\b", palabra, "\\b"), n, perl = TRUE)
  
  ok <- rep(TRUE, length(n))
  for (p in regla$requerir) ok <- ok & tiene(p)
  for (p in regla$excluir)  ok <- ok & !tiene(p)
  ok <- ok | (trimws(datos$sku) %in% conservar)
  
  datos[ok, , drop = FALSE]
}


# =====================================================================
# SEGURIDAD (1/3): CADA CONSULTA ES SEGURA Y QUEDA REGISTRADA
#    Reintenta, guarda el MOTIVO del fallo y distingue "la pagina fallo"
#    de "la busqueda no tiene resultados".
# =====================================================================
LOG <- new.env()
reiniciar_log <- function() {
  LOG$filtros <- tibble(comercio = character(), id_ejecucion = character(),
                        busqueda = character(), filtro = character(),
                        url = character(), estado = character(), intentos = integer(),
                        n_capturados = integer(), n_tras_descarte = integer(), mensaje = character())
}
reiniciar_log()

registrar_log <- function(comercio, id_ejecucion, busqueda, filtro = NA_character_, url = NA_character_,
                          estado, intentos = NA_integer_, n_capturados = NA_integer_,
                          n_tras_descarte = NA_integer_, mensaje = NA_character_) {
  LOG$filtros <- bind_rows(LOG$filtros, tibble(
    comercio = comercio, id_ejecucion = id_ejecucion, busqueda = busqueda, filtro = filtro,
    url = url, estado = estado, intentos = as.integer(intentos),
    n_capturados = as.integer(n_capturados), n_tras_descarte = as.integer(n_tras_descarte),
    mensaje = mensaje))
}

# Devuelve SIEMPRE una lista: datos (nunca falla), estado, intentos, mensaje
#   estado = "ok" | "sin_resultados" | "error"
scrapear_pagina_segura <- function(cm, url, pausa = 4, ts = marca_tiempo(), reintentos = REINTENTOS) {
  force(ts)
  mensaje <- NA_character_
  datos   <- tabla_productos_vacia()
  for (i in seq_len(reintentos + 1)) {
    res <- tryCatch(cm$scrapear_pagina(url, pausa = pausa, ts = ts), error = function(e) e)
    
    if (inherits(res, "error")) {
      mensaje <- conditionMessage(res)
      cat("   intento", i, "de", reintentos + 1, "FALLO:", mensaje, "\n")
    } else if (nrow(res) == 0) {
      mensaje <- NA_character_
      cat("   intento", i, "de", reintentos + 1, "sin productos\n")
    } else {
      return(list(datos = res, estado = "ok", intentos = i, mensaje = NA_character_))
    }
    if (i <= reintentos) Sys.sleep(3 * i)
  }
  estado <- if (is.na(mensaje)) "sin_resultados" else "error"
  list(datos = datos, estado = estado, intentos = reintentos + 1, mensaje = mensaje)
}


# =====================================================================
# UNA BUSQUEDA COMPLETA: una URL por filtro -> descarte -> unir -> sin duplicados
#    Cada filtro es independiente: si uno falla, los demas se conservan.
# =====================================================================
scrapear_busqueda <- function(cm, termino_busqueda, pausa = 4, ts = marca_tiempo()) {
  force(ts)                      # misma fecha / id_ejecucion para TODAS las URLs
  cfg <- obtener_config_busqueda(cm, termino_busqueda)
  conservar <- catalogo_largo$sku[catalogo_largo$comercio == cm$nombre]
  
  resultados <- purrr::map(cfg$filtros, function(f) {
    url <- cm$url_del_filtro(cfg, f)
    cat("\n>>", f$etiqueta, "\n   ", url, "\n")
    
    r <- scrapear_pagina_segura(cm, url, pausa = pausa, ts = ts)
    
    datos <- tryCatch(aplicar_regla_descarte(r$datos, f$regla, conservar), error = function(e) {
      r$estado  <<- "error"
      r$mensaje <<- paste("descarte:", conditionMessage(e))
      tabla_productos_vacia()
    })
    
    cat("   estado:", r$estado, "|", nrow(r$datos), "capturados ->", nrow(datos), "tras el descarte\n")
    registrar_log(cm$nombre, ts$id_ejecucion, cfg$termino, f$etiqueta, url, r$estado, r$intentos,
                  nrow(r$datos), nrow(datos), r$mensaje)
    datos
  })
  
  # todo lo crudo es texto: se fuerza para que ningun filtro pueda romper la union
  resultados <- purrr::map(resultados, ~ dplyr::mutate(.x, dplyr::across(dplyr::everything(), as.character)))
  res <- dplyr::bind_rows(resultados)
  if (nrow(res) == 0) return(res)
  
  n_antes <- nrow(res)
  res <- res %>% distinct(sku, .keep_all = TRUE)       # una busqueda por filtro => coincidencias
  cat("\nDuplicados eliminados entre filtros:", n_antes - nrow(res), "\n")
  # informativo: nunca debe poder tumbar una busqueda ya scrapeada
  tryCatch(reportar_promociones(res), error = function(e) cat("   (no se pudo resumir las promociones: ", conditionMessage(e), ")\n", sep = ""))
  res
}


# =====================================================================
# DESCUENTOS: SOLO LA OFERTA GENERAL
#     oferta_general = 1  <=>  hay precio con descuento menor al de lista
#                              Y ninguna condicion coincide con
#                              REGEX_DESCUENTO_CONDICIONAL
#   precio_real_cop = precio con descuento si oferta_general = 1; si no, el de lista.
#   (Los descuentos condicionales -primera compra, Prime, tarjeta...- no se
#    usan para el precio; el texto completo queda en texto_original_anuncio.)
# =====================================================================
normalizar_precios <- function(df) {
  df %>%
    mutate(
      precio_lista_cop = precio_a_cop(precio_cop),
      .oferta      = precio_a_cop(precio_oferta),
      .condicional = str_detect(coalesce(condicion_promocion, ""), REGEX_DESCUENTO_CONDICIONAL),
      oferta_general = as.integer(!is.na(.oferta) & !is.na(precio_lista_cop) &
                                    .oferta < precio_lista_cop & !.condicional),
      precio_real_cop = if_else(oferta_general == 1L, .oferta, precio_lista_cop)
    )
}

# Muestra en consola que descuentos se vieron y como se clasificaron
reportar_promociones <- function(df) {
  if (nrow(df) == 0) return(invisible(NULL))
  lista  <- precio_a_cop(df$precio_cop)
  oferta <- precio_a_cop(df$precio_oferta)
  con_desc <- !is.na(oferta) & !is.na(lista) & oferta < lista
  cond <- df$condicion_promocion[con_desc]
  
  general <- con_desc & !str_detect(coalesce(df$condicion_promocion, ""), REGEX_DESCUENTO_CONDICIONAL)
  cat("   descuentos: ", sum(con_desc), " de ", nrow(df), " productos | oferta general: ", sum(general), "\n", sep = "")
  
  vistos <- table(coalesce(cond, "(sin condicion)"))
  for (t in names(vistos)) {
    clase <- if (t == "(sin condicion)") "GENERAL (sin condicion)" else
      if (str_detect(t, REGEX_DESCUENTO_CONDICIONAL)) "condicional (NO general)" else "GENERAL"
    cat(sprintf("      %-38s -> %-26s n = %d\n", t, clase, vistos[[t]]))
  }
  invisible(NULL)
}


# =====================================================================
# CONTENIDO: medida (ml / g), cantidad, unidades, envase y precio por medida
#   El texto de busqueda es PRESENTACION + NOMBRE (en unos comercios el tamano
#   viene en la presentacion y en otros dentro del nombre).
# =====================================================================
RX_NUM  <- "\\d+(?:[.,]\\d+)*"
RX_UNID <- "(?:ml|cc|mililitros?|l|lt|lts|litros?|g|gr|grs|gramos?|kg|kilos?)"
RX_ENVASE <- "\\b(botella|frasco|pote|tarro|tubo|caja|barra|sobre|bolsa|dispensador|atomizador|spray|ampolla|sachet|lata)\\b"

# contenido: primero "x 500 ml", y si no hay "x", cualquier "500 ml" suelto
rx_cont_x   <- regex(paste0("\\bx\\s*(", RX_NUM, ")\\s*(", RX_UNID, ")\\b"), ignore_case = TRUE)
rx_cont_any <- regex(paste0("(?<![\\d.,])(", RX_NUM, ")\\s*(", RX_UNID, ")\\b"), ignore_case = TRUE)

contenido_cantidad <- function(texto) {
  a_numero(coalesce(str_match(texto, rx_cont_x)[, 2], str_match(texto, rx_cont_any)[, 2]))
}
contenido_unidad <- function(texto) {
  str_to_lower(coalesce(str_match(texto, rx_cont_x)[, 3], str_match(texto, rx_cont_any)[, 3]))
}

# unidades por paquete. "x 30 ml" NO se lee como 30 unidades.
RX_N      <- "([2-9]|[12][0-9]|3[0-6])"
NO_MEDIDA <- paste0("(?!\\s*", RX_UNID, "\\b)")
rx_six  <- regex("\\bsix\\s*-?\\s*pack\\b", ignore_case = TRUE)
rx_x    <- regex(paste0("\\bx\\s*", RX_N, NO_MEDIDA, "\\s*(?:und(?:s)?|unidades?)?\\b"), ignore_case = TRUE)
rx_pack <- regex(paste0("\\b", RX_N, "\\s*-?\\s*pack\\b"), ignore_case = TRUE)
rx_und  <- regex(paste0("\\b", RX_N, "\\s*(?:und(?:s)?|unidades?)\\b"), ignore_case = TRUE)

estandarizar_contenido <- function(df) {
  df %>%
    mutate(
      .texto_medida = str_squish(str_replace_all(paste(coalesce(presentacion, ""), nombres), "[()]", " ")),
      
      envase = coalesce(
        str_extract(normalizar_texto(presentacion), RX_ENVASE),
        str_extract(normalizar_texto(nombres),      RX_ENVASE)
      ) %>% str_replace("^tarro$", "pote"),
      
      .cant = contenido_cantidad(.texto_medida),
      .uni  = contenido_unidad(.texto_medida),
      .uni_std = case_when(
        .uni %in% c("ml", "cc") | str_detect(.uni, "^mililitro") ~ "ml",
        .uni %in% c("l", "lt", "lts") | str_detect(.uni, "^litro") ~ "l",
        .uni %in% c("g", "gr", "grs") | str_detect(.uni, "^gramo") ~ "g",
        .uni == "kg" | str_detect(.uni, "^kilo") ~ "kg"
      ),
      
      medida = case_when(.uni_std %in% c("ml", "l") ~ "ml",
                         .uni_std %in% c("g", "kg") ~ "g"),
      cantidad_unidad = case_when(.uni_std %in% c("ml", "g") ~ .cant,
                                  .uni_std %in% c("l", "kg") ~ .cant * 1000),
      
      unidades = case_when(
        str_detect(.texto_medida, rx_six)  ~ 6,
        str_detect(.texto_medida, rx_x)    ~ as.numeric(str_match(.texto_medida, rx_x)[, 2]),
        str_detect(.texto_medida, rx_pack) ~ as.numeric(str_match(.texto_medida, rx_pack)[, 2]),
        str_detect(.texto_medida, rx_und)  ~ as.numeric(str_match(.texto_medida, rx_und)[, 2]),
        TRUE ~ 1
      ),
      
      cantidad_total = unidades * cantidad_unidad,
      precio_medida  = round(precio_real_cop / cantidad_total, 2)
    )
}

estandarizar_productos <- function(df) {
  if (nrow(df) == 0) return(df)
  df %>% normalizar_precios() %>% estandarizar_contenido()
}


# =====================================================================
# SEGURIDAD (2/3): RESPALDO CSV QUE NUNCA PIERDE DATOS Y NUNCA SE LEE
#   - Se AGREGAN filas al final del archivo (append): la base existente no se
#     carga en memoria. Solo se mira su ENCABEZADO.
#   - Si el encabezado no es el del esquema final (base de una version anterior),
#     el archivo viejo se aparta intacto como *_ANTES_DE_UNIFICAR_<fecha>.csv y
#     se empieza uno nuevo con el esquema actual.
#   - Escribe sin notacion cientifica (100000 y no 1e+05).
#   - Si algo falla, los datos nuevos quedan en RESCATE_*.csv junto al respaldo.
# =====================================================================
rescatar_datos <- function(df, ruta_base, motivo) {
  slug   <- gsub("[^a-z0-9]+", "_", normalizar_texto(df$busqueda[1]))
  nombre <- paste0("RESCATE_", df$id_ejecucion[1], "_", slug, "_", basename(ruta_base))
  ruta_r <- file.path(dirname(ruta_base), nombre)
  
  old <- options(scipen = 999); on.exit(options(old), add = TRUE)
  ok <- tryCatch({ write.csv(df, ruta_r, row.names = FALSE); TRUE }, error = function(e) FALSE)
  if (!ok) { ruta_r <- file.path(tempdir(), nombre); write.csv(df, ruta_r, row.names = FALSE) }
  
  cat("!! NO se pudo escribir el respaldo (", motivo, ").\n   Los datos nuevos quedaron en: ", ruta_r, "\n", sep = "")
  invisible(FALSE)
}

guardar_respaldo <- function(df, ruta_base) {
  if (nrow(df) == 0) return(invisible(TRUE))
  old <- options(scipen = 999); on.exit(options(old), add = TRUE)
  
  ok <- tryCatch({
    dir.create(dirname(ruta_base), recursive = TRUE, showWarnings = FALSE)
    existe <- file.exists(ruta_base) && file.size(ruta_base) > 0
    encabezado <- if (existe) {
      tryCatch(names(read.csv(ruta_base, nrows = 0, check.names = FALSE)), error = function(e) character(0))
    } else NULL
    
    if (!existe) {
      write.csv(df, ruta_base, row.names = FALSE)
    } else if (identical(encabezado, COLUMNAS_FINALES)) {
      write.table(df, ruta_base, sep = ",", row.names = FALSE, col.names = FALSE,
                  append = TRUE, qmethod = "double")
    } else {
      copia <- sub("\\.csv$", paste0("_ANTES_DE_UNIFICAR_", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv"), ruta_base)
      if (!file.rename(ruta_base, copia)) stop("no se pudo apartar la base anterior")
      cat("   (respaldo con otro esquema: el archivo anterior quedo en ", basename(copia), ")\n", sep = "")
      write.csv(df, ruta_base, row.names = FALSE)
    }
    TRUE
  }, error = function(e) e)
  
  if (inherits(ok, "error")) return(rescatar_datos(df, ruta_base, conditionMessage(ok)))
  cat("   respaldo: ", nrow(df), " filas -> ", basename(ruta_base), "\n", sep = "")
  invisible(TRUE)
}


# =====================================================================
# SEGURIDAD (3/3): TODAS LAS BUSQUEDAS DE UN COMERCIO EN UNA CORRIDA
#   Cada busqueda es independiente: si una falla, se registra y sigue la
#   siguiente. Devuelve TODO lo estandarizado (en memoria); el respaldo en CSV
#   se escribe apenas termina cada busqueda.
# =====================================================================
ejecutar_busquedas <- function(cm, terminos = NULL, ts = marca_tiempo(), ruta_base = NULL,
                               pausa = 4, respaldar = TRUE) {
  force(ts)
  if (is.null(terminos)) terminos <- names(cm$catalogo)
  acumulado <- list()
  
  for (t in terminos) {
    cat("\n====================", toupper(cm$nombre), "|", t, "====================\n")
    
    cfg <- tryCatch(obtener_config_busqueda(cm, t), error = function(e) e)
    if (inherits(cfg, "error")) {
      cat("!! La busqueda '", t, "' fallo: ", conditionMessage(cfg), "\n", sep = "")
      registrar_log(cm$nombre, ts$id_ejecucion, t, estado = "error_busqueda", mensaje = conditionMessage(cfg))
      next
    }
    
    # 1) scraping
    datos <- tryCatch(scrapear_busqueda(cm, t, pausa = pausa, ts = ts), error = function(e) {
      cat("!! La busqueda '", t, "' fallo: ", conditionMessage(e), "\n", sep = "")
      registrar_log(cm$nombre, ts$id_ejecucion, cfg$termino, estado = "error_busqueda", mensaje = conditionMessage(e))
      NULL
    })
    if (is.null(datos) || nrow(datos) == 0) { cat("   (sin productos para guardar)\n"); next }
    
    # 2) estandarizacion al esquema final
    out <- tryCatch(
      datos %>%
        estandarizar_productos() %>%
        mutate(comercio = cm$nombre, busqueda = cfg$termino) %>%
        select(all_of(COLUMNAS_FINALES)) %>%
        tipificar_base(),
      error = function(e) {
        registrar_log(cm$nombre, ts$id_ejecucion, cfg$termino, estado = "error_estandarizacion", mensaje = conditionMessage(e))
        if (respaldar && !is.null(ruta_base)) {
          rescatar_datos(mutate(datos, busqueda = cfg$termino), ruta_base, paste("estandarizacion:", conditionMessage(e)))
        } else cat("!! estandarizacion fallo: ", conditionMessage(e), "\n", sep = "")
        NULL
      })
    if (is.null(out)) next
    
    # 3) respaldo (solo escribe) y 4) memoria
    if (respaldar && !is.null(ruta_base)) guardar_respaldo(out, ruta_base)
    acumulado[[length(acumulado) + 1]] <- out
  }
  
  # ---- resumen del comercio ----
  cat("\n\n==================== RESUMEN ", toupper(cm$nombre), " ====================\n", sep = "")
  log_cm <- LOG$filtros %>% filter(comercio == cm$nombre)
  if (nrow(log_cm) > 0) {
    print(as.data.frame(log_cm[, c("busqueda", "filtro", "estado", "intentos", "n_capturados", "n_tras_descarte")]),
          row.names = FALSE, right = FALSE)
  }
  
  todo <- if (length(acumulado) > 0) dplyr::bind_rows(acumulado) else tibble()
  if (nrow(todo) > 0) {
    cat("\n")
    print(as.data.frame(todo %>% group_by(busqueda) %>%
                          summarise(productos = n(), con_oferta_general = sum(oferta_general == 1L, na.rm = TRUE),
                                    sin_medida = sum(is.na(medida)), .groups = "drop")),
          row.names = FALSE)
  }
  
  problemas <- log_cm %>% filter(estado != "ok")
  if (nrow(problemas) > 0) {
    cat("\n!! REVISAR: ", nrow(problemas), " consulta(s) de ", cm$nombre,
        " no terminaron en 'ok' (ver estado y mensaje en el log)\n", sep = "")
  } else {
    cat("\nTodas las consultas de ", cm$nombre, " terminaron en 'ok'.\n", sep = "")
  }
  invisible(todo)
}

# lee los respaldos CSV (solo en MODO = "respaldo")
leer_respaldos <- function(dir_raw, comercios) {
  piezas <- list()
  for (cmn in comercios) {
    ruta <- file.path(dir_raw, paste0("resultado_seguro_", cmn, ".csv"))
    if (!file.exists(ruta)) { warning("No existe el respaldo de ", cmn, ": ", ruta, call. = FALSE); next }
    d <- read.csv(ruta, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE,
                  encoding = "UTF-8")
    if (!identical(names(d), COLUMNAS_FINALES)) {
      warning("El respaldo de ", cmn, " no tiene el esquema final (se omite): ", ruta, call. = FALSE)
      next
    }
    cat("  respaldo leido:", basename(ruta), "->", nrow(d), "filas\n")
    piezas[[cmn]] <- tipificar_base(d)
  }
  if (length(piezas) == 0) stop("No se pudo leer ningun respaldo en: ", dir_raw, call. = FALSE)
  dplyr::bind_rows(piezas)
}


# #####################################################################
# [3] COMERCIO: EXITO   (todo lo propio lleva prefijo ex_ / EX_)
#
#  Su tarjeta no trae marca ni presentacion como campos: la presentacion sale
#  del nombre y la marca es una heuristica (primer bloque en MAYUSCULAS).
#  Exito paginaba con el boton "Siguiente" (reemplaza la grilla; no acumula),
#  asi que se extraen los productos de CADA pagina antes de avanzar.
#  Clases con hash (productCard_xxx__M0677) cambian en cada despliegue: se usan
#  los atributos data-fs-*.
# #####################################################################
EX_BASE_URL      <- "https://www.exito.com"
EX_SEL_NODOS     <- "ul[data-fs-product-grid-list='true'] > li"
EX_SEL_LINKS     <- "ul[data-fs-product-grid-list='true'] > li a[data-testid='product-link']"
EX_SEL_BOTON_SIG <- "button[aria-label*='xima']:not([disabled])"   # aria-label="Próxima Pagina"

# ---------------------------------------------------------------------
# 3.1 EXTRACCION DE UN PRODUCTO (texto crudo, segun el contrato del nucleo)
#   precio_cop    = precio de lista (el tachado; si no hay descuento, el unico precio)
#   precio_oferta = precio final si hay tachado mayor; si no, NA
#   condicion_promocion = NA (la tarjeta de Exito no la muestra: ver nota al final)
# ---------------------------------------------------------------------
ex_extraer_producto <- function(nodo) {
  txt <- function(css) nodo %>% html_element(css) %>% html_text2()
  
  nombres <- txt("h3")
  
  # ("... (946 gr)", "... X1000ml", "... 562 Ml")
  presentacion <- nombres %>%
    str_extract("(?i)\\d+(?:[.,]\\d+)?\\s*(?:ml|gr|g|kg|lt|l|oz|und|unds|unidades)\\b")
  
  marca <- nombres %>%
    str_extract("\\b[A-ZÁÉÍÓÚÑ]{2,}(?:\\s+[A-ZÁÉÍÓÚÑ]{1,}\\b)*")
  
  precio_final_txt <- txt("p[data-fs-container-price-otros='true']")
  precio_lista_txt <- txt("p[class*='price-dashed']")
  hay_desc <- !is.na(precio_a_cop(precio_lista_txt)) && !is.na(precio_a_cop(precio_final_txt)) &&
    precio_a_cop(precio_lista_txt) > precio_a_cop(precio_final_txt)
  
  href <- nodo %>% html_element("a[data-testid='product-link']") %>% html_attr("href")
  url  <- if (is.na(href)) NA_character_ else url_absolute(href, EX_BASE_URL)
  
  # Exito no expone SKU en la tarjeta: se usa el id numerico del final de la URL
  # (".../crema-corporal-uv15-878339/p" -> 878339; ".../...-104294822-mp/p" -> 104294822)
  sku <- str_match(href, "-(\\d+)(?:-mp)?/p/?$")[, 2]
  if (is.na(sku)) sku <- str_remove_all(href, "^/|/p/?$")
  
  tibble(
    nombres = nombres,
    presentacion = presentacion,
    marca = marca,
    precio_cop = if (hay_desc) precio_lista_txt else precio_final_txt,
    precio_oferta = if (hay_desc) precio_final_txt else NA_character_,
    condicion_promocion = NA_character_,
    url = url,
    sku = sku,
    texto_original_anuncio = html_text2(nodo)
  )
}

# ---------------------------------------------------------------------
# 3.2 CATALOGO DE BUSQUEDAS - EXITO
#   cats = filtros de categoria tal como los arma Exito en la URL:
#          c("category-1" = "slug", ...). La URL lleva category-N=slug y
#          facets=category-1,category-2,...
#     category-1 = departamento (cuidado-personal)
#     category-2 = categoria    (cuidado-corporal)
#     category-3 = subcategoria (cremas-de-manos-y-cuerpo | aceites-corporales)
# ---------------------------------------------------------------------
EX_CAT_CREMAS  <- c("category-1" = "cuidado-personal",
                    "category-2" = "cuidado-corporal",
                    "category-3" = "cremas-de-manos-y-cuerpo")

EX_CAT_ACEITES <- c("category-1" = "cuidado-personal",
                    "category-2" = "cuidado-corporal",
                    "category-3" = "aceites-corporales")

# La subcategoria "manos y cuerpo" mezcla productos de manos y de bebes:
# se exige "corporal" y se descartan manos / bebes.
EX_PALABRAS_MANOS_BEBES <- c("mano", "manos", "baby", "bebe", "bebes")

EX_CATALOGO_BUSQUEDAS <- list(
  
  "crema hidratante corporal" = list(
    termino = "crema hidratante corporal",
    alias   = c("crema hidrante corporal"),
    filtros = list(
      list(etiqueta = "subcategoria: Cremas de manos y cuerpo",
           cats     = EX_CAT_CREMAS,
           regla    = list(requerir = "corporal",
                           excluir  = EX_PALABRAS_MANOS_BEBES))
    )
  ),
  
  "aceite corporal" = list(
    termino = "aceite corporal",
    filtros = list(
      list(etiqueta = "subcategoria: Aceites corporales",
           cats     = EX_CAT_ACEITES,
           regla    = list(requerir = "corporal",
                           excluir  = EX_PALABRAS_MANOS_BEBES))
    )
  ),
  
  # Solo cremas: se exige "crema" y "corporal", se descartan las de manos
  "exfoliante" = list(
    termino = "exfoliante",
    filtros = list(
      list(etiqueta = "subcategoria: Cremas de manos y cuerpo",
           cats     = EX_CAT_CREMAS,
           regla    = list(requerir = c("crema", "corporal"),
                           excluir  = c("mano", "manos")))
    )
  ),
  
  # Se toman TODOS los productos de ambas subcategorias (sin descarte):
  # cremas, aceites, mantequillas, productos de masaje, etc.
  "estrias" = list(
    termino = "estrías",
    filtros = list(
      list(etiqueta = "subcategoria: Cremas de manos y cuerpo",
           cats     = EX_CAT_CREMAS),
      list(etiqueta = "subcategoria: Aceites corporales",
           cats     = EX_CAT_ACEITES)
    )
  )
)

# ---------------------------------------------------------------------
# 3.3 URL DE BUSQUEDA
#    /s?q=<termino>&category-1=..&category-2=..&category-3=..
#       &facets=category-1%2Ccategory-2%2Ccategory-3&sort=score_desc&page=0
# ---------------------------------------------------------------------
# espacios como "+", tildes en UTF-8
ex_codificar_q <- function(x) {
  x <- utils::URLencode(enc2utf8(x), reserved = TRUE)
  gsub("%20", "+", x, fixed = TRUE)
}

ex_construir_url_busqueda <- function(termino_busqueda, cats = NULL, pagina = 0,
                                      orden = "score_desc") {
  q <- ex_codificar_q(paste(termino_busqueda, collapse = " "))
  
  partes <- paste0("q=", q)
  if (!is.null(cats) && length(cats) > 0) {
    partes <- c(partes,
                paste0(names(cats), "=", cats),
                paste0("facets=", paste(names(cats), collapse = "%2C")))
  }
  partes <- c(partes, paste0("sort=", orden), paste0("page=", pagina))
  
  paste0(EX_BASE_URL, "/s?", paste(partes, collapse = "&"))
}

ex_url_del_filtro <- function(cfg, f) ex_construir_url_busqueda(cfg$termino, f$cats)

# ---------------------------------------------------------------------
# 3.4 SCRAPING DE UNA URL
#   Exito re-renderiza la grilla al paginar y Chrome invalida los "node ids" que
#   usa rvest::LiveHTML (error -32000 "No node with given id"). Por eso NO se usa
#   html_elements(sesion, ...): se pide el HTML completo por JavaScript y se
#   parsea como documento estatico. El clic tambien es por JS.
# ---------------------------------------------------------------------
ex_html_actual <- function(sesion) {
  r <- tryCatch(
    sesion$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value,
    error = function(e) NULL)
  if (is.null(r) || !is.character(r) || !nzchar(r)) return(NULL)
  xml2::read_html(r)
}

# Huella de la pagina actual: primeros enlaces de producto. Cambia al paginar.
ex_firma_pagina <- function(sesion) {
  doc <- ex_html_actual(sesion)
  if (is.null(doc)) return("")
  h <- doc %>% html_elements(EX_SEL_LINKS) %>% html_attr("href")
  paste(head(h, 6), collapse = "|")
}

ex_esperar_productos <- function(sesion, max_seg = 25) {
  for (i in seq_len(max_seg)) {
    doc <- ex_html_actual(sesion)
    if (!is.null(doc) && length(html_elements(doc, EX_SEL_NODOS)) > 0) return(TRUE)
    Sys.sleep(1)
  }
  FALSE
}

# Clic por JS sobre el boton "Siguiente". Devuelve TRUE si habia boton activo.
ex_clic_siguiente <- function(sesion) {
  js <- "(function(){
    var b = document.querySelector(\"button[aria-label*='xima']:not([disabled])\");
    if (!b) return false;
    b.scrollIntoView({block: 'center'});
    b.click();
    return true;
  })()"
  r <- tryCatch(sesion$session$Runtime$evaluate(js)$result$value, error = function(e) FALSE)
  isTRUE(r)
}

# Devuelve TRUE si logro pasar a la siguiente pagina (la grilla cambio)
ex_ir_a_siguiente <- function(sesion, espera_max = 20) {
  antes <- ex_firma_pagina(sesion)
  if (!ex_clic_siguiente(sesion)) return(FALSE)     # sin boton activo = ultima pagina
  
  for (i in seq_len(espera_max)) {
    Sys.sleep(1)
    ahora <- ex_firma_pagina(sesion)
    if (nzchar(ahora) && ahora != antes) {
      Sys.sleep(1.5)                                # deja terminar de pintar la grilla
      return(TRUE)
    }
  }
  FALSE
}

ex_abrir_sesion <- function(url, intentos = 3) {
  for (i in seq_len(intentos)) {
    sesion <- tryCatch(rvest::read_html_live(url), error = function(e) {
      cat("   intento", i, "de", intentos, "fallo al abrir:", conditionMessage(e), "\n")
      NULL
    })
    if (!is.null(sesion)) return(sesion)
    Sys.sleep(3 * i)
  }
  NULL
}

# Recibe la URL completa (con filtros), no el termino
ex_scrapear_pagina_live <- function(url, pausa = 4, ts = marca_tiempo(), max_paginas = 60) {
  force(ts)                      # fija la fecha AL INICIO de la consulta
  sesion <- ex_abrir_sesion(url)
  if (is.null(sesion)) {
    warning("No se pudo abrir: ", url, call. = FALSE)
    return(tabla_productos_vacia())
  }
  on.exit(try(sesion$session$close(), silent = TRUE), add = TRUE)
  
  Sys.sleep(pausa)
  if (!ex_esperar_productos(sesion)) return(tabla_productos_vacia())
  
  acumulado <- list()
  for (pag in seq_len(max_paginas)) {
    doc <- ex_html_actual(sesion)
    if (is.null(doc)) break
    nodos <- doc %>% html_elements(EX_SEL_NODOS)
    if (length(nodos) == 0) break
    
    acumulado[[pag]] <- purrr::map_dfr(nodos, ex_extraer_producto)
    cat("  pagina", pag, "->", length(nodos), "productos\n")
    
    if (!ex_ir_a_siguiente(sesion)) break
  }
  
  if (length(acumulado) == 0) return(tabla_productos_vacia())
  
  dplyr::bind_rows(acumulado) %>%
    filter(!is.na(nombres)) %>%                     # <li> que no son productos
    distinct(sku, .keep_all = TRUE) %>%
    mutate(
      id_ejecucion   = ts$id_ejecucion,
      fecha_consulta = ts$fecha_consulta,
      .before = 1
    )
}
# NOTA Exito: la tarjeta no muestra la condicion del descuento, asi que todo
# descuento visible se toma como GENERAL (oferta_general = 1). Si descubres que
# alguno exige tarjeta/cuenta, el texto completo queda en texto_original_anuncio.


# #####################################################################
# [4] COMERCIO: FARMATODO   (todo lo propio lleva prefijo fa_ / FA_)
# #####################################################################

# ---------------------------------------------------------------------
# 4.1 EXTRACCION DE UN PRODUCTO (texto crudo, segun el contrato del nucleo)
#   precio_cop          = precio de lista (el tachado)
#   precio_oferta       = precio con el descuento de la 1ra banda
#   condicion_promocion = TODOS los textos de condicion de la tarjeta
#                         ("1ra. Compra App y Web", "Dcto. Cuidado Personal"...)
#   (el precio Prime y el precio por unidad del sitio no se usan: el Prime es
#    condicional y el precio por unidad se recalcula; siguen en texto_original_anuncio)
# ---------------------------------------------------------------------
fa_extraer_producto <- function(nodo) {
  nombres <- nodo %>% html_element("a.product-card-handler__detail-description") %>% html_text2()
  
  presentacion <- nodo %>% html_element("a.product-card-handler__detail-description") %>% html_text2() %>%
    str_extract("(?i)(?:(?:botella|pote|frasco|tubo|caja|barra|sobre|bolsa|dispensador)\\s+)?x\\s*\\d+(?:[.,]\\d+)?\\s*(?:ml|gr|g|kg|lt|l|oz|und|unds|unidades)\\b")
  
  marca <- nodo %>% html_element("p.product-card-handler__brand-value") %>% html_text2()
  
  precio_cop    <- nodo %>% html_element("span.product-all-price__normal") %>% html_text2()
  precio_oferta <- nodo %>% html_element("span.product-all-price__offer") %>% html_text2()
  
  condicion_promocion <- nodo %>% html_elements("span.product-offer-description__text-value") %>%
    html_text2() %>% unir_textos()
  
  url <- nodo %>% html_element("a.product-card-handler__detail-description") %>% html_attr("href") %>%
    url_absolute("https://www.farmatodo.com.co")
  
  sku <- nodo %>% html_element("button.app-button__icon-plus") %>% html_attr("data-cuf")
  
  texto_original_anuncio <- nodo %>% html_text2()
  
  tibble(
    nombres = nombres,
    presentacion = presentacion,
    marca = marca,
    precio_cop = precio_cop,
    precio_oferta = precio_oferta,
    condicion_promocion = condicion_promocion,
    url = url,
    sku = sku,
    texto_original_anuncio = texto_original_anuncio
  )
}

# ---------------------------------------------------------------------
# 4.2 CATALOGO DE BUSQUEDAS - FARMATODO
#     Cada filtro = una URL.  expr = filtro de categoria / subcategoria tal como
#     lo arma la pagina.  regla = descarte por nombre, SOLO para ese filtro
#     (las subcategorias).  Para agregar un producto basta con sumar una entrada.
# ---------------------------------------------------------------------
FA_CATALOGO_BUSQUEDAS <- list(
  
  "crema hidratante corporal" = list(
    termino = "crema hidratante corporal",
    alias   = c("crema hidrante corporal"),
    filtros = list(
      list(etiqueta = "categoria: Corporal",
           expr     = "categorie:'Corporal'"),
      list(etiqueta = "subcategoria: Cuidado corporal",
           expr     = "subCategory:'Cuidado corporal'",
           regla    = list(requerir = c("crema", "corporal")))
    )
  ),
  
  "aceite corporal" = list(
    termino = "aceite corporal",
    filtros = list(
      list(etiqueta = "categoria: Corporal",
           expr     = "categorie:'Corporal'"),
      list(etiqueta = "subcategoria: Cuidado corporal",
           expr     = "subCategory:'Cuidado corporal'",
           regla    = list(requerir = "aceite",
                           excluir  = c("locion", "gel", "crema", "solar")))
    )
  ),
  
  # una sola URL con ambos filtros combinados (como la arma la pagina)
  "exfoliante" = list(
    termino = "exfoliante",
    filtros = list(
      list(etiqueta = "categoria: Corporal o Cuidado de la piel + subcategoria: Cuidado corporal",
           expr     = "categorie:'Corporal' OR categorie:'Cuidado de la piel' AND subCategory:'Cuidado corporal'")
    )
  ),
  
  "estrias" = list(
    termino = "estrías",
    filtros = list(
      list(etiqueta = "categoria: Corporal",
           expr     = "categorie:'Corporal'"),
      list(etiqueta = "subcategoria: Cuidado corporal",
           expr     = "subCategory:'Cuidado corporal'",
           regla    = list(excluir = c("locion", "gel hidratante", "crema de sol", "solar"))),
      list(etiqueta = "subcategoria: Aceites corporales",
           expr     = "subCategory:'Aceites corporales'",
           regla    = list(excluir = c("locion", "gel hidratante", "crema de sol", "solar")))
    )
  )
)

# ---------------------------------------------------------------------
# 4.3 URL DE BUSQUEDA
# ---------------------------------------------------------------------
# Codificacion identica a la de la pagina: espacios %20, comillas %27,
# tildes en UTF-8 (estrías -> estr%C3%ADas) y ":" SIN codificar.
fa_codificar_url <- function(x) {
  x <- utils::URLencode(enc2utf8(x), reserved = TRUE)
  gsub("%3A", ":", x, fixed = TRUE)
}

fa_construir_url_busqueda <- function(termino_busqueda, expr_filtro = NULL) {
  termino_url <- fa_codificar_url(paste(termino_busqueda, collapse = " "))
  filtros <- if (is.null(expr_filtro)) "" else paste0(" AND  ", expr_filtro)
  paste0("https://www.farmatodo.com.co/buscar?product=", termino_url,
         "&departamento=Todos&filtros=", fa_codificar_url(filtros))
}

fa_url_del_filtro <- function(cfg, f) fa_construir_url_busqueda(cfg$termino, f$expr)

# ---------------------------------------------------------------------
# 4.4 SCRAPING DE UNA URL (Farmatodo no usa paginas: boton "Cargar mas")
# ---------------------------------------------------------------------
FA_SEL_NODOS        <- "div.cont-card-ftd.cont-group-view__row > div.card-ftd"
FA_SEL_BOTON_ACTIVO <- "#group-view-load-more:not([disabled])"

fa_clic_cargar_mas <- function(sesion) {
  ok <- tryCatch({ sesion$click("#group-view-load-more"); TRUE }, error = function(e) FALSE)
  if (!ok) {   # plan B: clic por JavaScript
    sesion$session$Runtime$evaluate(
      expression = "document.querySelector('#group-view-load-more').click()")
  }
  invisible(ok)
}

fa_cargar_todo <- function(sesion, max_clics = 60, pausa = 2) {
  for (i in 1:20) {                                   # esperar los primeros productos (hasta 20 s)
    if (length(html_elements(sesion, FA_SEL_NODOS)) > 0) break
    Sys.sleep(1)
  }
  sin_cambio <- 0
  for (i in seq_len(max_clics)) {
    antes <- length(html_elements(sesion, FA_SEL_NODOS))
    sesion$scroll_by(top = 5000); Sys.sleep(1)        # el boton esta al final de la lista
    
    if (length(html_elements(sesion, FA_SEL_BOTON_ACTIVO)) == 0) {
      Sys.sleep(pausa)                                # puede estar deshabilitado mientras carga
      if (length(html_elements(sesion, FA_SEL_BOTON_ACTIVO)) == 0) break
    }
    fa_clic_cargar_mas(sesion)
    Sys.sleep(pausa)
    
    despues <- length(html_elements(sesion, FA_SEL_NODOS))
    cat("clic", i, "-> productos:", despues, "\n")
    sin_cambio <- if (despues <= antes) sin_cambio + 1 else 0
    if (sin_cambio >= 2) break                        # el clic no esta cargando nada
  }
  invisible(sesion)
}

# Recibe la URL completa (con filtros), no el termino
fa_scrapear_pagina_live <- function(url, pausa = 4, ts = marca_tiempo()) {
  force(ts)                      # fija la fecha AL INICIO de la consulta
  sesion <- rvest::read_html_live(url)
  on.exit(try(sesion$session$close(), silent = TRUE), add = TRUE)
  
  Sys.sleep(pausa)
  fa_cargar_todo(sesion, pausa = 2)
  
  nodos <- sesion %>% html_elements(FA_SEL_NODOS)
  
  if (length(nodos) == 0) {
    return(tabla_productos_vacia())
  }
  
  purrr::map_dfr(nodos, fa_extraer_producto) %>%
    distinct(sku, .keep_all = TRUE) %>%
    mutate(
      id_ejecucion   = ts$id_ejecucion,
      fecha_consulta = ts$fecha_consulta,
      .before = 1
    )
}


# #####################################################################
# [5] COMERCIO: LA REBAJA   (todo lo propio lleva prefijo lr_ / LR_)
#
#  Sitio VTEX. Su paginacion es el boton "Mostrar mas" (acumula productos en
#  la misma pagina), igual que Farmatodo con "Cargar mas".
# #####################################################################

# ---------------------------------------------------------------------
# 5.1 EXTRACCION DE UN PRODUCTO (texto crudo, segun el contrato del nucleo)
#   precio_cop          = el "Antes" si hay descuento; si no, el unico precio
#   precio_oferta       = precio actual (solo cuando hay "Antes")
#   presentacion        = "FRASCO X 100 ML" (en La Rebaja viene en un elemento aparte)
#   condicion_promocion = texto del contenedor de condiciones de la etiqueta
#                         (en las tarjetas revisadas siempre estaba vacio; si algun
#                          dia trae algo como "con tarjeta", el nucleo lo clasifica)
# ---------------------------------------------------------------------
lr_extraer_producto <- function(nodo) {
  nombres      <- nodo %>% html_element("h3.vtex-product-summary-2-x-productNameContainer") %>% html_text2()
  presentacion <- nodo %>% html_element("div.copservir-larebaja-theme-9-x-PUM-content-wrapper > span") %>% html_text2()
  marca        <- nodo %>% html_element("span.vtex-store-components-3-x-productBrandName") %>% html_text2()
  
  precio_venta <- nodo %>% html_element("span.vtex-product-price-1-x-sellingPriceValue") %>% html_text2()
  precio_antes <- nodo %>% html_element("span.vtex-product-price-1-x-listPriceValue") %>% html_text2()
  precio_cop    <- coalesce(precio_antes, precio_venta)
  precio_oferta <- if_else(!is.na(precio_antes), precio_venta, NA_character_)
  
  condicion_promocion <- nodo %>% html_elements("div.copservir-larebaja-theme-9-x-flagsConditionsCont") %>%
    html_text2() %>% unir_textos()
  
  url <- nodo %>% html_element("a.vtex-product-summary-2-x-clearLink") %>% html_attr("href") %>%
    url_absolute("https://www.larebajavirtual.com")
  
  sku <- nodo %>% html_attr("data-af-product-id")
  
  texto_original_anuncio <- nodo %>% html_text2()
  
  tibble(
    nombres = nombres,
    presentacion = presentacion,
    marca = marca,
    precio_cop = precio_cop,
    precio_oferta = precio_oferta,
    condicion_promocion = condicion_promocion,
    url = url,
    sku = sku,
    texto_original_anuncio = texto_original_anuncio
  )
}

# ---------------------------------------------------------------------
# 5.2 CATALOGO DE BUSQUEDAS - LA REBAJA
#     categoria = slug de categoria de la pagina; NULL = busqueda SIN categoria.
#     Se usa "cuidado-corporal" y NO "corporal" (esta ultima trae manos,
#     arrugas, aceite de ducha, pomadas...).
# ---------------------------------------------------------------------
LR_CATALOGO_BUSQUEDAS <- list(
  
  "crema corporal" = list(
    termino = "crema corporal",
    alias   = c("crema  corporal", "crema cuerpo"),   # como aparece en tus URLs
    filtros = list(
      list(etiqueta  = "categoria: Cuidado corporal",
           categoria = "cuidado-corporal",
           regla     = list(requerir = c("crema", "corporal")))   # "gel hidratante" o "crema" sin "corporal" se descartan
    )
  ),
  
  "aceite corporal" = list(
    termino = "aceite corporal",
    filtros = list(
      list(etiqueta  = "categoria: Cuidado corporal",
           categoria = "cuidado-corporal")
    )
  ),
  
  "exfoliante" = list(
    termino = "exfoliante",
    filtros = list(
      list(etiqueta  = "categoria: Cuidado corporal",
           categoria = "cuidado-corporal")
    )
  ),
  
  # sin categoria: las estrias se presentan tanto en facial como en corporal
  "estrias" = list(
    termino = "estrías",
    filtros = list(
      list(etiqueta  = "sin categoria",
           categoria = NULL)
    )
  )
)

# ---------------------------------------------------------------------
# 5.3 URL DE BUSQUEDA  (formato de la pagina + filtro de categoria)
#      sin categoria : .../<termino>?_q=<termino>&map=ft&page=N
#      con categoria : .../<termino>?_q=<termino>&fuzzy=0&initialMap=ft&initialQuery=<termino>
#                      &map=category-2,ft&operator=and&query=/<categoria>/<termino>&searchState&page=N
#   (la paginacion real se hace con el boton "Mostrar mas"; `pagina` queda por compatibilidad)
# ---------------------------------------------------------------------
# tildes en UTF-8 (estrías -> estr%C3%ADas), espacios -> %20
lr_codificar_url <- function(x) utils::URLencode(enc2utf8(x), reserved = TRUE)

lr_construir_url_busqueda <- function(termino_busqueda, categoria = NULL, pagina = 1) {
  termino_url <- lr_codificar_url(paste(termino_busqueda, collapse = " "))
  base <- paste0("https://www.larebajavirtual.com/", termino_url, "?_q=", termino_url)
  
  if (is.null(categoria)) {
    paste0(base, "&map=ft&page=", pagina)
  } else {
    paste0(base, "&fuzzy=0&initialMap=ft&initialQuery=", termino_url,
           "&map=category-2,ft&operator=and&query=/", categoria, "/", termino_url,
           "&searchState&page=", pagina)
  }
}

lr_url_del_filtro <- function(cfg, f) lr_construir_url_busqueda(cfg$termino, f$categoria)

# ---------------------------------------------------------------------
# 5.4 SCRAPING DE UNA URL  (paginacion: boton "Mostrar mas")
# ---------------------------------------------------------------------
LR_SEL_NODOS     <- "#gallery-layout-container > div.vtex-search-result-3-x-galleryItem"
LR_SEL_BOTON_MAS <- "button.vtex-button.min-h-small"     # el de "Comprar" es min-h-regular

# el boton existe Y dice "Mostrar mas" (otros botones min-h-small no cuentan)
lr_hay_boton_mostrar_mas <- function(sesion) {
  b <- html_elements(sesion, LR_SEL_BOTON_MAS)
  length(b) > 0 && any(str_detect(html_text2(b), regex("^\\s*mostrar m[aá]s\\s*$", ignore_case = TRUE)))
}

# clic por JavaScript sobre el boton con ese selector Y ese texto
lr_clic_mostrar_mas <- function(sesion) {
  js <- paste0(
    "(function(){",
    "const b=[...document.querySelectorAll('", LR_SEL_BOTON_MAS, "')]",
    ".find(e=>/mostrar m(a|\\u00e1)s/i.test(e.innerText||''));",
    "if(!b) return false;",
    "b.scrollIntoView({block:'center'}); b.click(); return true;",
    "})()")
  r <- sesion$session$Runtime$evaluate(expression = js, returnByValue = TRUE)
  invisible(isTRUE(r$result$value))
}

lr_cargar_todo <- function(sesion, max_clics = 60, pausa = 2) {
  for (i in 1:20) {                                   # esperar los primeros productos (hasta 20 s)
    if (length(html_elements(sesion, LR_SEL_NODOS)) > 0) break
    Sys.sleep(1)
  }
  sin_cambio <- 0
  for (i in seq_len(max_clics)) {
    antes <- length(html_elements(sesion, LR_SEL_NODOS))
    sesion$scroll_by(top = 5000); Sys.sleep(1)        # el boton esta al final de la lista
    
    if (!lr_hay_boton_mostrar_mas(sesion)) {
      Sys.sleep(pausa)                                # puede desaparecer un momento mientras carga
      if (!lr_hay_boton_mostrar_mas(sesion)) break
    }
    lr_clic_mostrar_mas(sesion)
    Sys.sleep(pausa)
    
    despues <- length(html_elements(sesion, LR_SEL_NODOS))
    cat("clic", i, "-> productos:", despues, "\n")
    sin_cambio <- if (despues <= antes) sin_cambio + 1 else 0
    if (sin_cambio >= 2) break                        # el clic no esta cargando nada
  }
  invisible(sesion)
}

# Recibe la URL completa (con filtros), no el termino
lr_scrapear_pagina_live <- function(url, pausa = 4, ts = marca_tiempo()) {
  force(ts)                      # fija la fecha AL INICIO de la consulta
  sesion <- rvest::read_html_live(url)
  on.exit(try(sesion$session$close(), silent = TRUE), add = TRUE)
  
  Sys.sleep(pausa)
  lr_cargar_todo(sesion, pausa = 2)
  
  nodos <- sesion %>% html_elements(LR_SEL_NODOS)
  
  if (length(nodos) == 0) {
    return(tabla_productos_vacia())
  }
  
  purrr::map_dfr(nodos, lr_extraer_producto) %>%
    distinct(sku, .keep_all = TRUE) %>%
    mutate(
      id_ejecucion   = ts$id_ejecucion,
      fecha_consulta = ts$fecha_consulta,
      .before = 1
    )
}


# #####################################################################
# [6] REGISTRO DE COMERCIOS
#     Para un comercio nuevo: copiar un bloque [3], [4] o [5] (segun como
#     pagine el sitio), cambiar el prefijo y agregar aqui su ficha.
# #####################################################################
COMERCIOS <- list(
  exito = list(
    nombre          = "exito",
    catalogo        = EX_CATALOGO_BUSQUEDAS,
    url_del_filtro  = ex_url_del_filtro,
    scrapear_pagina = ex_scrapear_pagina_live
  ),
  farmatodo = list(
    nombre          = "farmatodo",
    catalogo        = FA_CATALOGO_BUSQUEDAS,
    url_del_filtro  = fa_url_del_filtro,
    scrapear_pagina = fa_scrapear_pagina_live
  ),
  larebaja = list(
    nombre          = "larebaja",
    catalogo        = LR_CATALOGO_BUSQUEDAS,
    url_del_filtro  = lr_url_del_filtro,
    scrapear_pagina = lr_scrapear_pagina_live
  )
)

# cada ficha debe cumplir el contrato del nucleo
for (.k in names(COMERCIOS)) {
  .cm <- COMERCIOS[[.k]]
  stopifnot(
    identical(.cm$nombre, .k),
    is.list(.cm$catalogo), length(.cm$catalogo) > 0,
    is.function(.cm$url_del_filtro), is.function(.cm$scrapear_pagina)
  )
}
.desconocidos <- setdiff(COMERCIOS_A_CORRER, names(COMERCIOS))
if (length(.desconocidos) > 0) {
  stop("Comercios sin ficha en COMERCIOS: ", paste(.desconocidos, collapse = ", "), call. = FALSE)
}
rm(.k, .cm, .desconocidos)


# #####################################################################
# [7] EJECUCION  (una sola corrida, de arriba abajo)
#     -> deja la base comun en memoria: `base_general`
# #####################################################################
ts_corrida <- marca_tiempo()     # un solo id_ejecucion / fecha_consulta para TODA la corrida
reiniciar_log()
dir.create(DIR_RAW, recursive = TRUE, showWarnings = FALSE)

if (MODO == "linea") {
  
  resultados <- list()
  for (nm in COMERCIOS_A_CORRER) {
    cm <- COMERCIOS[[nm]]
    ruta_respaldo <- file.path(DIR_RAW, paste0("resultado_seguro_", cm$nombre, ".csv"))
    
    # un comercio que falle por completo NO tumba a los demas
    resultados[[nm]] <- tryCatch(
      ejecutar_busquedas(cm, terminos = TERMINOS_A_BUSCAR, ts = ts_corrida,
                         ruta_base = ruta_respaldo, pausa = PAUSA_PAGINA, respaldar = RESPALDAR),
      error = function(e) {
        cat("!! El comercio '", nm, "' fallo por completo: ", conditionMessage(e), "\n", sep = "")
        registrar_log(nm, ts_corrida$id_ejecucion, NA_character_, estado = "error_comercio",
                      mensaje = conditionMessage(e))
        tibble()
      })
  }
  base_general <- dplyr::bind_rows(resultados)
  
  # log de consultas (un fallo aqui nunca debe tumbar la corrida)
  tryCatch({
    existe <- file.exists(RUTA_LOG)
    write.table(LOG$filtros, RUTA_LOG, sep = ",", row.names = FALSE,
                col.names = !existe, append = existe, qmethod = "double")
  }, error = function(e) cat("   (no se pudo escribir el log: ", conditionMessage(e), ")\n", sep = ""))
  
} else if (MODO == "respaldo") {
  
  cat("\nMODO respaldo: se lee la base desde los CSV de ", DIR_RAW, "\n", sep = "")
  base_general <- leer_respaldos(DIR_RAW, COMERCIOS_A_CORRER)
  
} else {
  stop("MODO debe ser \"linea\" o \"respaldo\"", call. = FALSE)
}

if (nrow(base_general) == 0) stop("No se obtuvo ningun dato.", call. = FALSE)
base_general <- tipificar_base(base_general)     # mismas columnas y tipos, venga de donde venga


# #####################################################################
# [8] CONTROL DE CALIDAD DE LA BASE COMUN
#     Detiene el proceso solo si el esquema no coincide; lo demas se informa.
# #####################################################################
control_calidad <- function(base) {
  cat("\n==================== CONTROL DE CALIDAD ====================\n")
  if (!identical(names(base), COLUMNAS_FINALES)) {
    stop("La base no tiene exactamente las COLUMNAS_FINALES, en el mismo orden.", call. = FALSE)
  }
  clases <- vapply(base, function(x) class(x)[1], character(1))
  mal <- names(clases)[clases != unname(TIPOS_COLUMNAS[COLUMNAS_FINALES])]
  if (length(mal) > 0) stop("Tipos distintos al esquema en: ", paste(mal, collapse = ", "), call. = FALSE)
  cat("Esquema: OK (", length(COLUMNAS_FINALES), " columnas, tipos correctos)\n", sep = "")
  
  resumen <- base %>%
    group_by(comercio, busqueda) %>%
    summarise(filas = n(), skus = n_distinct(sku),
              sin_precio = sum(is.na(precio_real_cop)),
              sin_medida = sum(is.na(medida)),
              con_oferta_general = sum(oferta_general == 1L, na.rm = TRUE),
              .groups = "drop")
  cat("\n")
  print(as.data.frame(resumen), row.names = FALSE)
  
  n_dup <- base %>% count(id_ejecucion, comercio, busqueda, sku) %>% filter(n > 1) %>% nrow()
  verificar <- function(titulo, n) {
    cat(sprintf("  %-62s %s\n", titulo, if (n == 0) "OK" else paste0("REVISAR (", n, ")")))
  }
  cat("\nVerificaciones:\n")
  verificar("SKU repetido dentro de (corrida, comercio, busqueda)", n_dup)
  verificar("filas sin sku o sin nombre", sum(is.na(base$sku) | is.na(base$nombres)))
  verificar("filas sin precio", sum(is.na(base$precio_real_cop) | is.na(base$precio_lista_cop)))
  verificar("precio <= 0", sum(base$precio_real_cop <= 0 | base$precio_lista_cop <= 0, na.rm = TRUE))
  verificar("precio real mayor que el de lista", sum(base$precio_real_cop > base$precio_lista_cop, na.rm = TRUE))
  verificar("oferta_general distinta de 0/1", sum(!(base$oferta_general %in% c(0L, 1L))))
  verificar("medida distinta de ml/g (con valor)", sum(!is.na(base$medida) & !(base$medida %in% c("ml", "g"))))
  verificar("cantidad_total no positiva", sum(base$cantidad_total <= 0, na.rm = TRUE))
  esperados <- COMERCIOS_A_CORRER
  verificar("comercios esperados sin ninguna fila", length(setdiff(esperados, unique(base$comercio))))
  invisible(resumen)
}

control <- control_calidad(base_general)
 
# tryCatch({
#   source(file.path(DIR_TALLER, "sqlite_scraper.R"))
#   con <- abrir_base_sqlite(file.path(DIR_SALIDA, "precios.sqlite"))
#   log_estado <- if (MODO == "linea") LOG$filtros else leer_log_csv(RUTA_LOG)
#   guardar_en_sqlite(con, base_general, log_estado)
#   DBI::dbDisconnect(con)
# }, error = function(e) cat("!! No se pudo guardar en SQLite:", conditionMessage(e), "\n"))

source(file.path(DIR_TALLER, "sqlite_scraper.R"), encoding = "UTF-8")
base_general <- marcar_comparables(base_general)   # agrega es_comparable, id_producto, producto_comparable

tryCatch({
  con <- abrir_base_sqlite(file.path(DIR_SALIDA, "precios.sqlite"))   # la crea si no existe
  log_estado <- if (MODO == "linea") LOG$filtros else leer_log_csv(RUTA_LOG)
  guardar_en_sqlite(con, base_general, log_estado)
  DBI::dbDisconnect(con)
}, error = function(e) cat("!! No se pudo guardar en SQLite:", conditionMessage(e), "\n"))

# #####################################################################
# [9] PRODUCTOS COMPARABLES
#     Busca en la base los SKU del catalogo [1b] y arma:
#       base_comparables : todas las observaciones de esos productos
#       precios          : el ultimo precio de cada producto en cada comercio
#       comparativa      : una fila por producto, una columna de precio por comercio
# #####################################################################
stopifnot(all(unique(catalogo_largo$comercio) %in% names(COMERCIOS)))

# ---------------------------------------------------------------------
# 9.1 BASE DE ORIGEN
#     Es la base de esta corrida (`base_general`). Si un producto del catalogo no
#     salio en esta corrida (la pagina no lo mostro) y COMPLETAR_CON_RESPALDOS es
#     TRUE, se agrega su observacion guardada en los respaldos CSV: solo de esos
#     SKU. Asi no se pierde un dato ya recolectado; su fecha queda en
#     `fecha_consulta`.
# ---------------------------------------------------------------------
completar_con_respaldos <- function(base, catalogo) {
  faltan <- catalogo %>%
    filter(comercio %in% COMERCIOS_A_CORRER) %>%
    anti_join(mutate(base, sku = trimws(sku)), by = c("comercio", "sku"))
  if (nrow(faltan) == 0) return(base)
  
  cat("\n", nrow(faltan), " producto(s) del catalogo no salieron en esta corrida: se buscan en los respaldos\n", sep = "")
  historico <- tryCatch(
    leer_respaldos(DIR_RAW, unique(faltan$comercio)),
    error = function(e) {
      cat("   (no se pudieron leer los respaldos: ", conditionMessage(e), ")\n", sep = "")
      NULL
    })
  if (is.null(historico)) return(base)
  
  historico <- historico %>%
    mutate(sku = trimws(sku)) %>%
    semi_join(faltan, by = c("comercio", "sku"))
  cat("   observaciones encontradas en los respaldos:", nrow(historico), "\n")
  bind_rows(base, historico)
}

base_origen <- if (MODO == "linea" && COMPLETAR_CON_RESPALDOS) {
  completar_con_respaldos(base_general, catalogo_largo)
} else {
  base_general
}

# ---------------------------------------------------------------------
# 9.2 BASE DE PRODUCTOS COMPARABLES
#     Filas cuyo (comercio, sku) esta en el catalogo, con el id y el nombre
#     unico del producto. Conserva todas las corridas.
# ---------------------------------------------------------------------

# extraer_comparables <- function(base, catalogo) {
#   base %>%
#     mutate(sku = trimws(sku)) %>%
#     inner_join(catalogo, by = c("comercio", "sku")) %>%
#     select(id_producto, producto_comparable, everything()) %>%
#     arrange(id_producto, comercio, id_ejecucion)
# }

extraer_comparables <- function(base, catalogo) {
  marcar_comparables(base) %>%
    filter(es_comparable == 1L) %>%
    select(id_producto, producto_comparable, everything()) %>%
    arrange(id_producto, comercio, id_ejecucion)
}

# ---------------------------------------------------------------------
# 9.3 PRECIOS DE LA ULTIMA FECHA
#     Por producto y comercio se toma la observacion mas reciente (id_ejecucion
#     mayor). El precio comparado es precio_real_cop, lo que paga cualquier
#     cliente; precio_lista_cop es el precio sin descuento. Si el producto salio
#     en varias busquedas de la misma corrida, se deja una sola fila.
# ---------------------------------------------------------------------
precios_ultima_fecha <- function(base_comparables) {
  base_comparables %>%
    filter(!is.na(precio_real_cop)) %>%
    group_by(id_producto, producto_comparable, comercio) %>%
    filter(id_ejecucion == max(id_ejecucion)) %>%
    arrange(precio_real_cop, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    select(id_producto, producto_comparable, comercio, sku, id_ejecucion, fecha_consulta,
           precio_lista_cop, precio_real_cop, oferta_general, url) %>%
    arrange(id_producto, comercio)
}

# ---------------------------------------------------------------------
# 9.4 TABLA COMPARATIVA
#     Una fila por producto del catalogo y una columna de precio por comercio
#     (NA si ese comercio no tiene precio del producto).
#     misma_corrida = FALSE avisa que los precios comparados no son de la misma
#     corrida (alguno viene de un respaldo anterior).
# ---------------------------------------------------------------------
tabla_comparativa <- function(precios, comercios = COMERCIOS_A_CORRER) {
  ancho <- CATALOGO_COMPARABLES %>%
    select(id_producto, producto_comparable) %>%
    left_join(
      precios %>%
        select(id_producto, comercio, precio_real_cop) %>%
        tidyr::pivot_wider(names_from = comercio, values_from = precio_real_cop),
      by = "id_producto")
  for (cm in setdiff(comercios, names(ancho))) ancho[[cm]] <- NA_integer_
  ancho <- ancho[, c("id_producto", "producto_comparable", comercios)]
  p <- as.matrix(ancho[, comercios])
  
  resumen_precios <- precios %>%
    group_by(id_producto) %>%
    summarise(alguna_oferta_general = any(oferta_general == 1L, na.rm = TRUE),
              misma_corrida = dplyr::n_distinct(id_ejecucion) == 1,
              .groups = "drop")
  
  ancho %>%
    mutate(n_comercios = rowSums(!is.na(p)),
           en_todos_los_comercios = n_comercios == length(comercios),
           precio_min = do.call(pmin, c(unname(as.list(ancho[comercios])), na.rm = TRUE)),
           precio_max = do.call(pmax, c(unname(as.list(ancho[comercios])), na.rm = TRUE)),
           comercio_mas_barato = apply(p, 1, function(x)
             if (all(is.na(x))) NA_character_
             else paste(comercios[which(x == min(x, na.rm = TRUE))], collapse = " = ")),
           brecha_pct = round(100 * (precio_max - precio_min) / precio_min, 1)) %>%
    left_join(resumen_precios, by = "id_producto") %>%
    mutate(alguna_oferta_general = dplyr::coalesce(alguna_oferta_general, FALSE))
}

# ---------------------------------------------------------------------
# 9.5 EJECUCION
# ---------------------------------------------------------------------
base_comparables <- extraer_comparables(base_origen, catalogo_largo)
base_comparables <- base_comparables %>% group_by(sku) %>%
  slice_max(order_by = id_ejecucion, n = 1, with_ties = FALSE) %>%
  ungroup()

precios          <- precios_ultima_fecha(base_comparables)
comparativa      <- tabla_comparativa(precios)

# productos del catalogo sin ningun precio (ni en esta corrida ni en los respaldos)
faltantes <- catalogo_largo %>%
  filter(comercio %in% COMERCIOS_A_CORRER) %>%
  anti_join(precios, by = c("id_producto", "comercio"))
if (nrow(faltantes) > 0) {
  cat("\nProductos del catalogo SIN precio en la base (revisa el SKU):\n")
  print(faltantes, n = Inf, width = Inf)
}
if (any(comparativa$misma_corrida %in% FALSE)) {
  cat("\nAviso: hay productos cuyos precios vienen de corridas distintas (misma_corrida = FALSE).\n")
}

cat("\n== Comparativa (ultimo precio de cada producto por comercio) ==\n")
print(comparativa, n = Inf, width = Inf)

# copias de las salidas (solo se escriben; el analisis sigue en memoria)
dir.create(DIR_SALIDA, showWarnings = FALSE, recursive = TRUE)
old <- options(scipen = 999)
# write.csv(base_comparables, file.path(DIR_SALIDA, "base_comparables.csv"),    row.names = FALSE)
write.csv(precios,          file.path(DIR_SALIDA, "precios_ultima_fecha.csv"), row.names = FALSE)
write.csv(comparativa,      file.path(DIR_SALIDA, "comparativa_precios.csv"),  row.names = FALSE)
options(old)
rm(old)

