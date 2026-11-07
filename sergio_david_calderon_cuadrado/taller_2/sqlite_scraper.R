# =====================================================================
#  BASE SQLITE PARA EL SCRAPER  (Exito + Farmatodo + La Rebaja)
#
#  USO: pegar en el scraper, DESPUES de [8] CONTROL DE CALIDAD y ANTES de [9]:
#
#      source(file.path(DIR_TALLER, "sqlite_scraper.R"), encoding = "UTF-8")
#      base_general <- marcar_comparables(base_general)   # columnas es_comparable / id_producto
#      tryCatch({
#        con <- abrir_base_sqlite(file.path(DIR_SALIDA, "precios.sqlite"))  # la crea si no existe
#        log_estado <- if (MODO == "linea") LOG$filtros else leer_log_csv(RUTA_LOG)
#        guardar_en_sqlite(con, base_general, log_estado)
#        DBI::dbDisconnect(con)
#      }, error = function(e) cat("!! No se pudo guardar en SQLite:", conditionMessage(e), "\n"))
#
#  El archivo .sqlite NO tiene que existir antes: abrir_base_sqlite() lo crea
#  (con todas sus tablas) la primera vez y solo lo abre las siguientes.
#
#  Requiere que el scraper ya este cargado (usa CATALOGO_COMPARABLES,
#  catalogo_largo, estandarizar_contenido, normalizar_texto, tipificar_base).
#
#  TABLAS
#    comercios      1 fila por comercio
#    productos      los productos COMPARABLES (id comun = id_producto del catalogo)
#    anuncios       cada producto tal como aparece en un comercio (UNIQUE comercio+sku)
#                   id_producto NULL  => pertenece al resto de la categoria
#    ejecuciones    1 fila por (corrida, comercio) con n_registros y estado
#    observaciones  1 fila por precio y momento (UNIQUE anuncio + fecha_hora_consulta)
#  VISTAS
#    v_observaciones       la base general completa en SQLite (con es_comparable) + precio por ml/g
#    v_variacion_precio    precio actual vs anterior por anuncio
# =====================================================================

for (.p in c("DBI", "RSQLite")) {
  if (!requireNamespace(.p, quietly = TRUE)) install.packages(.p)
}
rm(.p)


# #####################################################################
# [A] ESQUEMA
# #####################################################################
SCHEMA_SQL <- "
CREATE TABLE IF NOT EXISTS comercios (
  id_comercio  INTEGER PRIMARY KEY,
  codigo       TEXT NOT NULL UNIQUE,      -- clave del scraper: exito | farmatodo | larebaja
  nombre       TEXT NOT NULL,             -- nombre para mostrar
  sitio_web    TEXT
);

CREATE TABLE IF NOT EXISTS productos (
  id_producto      INTEGER PRIMARY KEY,   -- = id_producto de CATALOGO_COMPARABLES
  nombre           TEXT NOT NULL UNIQUE,
  linea            TEXT,                  -- nombre sin el contenido: 'Lubriderm humectacion diaria'
  marca_std        TEXT NOT NULL,
  subcategoria_std TEXT NOT NULL,
  presentacion     TEXT NOT NULL,         -- legible: '400 ml', '3 x 200 ml'
  medida           TEXT NOT NULL CHECK (medida IN ('ml','g')),
  cantidad_unidad  REAL NOT NULL CHECK (cantidad_unidad > 0),
  unidades         REAL NOT NULL DEFAULT 1 CHECK (unidades > 0),
  cantidad_total   REAL NOT NULL CHECK (cantidad_total > 0)
);

CREATE TABLE IF NOT EXISTS anuncios (
  id_anuncio            INTEGER PRIMARY KEY,
  id_comercio           INTEGER NOT NULL REFERENCES comercios (id_comercio),
  sku                   TEXT NOT NULL,    -- 'producto del comercio'
  id_producto           INTEGER REFERENCES productos (id_producto),   -- NULL = no comparable
  nombre_original       TEXT NOT NULL,    -- nombre tal como se publico
  presentacion_original TEXT,             -- presentacion tal como se publico (puede ser NULL)
  marca_original        TEXT,
  marca_std             TEXT,
  subcategoria_std      TEXT,
  envase                TEXT,
  medida                TEXT CHECK (medida IN ('ml','g')),
  cantidad_unidad       REAL CHECK (cantidad_unidad > 0),
  unidades              REAL CHECK (unidades > 0),
  cantidad_total        REAL CHECK (cantidad_total > 0),
  url_producto          TEXT,
  texto_original        TEXT,             -- texto de la tarjeta la PRIMERA vez que se vio, sin limpieza
  primera_consulta      TEXT NOT NULL,
  ultima_consulta       TEXT NOT NULL,
  UNIQUE (id_comercio, sku),
  UNIQUE (id_anuncio, id_comercio)        -- permite la FK compuesta de observaciones
);

-- un producto comparable tiene como maximo UN anuncio por comercio
CREATE UNIQUE INDEX IF NOT EXISTS ux_anuncio_producto_comercio
  ON anuncios (id_comercio, id_producto) WHERE id_producto IS NOT NULL;
CREATE INDEX IF NOT EXISTS ix_anuncios_producto ON anuncios (id_producto);

CREATE TABLE IF NOT EXISTS ejecuciones (
  id_ejecucion      INTEGER PRIMARY KEY,
  codigo_corrida    TEXT NOT NULL,        -- id_ejecucion del scraper (AAAAMMDDhhmmss)
  id_comercio       INTEGER NOT NULL REFERENCES comercios (id_comercio),
  fecha_hora_inicio TEXT NOT NULL
    CHECK (fecha_hora_inicio GLOB '[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9][+-][0-9][0-9]:[0-9][0-9]'),
  n_registros       INTEGER NOT NULL DEFAULT 0 CHECK (n_registros >= 0),
  estado            TEXT NOT NULL CHECK (estado IN ('completa','parcial','fallida')),
  mensaje           TEXT,
  UNIQUE (codigo_corrida, id_comercio),
  UNIQUE (id_ejecucion, id_comercio)      -- permite la FK compuesta de observaciones
);

CREATE TABLE IF NOT EXISTS observaciones (
  id_observacion         INTEGER PRIMARY KEY,
  id_anuncio             INTEGER NOT NULL,
  id_comercio            INTEGER NOT NULL,
  id_ejecucion           INTEGER NOT NULL,
  fecha_hora_consulta    TEXT NOT NULL    -- ISO 8601 con zona: 2026-10-15T09:30:00-05:00
    CHECK (fecha_hora_consulta GLOB '[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9][+-][0-9][0-9]:[0-9][0-9]'),
  precio_cop             INTEGER NOT NULL CHECK (precio_cop > 0),        -- lo que paga cualquier cliente
  precio_lista_cop       INTEGER NOT NULL CHECK (precio_lista_cop > 0),  -- precio sin descuento
  es_promocion           INTEGER NOT NULL CHECK (es_promocion IN (0,1)), -- 1 = precio_cop incluye descuento general
  url_extraccion         TEXT NOT NULL,   -- pagina de donde salio el precio
  texto_original_anuncio TEXT,            -- texto de la tarjeta en ESTE momento (trae precio y promo), sin limpieza
  busqueda               TEXT,            -- informativo: primera busqueda en que salio
  UNIQUE (id_anuncio, fecha_hora_consulta),
  CHECK (precio_cop <= precio_lista_cop),
  CHECK ((precio_cop < precio_lista_cop) = es_promocion),
  FOREIGN KEY (id_anuncio, id_comercio)   REFERENCES anuncios (id_anuncio, id_comercio),
  FOREIGN KEY (id_ejecucion, id_comercio) REFERENCES ejecuciones (id_ejecucion, id_comercio)
);

CREATE INDEX IF NOT EXISTS ix_obs_ejecucion ON observaciones (id_ejecucion);

DROP VIEW IF EXISTS v_observaciones;
CREATE VIEW v_observaciones AS
SELECT o.id_observacion,
       c.nombre                                   AS comercio,
       a.id_producto,
       p.nombre                                   AS producto,
       p.linea                                    AS linea,
       CASE WHEN a.id_producto IS NULL THEN 0 ELSE 1 END AS es_comparable,
       a.sku,
       a.nombre_original,
       a.marca_std,
       a.subcategoria_std,
       a.medida, a.cantidad_unidad, a.unidades, a.cantidad_total,
       o.precio_cop,
       o.precio_lista_cop,
       o.es_promocion,
       ROUND(o.precio_cop * 1.0 / a.cantidad_total, 2) AS precio_por_medida,   -- COP por ml o g
       o.fecha_hora_consulta,
       e.codigo_corrida,
       e.estado                                   AS estado_ejecucion,
       o.url_extraccion,
       a.url_producto,
       o.texto_original_anuncio
FROM observaciones o
JOIN anuncios    a ON a.id_anuncio   = o.id_anuncio
JOIN comercios   c ON c.id_comercio  = o.id_comercio
JOIN ejecuciones e ON e.id_ejecucion = o.id_ejecucion
LEFT JOIN productos p ON p.id_producto = a.id_producto;

DROP VIEW IF EXISTS v_variacion_precio;
CREATE VIEW v_variacion_precio AS
WITH ord AS (
  SELECT o.*, ROW_NUMBER() OVER (PARTITION BY o.id_anuncio
                                 ORDER BY o.fecha_hora_consulta DESC) AS rn
  FROM observaciones o
)
SELECT c.nombre AS comercio,
       a.id_producto,
       p.nombre AS producto,
       p.linea  AS linea,
       a.sku,
       a.nombre_original,
       act.fecha_hora_consulta AS fecha_actual,
       act.precio_cop          AS precio_actual,
       ROUND(act.precio_cop * 1.0 / a.cantidad_total, 2) AS precio_por_medida_actual,
       ant.fecha_hora_consulta AS fecha_anterior,
       ant.precio_cop          AS precio_anterior,
       act.precio_cop - ant.precio_cop AS variacion_cop,
       ROUND(100.0 * (act.precio_cop - ant.precio_cop) / ant.precio_cop, 1) AS variacion_pct
FROM ord act
JOIN anuncios  a ON a.id_anuncio  = act.id_anuncio
JOIN comercios c ON c.id_comercio = act.id_comercio
LEFT JOIN productos p ON p.id_producto = a.id_producto
LEFT JOIN ord ant ON ant.id_anuncio = act.id_anuncio AND ant.rn = 2
WHERE act.rn = 1;
"

# Nombres para mostrar. Un comercio nuevo sin entrada aqui se guarda con su codigo.
NOMBRES_COMERCIO <- c(exito = "\u00c9xito", farmatodo = "Farmatodo", larebaja = "La Rebaja")
SITIOS_COMERCIO  <- c(exito = "https://www.exito.com",
                      farmatodo = "https://www.farmatodo.com.co",
                      larebaja = "https://www.larebajavirtual.com")

# Subcategoria de los 9 productos del catalogo. Si algun dia hay otra, ajustar aqui.
SUBCATEGORIA_COMPARABLES <- "crema_corporal"

# Alias de marca -> marca estandar (en MAYUSCULAS sin tildes). Ampliar segun aparezcan.
MARCAS_ALIAS <- c("NIVEA SUN" = "NIVEA", "LUBRIDERM FPS" = "LUBRIDERM")

RX_ISO8601 <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}[+-]\\d{2}:\\d{2}$"


# #####################################################################
# [A2] COLUMNA DE COMPARABLES EN LA BASE GENERAL
#      es_comparable        1 = el (comercio, sku) esta en CATALOGO_COMPARABLES [1b]; 0 = resto
#      id_producto          id comun del producto (NA si no es comparable)
#      producto_comparable  nombre unico del producto comparable (NA si no lo es)
#    Idempotente: se puede llamar sobre una base que ya las trae.
# #####################################################################
marcar_comparables <- function(base) {
  cat_sku <- catalogo_largo %>%
    dplyr::transmute(comercio, .sku_limpio = trimws(sku),
                     id_producto = as.integer(id_producto), producto_comparable)
  base %>%
    dplyr::select(-dplyr::any_of(c("es_comparable", "id_producto", "producto_comparable"))) %>%
    dplyr::mutate(.sku_limpio = trimws(sku)) %>%
    dplyr::left_join(cat_sku, by = c("comercio", ".sku_limpio")) %>%
    dplyr::mutate(es_comparable = as.integer(!is.na(id_producto))) %>%
    dplyr::select(-.sku_limpio) %>%
    dplyr::relocate(es_comparable, id_producto, producto_comparable, .after = busqueda)
}


# #####################################################################
# [B] ESTANDARIZACION DE MARCA Y SUBCATEGORIA
# #####################################################################
estandarizar_marca <- function(x) {
  m <- toupper(normalizar_texto(x))
  m <- trimws(gsub("[^A-Z0-9 ]", "", m))
  m[!nzchar(m)] <- NA_character_
  alias <- m %in% names(MARCAS_ALIAS)
  m[alias] <- unname(MARCAS_ALIAS[m[alias]])
  m
}

# Punto de partida: reglas por palabra sobre el nombre. Revisar con
#   SELECT subcategoria_std, COUNT(*) FROM anuncios GROUP BY 1;
estandarizar_subcategoria <- function(nombres) {
  n <- normalizar_texto(nombres)
  dplyr::case_when(
    grepl("\\b(exfoliante|scrub)\\b", n)                                      ~ "exfoliante_corporal",
    grepl("\\b(crema|cremas|locion|leche|mantequilla|emulsion|hidratante)\\b", n) ~ "crema_corporal",
    grepl("\\baceite", n)                                                     ~ "aceite_corporal",
    TRUE                                                                      ~ "otros_corporal"
  )
}


# #####################################################################
# [C] UTILIDADES
# #####################################################################
`%||%` <- function(a, b) if (is.null(a)) b else a

# \"20261015093000\" -> \"2026-10-15T09:30:00-05:00\"
iso_desde_codigo <- function(codigo) {
  t <- as.POSIXct(codigo, format = "%Y%m%d%H%M%S", tz = "America/Bogota")
  f <- format(t, "%Y-%m-%dT%H:%M:%S%z", tz = "America/Bogota")
  sub("(\\d{2})(\\d{2})$", "\\1:\\2", f)
}

leer_log_csv <- function(ruta) {
  if (!file.exists(ruta)) return(NULL)
  tryCatch(read.csv(ruta, stringsAsFactors = FALSE, colClasses = "character"),
           error = function(e) NULL)
}

# Estado de una (corrida, comercio):
#   fallida  : 0 registros, o el comercio fallo por completo
#   parcial  : alguna consulta no termino en 'ok', faltan productos del catalogo,
#              o no hay log que confirme que la corrida fue completa
#   completa : todas las consultas 'ok' y estan todos los SKU del catalogo
calcular_estado <- function(n, log_cm, n_faltan_catalogo) {
  errores <- log_cm$estado[log_cm$estado != "ok"]
  if (n == 0 || any(log_cm$estado == "error_comercio")) {
    return(list(estado = "fallida",
                mensaje = (log_cm$mensaje[!is.na(log_cm$mensaje) & nzchar(log_cm$mensaje)])[1] %||%
                  "sin registros"))
  }
  partes <- character(0)
  if (nrow(log_cm) == 0)       partes <- c(partes, "sin log de consultas para esta corrida")
  if (length(errores) > 0)     partes <- c(partes, sprintf("%d consulta(s) sin 'ok'", length(errores)))
  if (n_faltan_catalogo > 0)   partes <- c(partes, sprintf("%d producto(s) del catalogo sin precio", n_faltan_catalogo))
  if (length(partes) > 0) return(list(estado = "parcial", mensaje = paste(partes, collapse = "; ")))
  list(estado = "completa", mensaje = NA_character_)
}


# #####################################################################
# [D] APERTURA DE LA BASE (crea el esquema y siembra comercios / productos)
# #####################################################################
# Linea de producto = nombre del producto SIN el contenido final.
#   "Lubriderm humectacion diaria 200 ml" -> "Lubriderm humectacion diaria"
#   "Nivea hidratacion express 1000 ml"   -> "Nivea hidratacion express"
linea_desde_nombre <- function(nombre) {
  l <- sub("(?i)\\s*\\(?\\d+(?:[.,]\\d+)?\\s*(?:ml|cc|l|lt|g|gr|kg)\\)?\\s*$", "", nombre, perl = TRUE)
  trimws(l)
}

productos_desde_catalogo <- function() {
  p <- CATALOGO_COMPARABLES %>%
    dplyr::transmute(id_producto = as.integer(id_producto),
                     nombre = producto_comparable,
                     nombres = producto_comparable,       # estandarizar_contenido() lee `nombres`
                     presentacion = NA_character_,
                     precio_real_cop = NA_integer_) %>%
    estandarizar_contenido()

  sin_medida <- p$nombre[is.na(p$medida) | is.na(p$cantidad_total)]
  if (length(sin_medida) > 0) {
    stop("No se pudo leer el contenido (ml/g) de: ", paste(sin_medida, collapse = " | "), call. = FALSE)
  }

  num <- function(x) format(x, trim = TRUE, scientific = FALSE)
  p %>% dplyr::transmute(
    id_producto, nombre,
    linea            = linea_desde_nombre(nombre),
    marca_std        = estandarizar_marca(stringr::word(nombre, 1)),
    subcategoria_std = SUBCATEGORIA_COMPARABLES,
    presentacion     = ifelse(unidades > 1,
                              paste0(num(unidades), " x ", num(cantidad_unidad), " ", medida),
                              paste0(num(cantidad_unidad), " ", medida)),
    medida, cantidad_unidad, unidades, cantidad_total)
}

asegurar_comercios <- function(con, codigos) {
  codigos <- unique(codigos[!is.na(codigos)])
  if (length(codigos) == 0) return(invisible(NULL))
  nombre <- ifelse(codigos %in% names(NOMBRES_COMERCIO), NOMBRES_COMERCIO[codigos], codigos)
  sitio  <- ifelse(codigos %in% names(SITIOS_COMERCIO),  SITIOS_COMERCIO[codigos],  NA_character_)
  DBI::dbExecute(con,
    "INSERT INTO comercios (codigo, nombre, sitio_web) VALUES (?, ?, ?)
     ON CONFLICT (codigo) DO NOTHING",
    params = list(codigos, unname(nombre), unname(sitio)))
  invisible(NULL)
}

sincronizar_productos <- function(con) {
  p <- productos_desde_catalogo()
  DBI::dbExecute(con,
    "INSERT INTO productos (id_producto, nombre, linea, marca_std, subcategoria_std, presentacion,
                            medida, cantidad_unidad, unidades, cantidad_total)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
     ON CONFLICT (id_producto) DO UPDATE SET
       nombre = excluded.nombre, linea = excluded.linea, marca_std = excluded.marca_std,
       subcategoria_std = excluded.subcategoria_std, presentacion = excluded.presentacion,
       medida = excluded.medida, cantidad_unidad = excluded.cantidad_unidad,
       unidades = excluded.unidades, cantidad_total = excluded.cantidad_total",
    params = unname(as.list(p)))
  invisible(p)
}

# Una base creada con una version anterior del esquema no se actualiza sola
# (CREATE TABLE IF NOT EXISTS no agrega columnas). Aqui se agregan las que falten.
migrar_esquema <- function(con) {
  if (DBI::dbExistsTable(con, "productos") &&
      !"linea" %in% DBI::dbListFields(con, "productos")) {
    DBI::dbExecute(con, "ALTER TABLE productos ADD COLUMN linea TEXT")
    cat("   (migracion: se agrego la columna productos.linea)\n")
  }
  invisible(NULL)
}

# TRUE si la base existe y ya tiene el esquema vigente (la vista expone `linea`).
base_sqlite_vigente <- function(ruta) {
  if (!file.exists(ruta)) return(FALSE)
  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), ruta, flags = RSQLite::SQLITE_RO)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    "linea" %in% DBI::dbListFields(con, "v_observaciones")
  }, error = function(e) FALSE)
}

abrir_base_sqlite <- function(ruta) {
  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ruta)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")      # SQLite no las exige por defecto
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  migrar_esquema(con)                                  # antes de crear tablas/vistas

  sentencias <- strsplit(SCHEMA_SQL, ";[[:space:]]*\n")[[1]]
  for (s in sentencias) {
    solo_codigo <- trimws(gsub("--[^\n]*", "", s))
    if (nzchar(solo_codigo)) DBI::dbExecute(con, s)
  }
  asegurar_comercios(con, names(NOMBRES_COMERCIO))
  sincronizar_productos(con)
  cat("Base SQLite lista:", ruta, "\n")
  con
}


# #####################################################################
# [E] GUARDADO DE UNA CORRIDA (o de varias, al migrar respaldos)
#     Idempotente: correrlo dos veces con los mismos datos no duplica nada.
#     Todo ocurre en UNA transaccion: o entra todo o no entra nada.
# #####################################################################
guardar_en_sqlite <- function(con, base, log = NULL) {
  base <- tibble::as_tibble(base)
  if (!"url_extraccion" %in% names(base)) {
    # el scraper aun no guarda la URL de la pagina de busqueda: se usa la del producto
    base$url_extraccion <- base$url
  }

  # ---- 1. depuracion previa (lo que violaria el esquema se informa, no se oculta) ----
  base <- base %>%
    dplyr::mutate(sku = trimws(sku)) %>%
    dplyr::arrange(fecha_consulta)
  valida <- !is.na(base$sku) & nzchar(base$sku) & !is.na(base$nombres) &
    !is.na(base$precio_real_cop) & !is.na(base$precio_lista_cop) &
    base$precio_real_cop > 0 & base$precio_lista_cop > 0 &
    grepl(RX_ISO8601, base$fecha_consulta %||% "") & !is.na(base$url_extraccion)
  if (any(!valida)) {
    cat("!! ", sum(!valida), " fila(s) NO se guardan (sin sku, nombre, precio, fecha ISO 8601 o url):\n", sep = "")
    print(utils::head(base[!valida, c("comercio", "sku", "nombres", "precio_real_cop", "fecha_consulta")], 10))
  }
  descartadas <- base[!valida, ]
  base <- base[valida, ]

  # ---- 2. comparables: columna es_comparable / id_producto de la base general ----
  base <- marcar_comparables(base)
  cat(sprintf("Comparables en esta carga: %d de %d fila(s) validas\n", sum(base$es_comparable), nrow(base)))

  runs <- dplyr::bind_rows(
    dplyr::distinct(base, codigo_corrida = id_ejecucion, comercio),
    if (!is.null(log) && nrow(log) > 0) {
      dplyr::distinct(log[!is.na(log$comercio) & !is.na(log$id_ejecucion), ],
                      codigo_corrida = id_ejecucion, comercio)
    })
  runs <- dplyr::distinct(runs)

  res <- DBI::dbWithTransaction(con, {

    asegurar_comercios(con, c(runs$comercio, base$comercio))
    cmap <- DBI::dbGetQuery(con, "SELECT id_comercio, codigo AS comercio FROM comercios")

    # ---- 3. ejecuciones (estado y n_registros se completan al final) ----
    runs <- dplyr::left_join(runs, cmap, by = "comercio")
    runs$fecha_hora_inicio <- iso_desde_codigo(runs$codigo_corrida)
    DBI::dbExecute(con,
      "INSERT INTO ejecuciones (codigo_corrida, id_comercio, fecha_hora_inicio, estado)
       VALUES (?, ?, ?, 'fallida')
       ON CONFLICT (codigo_corrida, id_comercio) DO NOTHING",
      params = list(runs$codigo_corrida, runs$id_comercio, runs$fecha_hora_inicio))
    emap <- DBI::dbGetQuery(con,
      "SELECT id_ejecucion, codigo_corrida, id_comercio FROM ejecuciones")

    # ---- 4. anuncios: atributos = ultima vez visto; texto_original = primera vez ----
    anu <- base %>%
      dplyr::group_by(comercio, sku) %>%
      dplyr::summarise(
        nombre_original       = dplyr::last(nombres),
        presentacion_original = dplyr::last(presentacion),
        marca_original        = dplyr::last(marca),
        envase                = dplyr::last(envase),
        medida                = dplyr::last(medida),
        cantidad_unidad       = dplyr::last(cantidad_unidad),
        unidades              = dplyr::last(unidades),
        cantidad_total        = dplyr::last(cantidad_total),
        url_producto          = dplyr::last(url),
        texto_original        = dplyr::first(texto_original_anuncio),
        primera_consulta      = dplyr::first(fecha_consulta),
        ultima_consulta       = dplyr::last(fecha_consulta),
        .groups = "drop") %>%
      dplyr::mutate(marca_std        = estandarizar_marca(marca_original),
                    subcategoria_std = estandarizar_subcategoria(nombre_original)) %>%
      dplyr::left_join(cmap, by = "comercio")

    # los vinculos con el catalogo se rehacen siempre (por si cambia un SKU)
    DBI::dbExecute(con, "UPDATE anuncios SET id_producto = NULL WHERE id_producto IS NOT NULL")

    if (nrow(anu) > 0) {
      DBI::dbExecute(con,
        "INSERT INTO anuncios (id_comercio, sku, nombre_original, presentacion_original,
                               marca_original, marca_std, subcategoria_std, envase, medida,
                               cantidad_unidad, unidades, cantidad_total, url_producto,
                               texto_original, primera_consulta, ultima_consulta)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
         ON CONFLICT (id_comercio, sku) DO UPDATE SET
           nombre_original = excluded.nombre_original,
           presentacion_original = excluded.presentacion_original,
           marca_original = excluded.marca_original, marca_std = excluded.marca_std,
           subcategoria_std = excluded.subcategoria_std, envase = excluded.envase,
           medida = excluded.medida, cantidad_unidad = excluded.cantidad_unidad,
           unidades = excluded.unidades, cantidad_total = excluded.cantidad_total,
           url_producto = excluded.url_producto,
           primera_consulta = MIN(anuncios.primera_consulta, excluded.primera_consulta),
           ultima_consulta  = MAX(anuncios.ultima_consulta,  excluded.ultima_consulta)",
        params = unname(as.list(anu[, c("id_comercio", "sku", "nombre_original",
          "presentacion_original", "marca_original", "marca_std", "subcategoria_std",
          "envase", "medida", "cantidad_unidad", "unidades", "cantidad_total",
          "url_producto", "texto_original", "primera_consulta", "ultima_consulta")])))
    }

    # vinculo con el catalogo: marca y subcategoria de los comparables vienen de `productos`
    for (i in seq_len(nrow(catalogo_largo))) {
      DBI::dbExecute(con,
        "UPDATE anuncios
            SET id_producto      = :id,
                marca_std        = (SELECT marca_std        FROM productos WHERE id_producto = :id),
                subcategoria_std = (SELECT subcategoria_std FROM productos WHERE id_producto = :id)
          WHERE sku = :sku
            AND id_comercio = (SELECT id_comercio FROM comercios WHERE codigo = :cm)",
        params = list(id = as.integer(catalogo_largo$id_producto[i]),
                      sku = trimws(catalogo_largo$sku[i]),
                      cm  = catalogo_largo$comercio[i]))
    }

    # control: el SKU del catalogo debe tener el mismo contenido que el producto comun
    desajuste <- DBI::dbGetQuery(con,
      "SELECT c.codigo AS comercio, a.sku, a.nombre_original, p.nombre AS producto_catalogo,
              a.cantidad_total AS total_anuncio, p.cantidad_total AS total_catalogo
         FROM anuncios a JOIN productos p ON p.id_producto = a.id_producto
         JOIN comercios c ON c.id_comercio = a.id_comercio
        WHERE a.cantidad_total IS NULL OR a.cantidad_total <> p.cantidad_total")
    if (nrow(desajuste) > 0) {
      cat("!! REVISAR: el contenido del anuncio no coincide con el del producto comun (SKU mal asignado?)\n")
      print(desajuste)
    }

    # ---- 5. observaciones (ON CONFLICT: solo ignora la duplicada, no otros errores) ----
    amap <- DBI::dbGetQuery(con, "SELECT id_anuncio, id_comercio, sku FROM anuncios")
    obs <- base %>%
      dplyr::left_join(cmap, by = "comercio") %>%
      dplyr::left_join(amap, by = c("id_comercio", "sku")) %>%
      dplyr::mutate(codigo_corrida = id_ejecucion, id_ejecucion = NULL) %>%
      dplyr::left_join(emap, by = c("id_comercio", "codigo_corrida")) %>%
      dplyr::distinct(id_anuncio, fecha_consulta, .keep_all = TRUE)

    n_insertadas <- 0L
    if (nrow(obs) > 0) {
      n_insertadas <- DBI::dbExecute(con,
        "INSERT INTO observaciones (id_anuncio, id_comercio, id_ejecucion, fecha_hora_consulta,
                                    precio_cop, precio_lista_cop, es_promocion, url_extraccion,
                                    texto_original_anuncio, busqueda)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
         ON CONFLICT (id_anuncio, fecha_hora_consulta) DO NOTHING",
        params = unname(as.list(data.frame(
          obs$id_anuncio, obs$id_comercio, obs$id_ejecucion, obs$fecha_consulta,
          as.integer(obs$precio_real_cop), as.integer(obs$precio_lista_cop),
          as.integer(obs$oferta_general %in% 1L), obs$url_extraccion,
          obs$texto_original_anuncio, obs$busqueda,
          stringsAsFactors = FALSE))))
    }

    # ---- 6. cierre de cada ejecucion: n_registros y estado ----
    for (i in seq_len(nrow(runs))) {
      cm  <- runs$comercio[i];  cod <- runs$codigo_corrida[i]
      id_e <- emap$id_ejecucion[emap$codigo_corrida == cod & emap$id_comercio == runs$id_comercio[i]]
      n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM observaciones WHERE id_ejecucion = ?",
                           params = list(id_e))$n
      log_cm <- if (is.null(log)) data.frame(estado = character(), mensaje = character())
                else log[log$comercio == cm & log$id_ejecucion == cod, c("estado", "mensaje")]
      esperados <- trimws(catalogo_largo$sku[catalogo_largo$comercio == cm])
      vistos    <- base$sku[base$comercio == cm & base$id_ejecucion == cod]
      st <- calcular_estado(n, log_cm, length(setdiff(esperados, vistos)))
      DBI::dbExecute(con,
        "UPDATE ejecuciones SET n_registros = ?, estado = ?, mensaje = ? WHERE id_ejecucion = ?",
        params = list(as.integer(n), st$estado, st$mensaje, id_e))
    }

    list(n_filas = nrow(base), n_unicas = nrow(obs), n_insertadas = n_insertadas,
         n_descartadas = nrow(descartadas))
  })

  cat(sprintf("SQLite: %d fila(s) recibidas, %d nueva(s) insertadas, %d ya existian, %d descartadas por datos incompletos\n",
              res$n_filas, res$n_insertadas, res$n_unicas - res$n_insertadas, res$n_descartadas))
  print(DBI::dbGetQuery(con,
    "SELECT e.codigo_corrida, c.codigo AS comercio, e.n_registros, e.estado, e.mensaje
       FROM ejecuciones e JOIN comercios c USING (id_comercio)
      ORDER BY e.codigo_corrida DESC, c.codigo LIMIT 9"), row.names = FALSE)
  invisible(res)
}
