# =====================================================================
# scraper.R  -  Módulo de extracción (Taller 1, Minería de Datos)
# Categoría: Primeros auxilios y curación
# Comercio implementado: Farmatodo   |   Pendientes: comercios 2 y 3
#
# Este archivo SOLO define funciones y constantes: al hacer source() no
# descarga nada. Lo usan exploracion.R y, al final, el taller_1.qmd.
#
# Recolección con UNA llamada:
#   recolectar_farmatodo(id_ejecucion, dir_raw, modo = "online")
#       -> consulta el sitio con Chrome y guarda las copias crudas en dir_raw
#   recolectar_farmatodo(id_ejecucion, dir_raw, modo = "offline")
#       -> reprocesa las copias de dir_raw SIN consultar el sitio
#
# Requiere Google Chrome instalado (read_html_live() usa chromote).
# =====================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(rvest, dplyr, stringr, purrr, tibble)

# ---- Constantes de Farmatodo -----------------------------------------

sitio_farmatodo <- "https://www.farmatodo.com.co"
base_farmatodo  <- paste0(sitio_farmatodo,
                          "/categorias/salud-y-medicamentos/botiquin-y-primeros-auxilios")

# Subcategorías a recolectar. Quedan FUERA (no son de la categoría asignada):
# "jeringas-guantes-y-material-hosp" y "aparatos-medicos-y-pastilleros".
# La exclusión fina (guantes, tapabocas, etc.) se hace después, en la limpieza.
slugs_farmatodo <- c(
  "curitas-algodones-adhesivos-y-botiquin",
  "antisepticos",
  "vendas",
  "compresas"
)

# ---- Utilidades -------------------------------------------------------

# Fecha y hora ISO 8601 con hora de Colombia (ej. 2026-10-04T17:30:49-05:00)
ahora_iso <- function() {
  x <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "America/Bogota")
  sub("(\\d{2})$", ":\\1", x)
}

# ---- Parser: de cada tarjeta de producto a una fila -------------------

parsear_farmatodo <- function(pagina, subcategoria, id_ejecucion, ts) {
  tarjetas <- html_elements(pagina, "div.product-card-handler")
  if (length(tarjetas) == 0) return(tibble())

  map_dfr(tarjetas, function(t) {
    enlace <- html_element(t, "a.product-card-handler__detail-description")
    # Enlace: primero el del nombre; si falta, el de la imagen
    href_det <- html_attr(enlace, "href")
    href_img <- html_attr(html_element(t, "a.product-card-handler__image-link"), "href")
    href     <- dplyr::coalesce(href_det, href_img)
    # SKU: del enlace; si falta, del atributo data-test de la tarjeta
    sku_card <- str_match(html_attr(t, "data-test"), "product-card-(\\d+)")[, 2]
    sku      <- dplyr::coalesce(str_match(href, "/producto/(\\d+)")[, 2], sku_card)

    if (is.na(href)) {   # diagnóstico: mostrar cómo es esta tarjeta
      cat("\n!! Tarjeta SIN enlace (", subcategoria, ") SKU por data-test:", sku_card, "\n",
          substr(as.character(t), 1, 1500), "\n")
    }

    bloque <- trimws(html_text2(html_element(t, "app-product-all-price")))
    montos <- as.numeric(gsub("[^0-9]", "",
                              str_extract_all(bloque, "\\$\\s*[0-9.]+")[[1]]))

    tibble(
      comercio               = "Farmatodo",
      sku                    = sku,
      nombre_original        = trimws(html_text(enlace)),
      marca_original         = trimws(html_text(html_element(t, ".product-card-handler__brand-value"))),
      subcategoria_origen    = subcategoria,
      # Sin precio publicado (producto agotado) => NA, nunca 0
      precio                 = if (length(montos) == 0) NA_real_ else min(montos),
      precio_antes           = if (length(montos) >= 2) max(montos) else NA_real_,
      es_promocion           = as.integer(length(montos) >= 2),  # SUPOSICIÓN: verificar con una oferta real
      precio_bloque_original = bloque,
      pum_texto              = trimws(html_text(html_element(t, ".product-card-handler__pum"))),
      vendedor               = str_match(html_text2(t), "Vendido por:\\s*([^\\n]+)")[, 2],
      url                    = if (is.na(href)) NA_character_ else paste0(sitio_farmatodo, href),
      fecha_hora_consulta    = ts,
      id_ejecucion           = id_ejecucion
    )
  })
}

# ---- Una subcategoría completa, con Chrome (read_html_live) -----------
# Pulsa "Cargar más" hasta que no aparezcan más productos (confirmando con
# una espera extra antes de parar), guarda la copia cruda si se da dir_raw
# y devuelve la tabla.

scrape_subcat_live <- function(slug, id_ejecucion = "prueba", max_clics = 60,
                               dir_raw = NULL) {
  url <- paste0(base_farmatodo, "/", slug)
  ts  <- ahora_iso()

  s <- rvest::read_html_live(url)
  on.exit(s$session$close(), add = TRUE)
  Sys.sleep(5)

  # productos únicos en pantalla (por enlace, para no contar duplicados)
  contar <- function() {
    s |> html_elements("a.product-card-handler__detail-description") |>
      html_attr("href") |> unique() |> length()
  }

  # pulsa el botón "Cargar más" buscándolo por su texto
  clic_cargar_mas <- function() {
    r <- s$session$Runtime$evaluate("
      (() => {
        const b = [...document.querySelectorAll('button')]
          .find(x => x.textContent.trim() === 'Cargar más');
        if (!b) return false;
        b.scrollIntoView();
        b.click();
        return true;
      })()
    ")
    isTRUE(r$result$value)
  }

  n_prev <- contar()
  cat("[", slug, "] inicial:", n_prev, "productos\n")

  for (i in seq_len(max_clics)) {
    if (!clic_cargar_mas()) { cat("   ya no hay botón\n"); break }
    Sys.sleep(3)
    n_act <- contar()
    if (n_act == n_prev) {            # confirmar: esperar más y recontar
      Sys.sleep(6)
      n_act <- contar()
      if (n_act == n_prev) { cat("   sin más productos tras esperar\n"); break }
    }
    cat("   clic", i, "->", n_act, "productos\n")
    n_prev <- n_act
  }

  # Copia cruda (sección 3.3 del enunciado): el HTML tal cual está en Chrome, comprimido
  if (!is.null(dir_raw)) {
    dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)
    html_crudo <- s$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    con <- gzfile(file.path(dir_raw, paste0(slug, ".html.gz")), "w")
    writeLines(html_crudo, con, useBytes = TRUE)
    close(con)
    writeLines(ts, file.path(dir_raw, paste0(slug, ".ts.txt")))   # fecha-hora de captura
    cat("   copia cruda guardada en", dir_raw, "\n")
  }

  df <- parsear_farmatodo(s, slug, id_ejecucion, ts)
  if (nrow(df) > 0) df <- distinct(df, sku, nombre_original, .keep_all = TRUE)
  cat("   FILAS FINALES:", nrow(df), "\n")
  df
}

# ---- MODO SIN CONEXIÓN: reprocesar una copia cruda ---------------------

parsear_desde_raw <- function(archivo, slug, id_ejecucion = "offline") {
  html <- paste(readLines(gzfile(archivo), encoding = "UTF-8", warn = FALSE),
                collapse = "\n")
  pagina <- read_html(charToRaw(html), encoding = "UTF-8")
  # la fecha-hora es la de la captura original, no la de hoy
  archivo_ts <- sub("\\.html\\.gz$", ".ts.txt", archivo)
  ts <- if (file.exists(archivo_ts)) readLines(archivo_ts, warn = FALSE)[1] else NA_character_
  df <- parsear_farmatodo(pagina, slug, id_ejecucion, ts)
  if (nrow(df) > 0) df <- distinct(df, sku, nombre_original, .keep_all = TRUE)
  df
}

# ---- Recolección completa de Farmatodo con UNA llamada -----------------

recolectar_farmatodo <- function(id_ejecucion, dir_raw,
                                 modo = c("online", "offline"),
                                 slugs = slugs_farmatodo) {
  modo <- match.arg(modo)

  if (modo == "online") {
    # un fallo en una subcategoría no frena las demás, pero se muestra el error
    seguro <- possibly(scrape_subcat_live, otherwise = tibble(), quiet = FALSE)
    res <- map(slugs, function(s) seguro(s, id_ejecucion = id_ejecucion, dir_raw = dir_raw))
  } else {
    res <- map(slugs, function(s) {
      f <- file.path(dir_raw, paste0(s, ".html.gz"))
      if (!file.exists(f)) {
        warning("No existe la copia cruda: ", f)
        return(tibble())
      }
      parsear_desde_raw(f, s, id_ejecucion)
    })
  }

  names(res) <- slugs
  bind_rows(res)
}
