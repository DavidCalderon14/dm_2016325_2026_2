# =====================================================================
# exploracion.R  -  Laboratorio: ejecutar, revisar y guardar
# Usa las funciones de scraper.R. Ejecutar por secciones, de arriba a abajo.
# =====================================================================

# ---- 0. Configuración --------------------------------------------------
# Carpeta del proyecto, RELATIVA al directorio de trabajo (getwd()).
# Sin "/" al inicio: en Windows eso apuntaría a la raíz del disco.
dir_proyecto <- "johan_steven_avilan_penaloza/parcial_01"

source(file.path(dir_proyecto, "scraper.R"))

# ---- 1. Ejecución de Farmatodo (EN LÍNEA) ------------------------------
# Abre Chrome y tarda unos minutos. Guarda las copias crudas en raw/.
fecha        <- format(Sys.time(), "%Y-%m-%d", tz = "America/Bogota")
id_ejecucion <- paste0("farmatodo_", fecha)
dir_raw      <- file.path(dir_proyecto, "raw", "farmatodo", fecha)

farmatodo <- recolectar_farmatodo(id_ejecucion, dir_raw, modo = "online")

# ---- 2. Resumen y chequeos de calidad ---------------------------------
cat("\n---------- RESUMEN ----------\n")
print(count(farmatodo, subcategoria_origen))
cat("Filas totales:            ", nrow(farmatodo), "\n")
cat("SKU únicos (sin repetir): ", n_distinct(farmatodo$sku), "\n")
cat("Precios faltantes:        ", sum(is.na(farmatodo$precio)), "\n")
cat("Con vendedor externo:     ", sum(!is.na(farmatodo$vendedor)), "\n")

# Filas sin SKU o sin URL (idealmente 0)
farmatodo |> filter(is.na(sku) | is.na(url)) |>
  select(sku, nombre_original, precio, url, subcategoria_origen)

# SKU repetidos de verdad (idealmente 0 filas)
farmatodo |> filter(!is.na(sku)) |> count(sku) |> filter(n > 1)

# Productos sin precio (en la ejecución del 04-10 eran 2, ambos agotados)
farmatodo |> filter(is.na(precio)) |>
  select(sku, nombre_original, precio_bloque_original, subcategoria_origen)

# ---- 3. Respaldo de la tabla (NO va en raw/) ---------------------------
# raw/ guarda el HTML original; esto es solo la tabla ya procesada.
dir_datos <- file.path(dir_proyecto, "datos")
dir.create(dir_datos, recursive = TRUE, showWarnings = FALSE)

saveRDS(farmatodo, file.path(dir_datos, paste0("farmatodo_", fecha, ".rds")))
write.csv(farmatodo, file.path(dir_datos, paste0("farmatodo_", fecha, ".csv")),
          row.names = FALSE, fileEncoding = "UTF-8")

list.files(dir_datos)

# ---- 4. Prueba del MODO SIN CONEXIÓN -----------------------------------
# Reprocesa las copias crudas sin abrir Chrome ni consultar el sitio.
farmatodo_off <- recolectar_farmatodo("offline", dir_raw, modo = "offline")

nrow(farmatodo); nrow(farmatodo_off)
setequal(farmatodo$sku, farmatodo_off$sku)     # debe dar TRUE
all.equal(farmatodo$precio[order(farmatodo$sku)],
          farmatodo_off$precio[order(farmatodo_off$sku)])   # debe dar TRUE

# ---- 5. SEGUNDO Y TERCER COMERCIO (VTEX): La Rebaja y Locatel ----------
# Van por el servicio JSON del propio sitio: sin Chrome, rápido, y con total
# declarado por el sitio para verificar que la recolección está completa.
dir_raw_lr  <- file.path(dir_proyecto, "raw", "la_rebaja", fecha)
dir_raw_loc <- file.path(dir_proyecto, "raw", "locatel",   fecha)

la_rebaja <- recolectar_vtex("La Rebaja", "www.larebajavirtual.com",
                             "drogueria/cuidado-de-la-herida",
                             paste0("larebaja_", fecha), dir_raw_lr, "online")

locatel   <- recolectar_vtex("Locatel", "www.locatelcolombia.com",
                             "drogueria/cuidado-heridas",
                             paste0("locatel_", fecha), dir_raw_loc, "online")

# ---- 6. Revisión de La Rebaja y Locatel ---------------------------------
resumen_vtex <- function(df) {
  cat("\n=====", df$comercio[1], "=====\n")
  cat("Total declarado por el sitio:  ", attr(df, "total_sitio"), "\n")
  cat("Productos distintos obtenidos: ", n_distinct(df$id_producto), "\n")
  cat("Filas (SKU) obtenidas:         ", nrow(df), "\n")
  cat("Sin precio (agotados):         ", sum(is.na(df$precio)), "\n")
  cat("Con EAN (código de barras):    ", sum(!is.na(df$ean) & df$ean != ""), "\n")
  cat("En promoción:                  ", sum(df$es_promocion == 1, na.rm = TRUE), "\n")
  print(count(df, subcategoria_origen, sort = TRUE))
}
resumen_vtex(la_rebaja)
resumen_vtex(locatel)

# Comprobación manual de precios: compara estas filas con lo que ves en el
# sitio, en tu navegador, con la ciudad en Bogotá.
la_rebaja |> filter(str_detect(nombre_original, regex("isodine|alcohol mk", ignore_case = TRUE))) |>
  select(nombre_original, presentacion_original, precio, precio_antes, es_promocion, disponible)

# ---- 7. Prueba del modo sin conexión (La Rebaja y Locatel) --------------
lr_off  <- recolectar_vtex("La Rebaja", "www.larebajavirtual.com", "drogueria/cuidado-de-la-herida",
                           "offline", dir_raw_lr, "offline")
loc_off <- recolectar_vtex("Locatel", "www.locatelcolombia.com", "drogueria/cuidado-heridas",
                           "offline", dir_raw_loc, "offline")
setequal(la_rebaja$sku, lr_off$sku)
setequal(locatel$sku, loc_off$sku)

# ---- 8. ¿Dónde viene el contenido (ej. 700 ml)? Inspección del JSON ------
# Muestra todos los campos de un producto de cada comercio (sin descargar nada).
leer_raw <- function(dir_raw) {
  f <- list.files(dir_raw, pattern = "^pag_.*json$", full.names = TRUE)
  unlist(lapply(f, function(x)
    jsonlite::fromJSON(paste(readLines(x, encoding = "UTF-8", warn = FALSE), collapse = "\n"),
                       simplifyVector = FALSE)), recursive = FALSE)
}

p_lr  <- Filter(function(p) grepl("ALCOHOL MK", p$productName), leer_raw(dir_raw_lr))[[1]]
p_loc <- leer_raw(dir_raw_loc)[[1]]

for (p in list(p_lr, p_loc)) {
  cat("\n=====", p$productName, "=====\n")
  str(p[setdiff(names(p), c("items", "description", "metaTagDescription"))], max.level = 2)
  cat("ITEM -> name:", p$items[[1]]$name, "| nameComplete:", p$items[[1]]$nameComplete,
      "| unidad:", p$items[[1]]$measurementUnit, "| multiplicador:", p$items[[1]]$unitMultiplier, "\n")
}

# ---- 9. ¿Qué marcas tienen en común los tres comercios? -----------------
norm <- function(x) x |> str_to_upper() |> stringi::stri_trans_general("Latin-ASCII") |> str_squish()
comunes <- Reduce(intersect, list(unique(norm(farmatodo$marca_original)),
                                  unique(norm(la_rebaja$marca_original)),
                                  unique(norm(locatel$marca_original))))
comunes

# ---- 10. Reprocesar con el parser actualizado (SIN descargar nada) --------
# Las respuestas ya están guardadas en raw/. Esto reconstruye las tablas con
# las columnas nuevas de contenido y presentación, sin tocar los sitios.
la_rebaja <- recolectar_vtex("La Rebaja", "www.larebajavirtual.com", "drogueria/cuidado-de-la-herida",
                             paste0("larebaja_", fecha), dir_raw_lr, "offline")
locatel   <- recolectar_vtex("Locatel", "www.locatelcolombia.com", "drogueria/cuidado-heridas",
                             paste0("locatel_", fecha), dir_raw_loc, "offline")

la_rebaja |> filter(str_detect(nombre_original, regex("alcohol mk|isodine", ignore_case = TRUE))) |>
  select(nombre_original, presentacion_texto_origen, contenido_origen,
         contenido_unidad_origen, unidades_pack_origen, precio) |> print(width = Inf)

locatel |> select(nombre_original, presentacion_original, contenido_origen,
                  contenido_unidad_origen, peso_origen, precio) |> head(8) |> print(width = Inf)

# ---- 11. Árbol de categorías (para ubicar los antisépticos de Locatel) -----
# Servicio de VTEX que lista las categorías reales de cada tienda.
arbol_vtex <- function(dominio, niveles = 3) {
  u <- paste0("https://", dominio, "/api/catalog_system/pub/category/tree/", niveles)
  j <- request(u) |> req_user_agent("Mozilla/5.0 (taller universitario)") |>
    req_perform() |> resp_body_json()
  aplanar <- function(n, ruta = "") {
    r <- paste0(ruta, "/", n$name)
    bind_rows(tibble(id = n$id, ruta = r, url = as.character(n$url)),
              map_dfr(n$children, aplanar, ruta = r))
  }
  map_dfr(j, aplanar)
}

arbol_loc <- arbol_vtex("www.locatelcolombia.com")
arbol_loc |>
  filter(str_detect(ruta, regex("antisep|primeros|botiqu|herida|curaci|alcohol|yodo", ignore_case = TRUE))) |>
  print(n = 50, width = Inf)

# ---- 13. Candidatos a productos comparables -------------------------------
# La Rebaja y Locatel traen EAN (código de barras): las coincidencias por EAN
# son el MISMO producto, sin ambigüedad. Farmatodo no trae EAN en el listado,
# así que ahí se cruza por marca, nombre y contenido.
sin_espacios <- function(x) x |> norm() |> str_remove_all("\\s")   # "CURE BAND" = "CUREBAND"

solo_ean <- function(df) df |> filter(!is.na(ean), ean != "") |> distinct(ean, .keep_all = TRUE)

comunes_ean <- inner_join(
  solo_ean(la_rebaja) |> transmute(ean, marca = marca_original, nombre_lr = nombre_original,
                                   contenido_lr = paste(contenido_origen, contenido_unidad_origen),
                                   precio_lr = precio),
  solo_ean(locatel)   |> transmute(ean, nombre_loc = nombre_original,
                                   contenido_loc = paste(contenido_origen, contenido_unidad_origen),
                                   precio_loc = precio),
  by = "ean")
nrow(comunes_ean)
print(comunes_ean, n = 80, width = Inf)

# Marcas comunes a los tres comercios, ignorando espacios
marcas3 <- Reduce(intersect, list(unique(sin_espacios(farmatodo$marca_original)),
                                  unique(sin_espacios(la_rebaja$marca_original)),
                                  unique(sin_espacios(locatel$marca_original))))
marcas3

# Productos de Farmatodo de esas marcas (para buscar la pareja de cada coincidencia)
farmatodo |> mutate(m = sin_espacios(marca_original)) |> filter(m %in% marcas3) |>
  arrange(m, nombre_original) |>
  select(sku, marca_original, nombre_original, precio) |> print(n = 100, width = Inf)

# ---- 14. Crear la base SQLite y cargar lo que ya está en memoria ---------
# Prueba rápida: NO consulta ningún sitio.
ruta_db <- file.path(dir_proyecto, "comparador_precios.sqlite")
if (file.exists(ruta_db)) file.remove(ruta_db)   # empezar de cero (el esquema cambió)
con <- crear_base(ruta_db)

cargar_ejecucion(con, farmatodo, "Farmatodo", n_subcats_esperadas = length(slugs_farmatodo))
cargar_ejecucion(con, la_rebaja, "La Rebaja")
cargar_ejecucion(con, locatel,   "Locatel")

DBI::dbGetQuery(con, "SELECT * FROM ejecuciones")
DBI::dbGetQuery(con, "
  SELECT c.nombre AS comercio, COUNT(DISTINCT a.id_anuncio) AS anuncios, COUNT(*) AS observaciones
  FROM observaciones o
  JOIN anuncios a  ON a.id_anuncio = o.id_anuncio
  JOIN comercios c ON c.id_comercio = a.id_comercio
  GROUP BY c.nombre")

# Repetir la carga NO debe duplicar nada (unicidad): las 3 deben dar 0 nuevas
cargar_ejecucion(con, farmatodo, "Farmatodo", n_subcats_esperadas = length(slugs_farmatodo))
DBI::dbDisconnect(con)
