# =====================================================================
# exploracion.R  -  Laboratorio: ejecutar, revisar y guardar
# Usa las funciones de scraper.R. Ejecutar por secciones, de arriba a abajo.
# =====================================================================

# ---- 0. Configuración --------------------------------------------------
# Carpeta del proyecto, RELATIVA al directorio de trabajo (getwd()).
# Sin "/" al inicio: en Windows eso apuntaría a la raíz del disco.
dir_trabajo <- "johan_steven_avilan_penaloza/parcial_01"

source(file.path(dir_trabajo, "scraper.R"))

# ---- 1. Ejecución de Farmatodo (EN LÍNEA) ------------------------------
fecha        <- format(Sys.time(), "%Y-%m-%d", tz = "America/Bogota")
id_ejecucion <- paste0("farmatodo_", fecha)
dir_raw      <- file.path(dir_trabajo, "raw", "farmatodo", fecha)

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

# Productos sin precio (por estar agotados)
farmatodo |> filter(is.na(precio)) |>
  select(sku, nombre_original, precio_bloque_original, subcategoria_origen)

# ---- 3. Respaldo de la tabla -----------------------------------------
dir_datos <- file.path(dir_trabajo, "datos")
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


