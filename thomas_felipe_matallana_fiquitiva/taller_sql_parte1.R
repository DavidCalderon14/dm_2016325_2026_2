url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
ruta_db     <- "chinook.db"

if (!file.exists(ruta_db)) {
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
  cat("Base de datos descargada en:", ruta_db, "\n")
} else {
  cat("La base de datos ya existe:", ruta_db, "\n")
}

paquetes  <- c("DBI", "RSQLite", "dplyr", "knitr")
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)
if (length(pendientes) > 0) install.packages(pendientes)

library(DBI)
library(RSQLite)
library(dplyr)

con <- dbConnect(RSQLite::SQLite(), "chinook.db")
cat("Conexión establecida.\n")

dbListTables(con)

# ====================================================================
# EJERCICIO 1: 
# ====================================================================

# 1. ¿Cuántas pistas hay en total en la base de datos?
ej1_1<- dbGetQuery(con, "SELECT COUNT(*) AS total_pistas FROM Track;")
print(ej1_1)

# 2. ¿Cuántos géneros distintos existen?
ej1_2 <- dbGetQuery(con, "SELECT COUNT(DISTINCT GenreId) AS generos_distintos FROM Track;")
print(ej1_2)

# 3. Género, número de pistas y duración promedio
ej1_3 <- dbGetQuery(con, "
  SELECT g.Name AS genero, COUNT(t.TrackId) AS num_pistas, ROUND(AVG(t.Milliseconds) / 60000.0, 2) AS duracion_promedio_min
  FROM Track t
  JOIN Genre g ON t.GenreId = g.GenreId
  GROUP BY g.Name
  ORDER BY num_pistas DESC;
")
print(head(ej1_3))

# 4. Las 5 pistas más largas
ej1_4 <- dbGetQuery(con, "
  SELECT t.Name AS pista, a.Title AS album, ROUND(t.Milliseconds / 60000.0, 2) AS duracion_min
  FROM Track t
  JOIN Album a ON t.AlbumId = a.AlbumId
  ORDER BY t.Milliseconds DESC
  LIMIT 5;
")
print(ej1_4)

# 5. Pistas con UnitPrice > $0.99 y su porcentaje
ej1_5 <- dbGetQuery(con, "
  SELECT 
      COUNT(CASE WHEN UnitPrice > 0.99 THEN 1 END) AS pistas_caras,
      ROUND(COUNT(CASE WHEN UnitPrice > 0.99 THEN 1 END) * 100.0 / COUNT(*), 2) AS porcentaje_total
  FROM Track;
")
print(ej1_5)

# 6. Media, mínimo, máximo y varianza de la duración (milisegundos)
ej1_6 <- dbGetQuery(con, "
  SELECT 
      ROUND(AVG(Milliseconds), 2) AS media_ms,
      MIN(Milliseconds) AS minimo_ms,
      MAX(Milliseconds) AS maximo_ms,
      ROUND(AVG(Milliseconds * Milliseconds) - AVG(Milliseconds) * AVG(Milliseconds), 2) AS varianza_ms
  FROM Track;
")
print(ej1_6)


# ====================================================================
# EJERCICIO 2:
# ====================================================================

# 1. Total de ingresos y promedio por factura
ej2_1 <- dbGetQuery(con, "
  SELECT ROUND(SUM(Total), 2) AS total_ingresos, ROUND(AVG(Total), 2) AS promedio_por_factura
  FROM Invoice;
")
print(ej2_1)

# 2. Facturas emitidas por año (ordenadas cronológicamente)
ej2_2 <- dbGetQuery(con, "
  SELECT SUBSTR(InvoiceDate, 1, 4) AS anio, COUNT(*) AS facturas_emitidas
  FROM Invoice
  GROUP BY anio
  ORDER BY anio ASC;
")
print(ej2_2)

# 3. Los 5 países con más ingresos
ej2_3 <- dbGetQuery(con, "
  SELECT BillingCountry AS pais, COUNT(*) AS num_facturas, ROUND(SUM(Total), 2) AS ingreso_total, ROUND(AVG(Total), 2) AS promedio_factura
  FROM Invoice
  GROUP BY BillingCountry
  ORDER BY ingreso_total DESC
  LIMIT 5;
")
print(ej2_3)

# 4. Desviación estándar del total de facturas
# (Calculamos la varianza en SQL y la raíz cuadrada en R)
ej2_4 <- dbGetQuery(con, "
  SELECT ROUND(AVG(Total * Total) - AVG(Total) * AVG(Total), 2) AS varianza_total
  FROM Invoice;
")
desviacion_estandar <- sqrt(ej2_4$varianza_total)
cat("\nDesviación estándar del total de facturas:", desviacion_estandar, "\n\n")

# 5. Meses con mayor y menor ingreso promedio
ej2_5 <- dbGetQuery(con, "
  SELECT SUBSTR(InvoiceDate, 1, 7) AS mes_anio, ROUND(AVG(Total), 2) AS ingreso_promedio
  FROM Invoice
  GROUP BY mes_anio
  ORDER BY ingreso_promedio DESC;
")

cat("Mes con el mayor ingreso promedio:\n")
print(head(ej2_5, 1))

cat("\nMes con el menor ingreso promedio:\n")
print(tail(ej2_5, 1))

# ====================================================================
# FINALIZAR
# ====================================================================

# Es importante cerrar siempre la conexión al terminar el trabajo
dbDisconnect(con)
cat("\nConexión cerrada.\n")
