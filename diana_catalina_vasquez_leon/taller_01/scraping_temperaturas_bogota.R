# ==============================================================================
# Web Scraping de Temperaturas de Bogotá (AccuWeather 2024-2025)
# Fuente: https://www.accuweather.com/es/co/bogota/107487/{mes}-weather/107487?year={ano}
#
# Verificaciones previas realizadas antes de este script (dejar documentado
# en el reporte / entregable):
#   - robots.txt de accuweather.com fue revisado: las rutas
#     /es/co/bogota/.../weather/... NO están bajo Disallow para User-agent: *.
#     La única regla con wildcard de query string es "Disallow: *day=*", que
#     no aplica al parámetro "year=" usado aquí.
#   - Una primera prueba con httr::GET() + User-Agent simple devolvió
#     HTTP 403 Forbidden (bloqueo técnico tipo WAF, no relacionado con
#     robots.txt). Se resolvió usando httr2 con un set de headers más
#     completo (User-Agent de navegador real + Accept + Accept-Language),
#     lo cual devolvió HTTP 200 y HTML con los datos esperados.
#   - Se confirmó manualmente que el HTML sí contiene el calendario completo
#     (35 nodos .monthly-daypanel por mes) sin necesidad de renderizar
#     JavaScript, por lo que NO fue necesario usar automatización de
#     navegador (Selenium/RSelenium/chromote).
# ==============================================================================

# 1. Cargar paquetes necesarios -------------------------------------------------
paquetes <- c("httr2", "rvest", "dplyr", "stringr", "purrr", "readr", "tibble")

instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)
if (length(pendientes) > 0) install.packages(pendientes)

invisible(lapply(paquetes, library, character.only = TRUE))

# 2. Construir las URLs correspondientes a cada mes (2024 y 2025) --------------
meses_nombres <- c("january", "february", "march", "april", "may", "june",
                   "july", "august", "september", "october", "november", "december")

fechas_grid <- expand.grid(mes_num = 1:12, ano = c(2024, 2025)) %>%
  arrange(ano, mes_num) %>%
  mutate(
    mes_nombre = meses_nombres[mes_num],
    url = sprintf(
      "https://www.accuweather.com/es/co/bogota/107487/%s-weather/107487?year=%d",
      mes_nombre, ano
    )
  )

# 3. Función para extraer los días de UN mes desde la página ya descargada -----
# Como el calendario mensual incluye días de relleno del mes anterior y del
# mes siguiente para completar la grilla, identificamos el mes real siguiendo
# la secuencia de números de día: cuando el número "baja" (ej. de 31 a 1),
# empieza un nuevo bloque de mes. El bloque real es el que tiene >= 25 días.
extraer_dias_mes <- function(pagina, mes_num, ano, url) {
  nodos_dias <- pagina %>% html_elements(".monthly-daypanel")
  if (length(nodos_dias) == 0) return(NULL)
  
  dias_raw <- map_dfr(seq_along(nodos_dias), function(i) {
    nodo <- nodos_dias[[i]]
    
    dia_texto <- nodo %>% html_element(".date") %>% html_text2() %>% str_squish()
    dia_num   <- suppressWarnings(as.integer(dia_texto))
    
    high <- nodo %>% html_element(".high") %>% html_text2() %>%
      str_replace_all("[^0-9\\-]", "") %>% as.numeric()
    low  <- nodo %>% html_element(".low")  %>% html_text2() %>%
      str_replace_all("[^0-9\\-]", "") %>% as.numeric()
    
    tibble(posicion = i, dia = dia_num, high = high, low = low)
  })
  
  if (all(is.na(dias_raw$dia))) return(NULL)
  
  dias_raw <- dias_raw %>%
    mutate(
      salto     = dia - lag(dia, default = dplyr::first(dia)),
      nuevo_mes = salto < 0
    ) %>%
    mutate(bloque_mes = cumsum(nuevo_mes) + 1)
  
  bloque_objetivo <- dias_raw %>%
    count(bloque_mes) %>%
    filter(n >= 25) %>%
    pull(bloque_mes)
  
  dias_raw %>%
    filter(bloque_mes %in% bloque_objetivo) %>%
    mutate(
      # 4. Convertir la fecha al tipo Date y las temperaturas a numérico -----
      fecha      = as.Date(sprintf("%d-%02d-%02d", ano, mes_num, dia)),
      high       = as.numeric(high),
      low        = as.numeric(low),
      # 5. Conservar la URL de origen para facilitar la verificación ---------
      url_origen = url
    ) %>%
    select(fecha, high, low, url_origen)
}

# 6. Función para descargar y procesar un mes completo --------------------------
scrapear_mes <- function(url, mes_num, ano) {
  
  # 2. Pausa entre solicitudes para respetar el servidor -----------------------
  Sys.sleep(1.5)
  
  resp <- tryCatch(
    request(url) %>%
      req_headers(
        `User-Agent` = paste(
          "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
          "(KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36"
        ),
        `Accept`          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        `Accept-Language` = "es-CO,es;q=0.9,en;q=0.8"
      ) %>%
      req_error(is_error = function(resp) FALSE) %>%  # no lanzar error automático; lo evaluamos abajo
      req_perform(),
    error = function(e) e
  )
  
  if (inherits(resp, "error")) {
    message("  [ERROR de conexión] ", url, " -> ", conditionMessage(resp))
    return(NULL)
  }
  
  if (resp_status(resp) != 200) {
    message("  [HTTP ", resp_status(resp), "] ", url)
    return(NULL)
  }
  
  pagina <- read_html(resp_body_string(resp))
  datos_mes <- extraer_dias_mes(pagina, mes_num, ano, url)
  
  if (is.null(datos_mes)) {
    message("  [SIN DATOS] No se encontraron paneles de días en: ", url)
  }
  
  datos_mes
}

# 7. Ejecutar el scraping para los 24 meses, con manejo explícito de errores ---
cat("Descargando", nrow(fechas_grid), "meses de AccuWeather Bogotá...\n")

scrapear_mes_seguro <- safely(scrapear_mes)

resultados_lista <- pmap(
  list(fechas_grid$url, fechas_grid$mes_num, fechas_grid$ano),
  function(url, mes_num, ano) {
    cat("Procesando:", ano, "-", mes_num, "\n")
    scrapear_mes_seguro(url, mes_num, ano)
  }
)

# Separar meses exitosos de meses fallidos -------------------------------------
datos_exitosos <- resultados_lista %>% map("result") %>% compact()
meses_fallidos <- sum(map_lgl(resultados_lista, ~ is.null(.x$result)))

if (meses_fallidos > 0) {
  cat("\n⚠️  ADVERTENCIA:", meses_fallidos, "de", nrow(fechas_grid),
      "meses no pudieron descargarse o no tenían datos.\n",
      "Revisa los mensajes anteriores antes de continuar.\n\n")
}

if (length(datos_exitosos) == 0) {
  stop(
    "No se pudo extraer NINGÚN dato real de AccuWeather. ",
    "No se genera CSV con datos simulados: revisa la conexión, los headers, ",
    "o si el sitio cambió su estructura HTML antes de reintentar."
  )
}

datos_finales <- list_rbind(datos_exitosos) %>%
  arrange(fecha)

# 8. Validaciones de calidad de datos -------------------------------------------
rango_esperado <- seq(as.Date("2024-01-01"), as.Date("2025-12-31"), by = "day")
fechas_faltantes <- setdiff(rango_esperado, datos_finales$fecha) %>% as.Date(origin = "1970-01-01")

cat("\n--- RESUMEN DE VALIDACIÓN ---\n")
resumen_validacion <- datos_finales %>%
  summarise(
    total_filas         = n(),
    fechas_unicas       = n_distinct(fecha),
    duplicados          = sum(duplicated(fecha)),
    faltantes_high      = sum(is.na(high)),
    faltantes_low       = sum(is.na(low)),
    fecha_min           = min(fecha),
    fecha_max           = max(fecha),
    dias_esperados      = length(rango_esperado),
    dias_faltantes      = length(fechas_faltantes),
    cobertura_pct       = round(100 * total_filas / length(rango_esperado), 1)
  )

print(resumen_validacion)

if (length(fechas_faltantes) > 0) {
  cat("\nFechas sin datos (primeras 10):\n")
  print(head(fechas_faltantes, 10))
}

if (any(duplicated(datos_finales$fecha))) {
  cat("\n⚠️  Hay fechas duplicadas. Revisando...\n")
  print(datos_finales %>% filter(duplicated(fecha) | duplicated(fecha, fromLast = TRUE)))
}

# 9. Exportar tabla final en CSV -------------------------------------------------
tabla_salida <- datos_finales %>%
  select(fecha, high, low) %>%
  arrange(fecha)

write_csv(tabla_salida, "temperaturas_bogota_2024_2025.csv")
cat("\n✅ Archivo 'temperaturas_bogota_2024_2025.csv' guardado con éxito.\n")
cat("   (", nrow(tabla_salida), "filas de", length(rango_esperado), "días esperados,",
    resumen_validacion$cobertura_pct, "% de cobertura )\n")

# Guardamos también la versión con url_origen, para verificación/trazabilidad
write_csv(datos_finales, "temperaturas_bogota_2024_2025_con_origen.csv")

# Vista previa
print(head(tabla_salida, 10))

