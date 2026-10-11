# ============================================
# Cristian Camilo Prieto Zambrano
# Taller 01
# Scraper AccuWeather - Temperaturas diarias 2025
# Bogotá
# ============================================

library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(httr2)
library(lubridate)

url_2025 <- c(
  "https://www.accuweather.com/es/co/bogota/107487/january-weather/107487?year=2025",
  "https://www.accuweather.com/es/co/bogota/107487/february-weather/107487?year=2025",
  "https://www.accuweather.com/es/co/bogota/107487/march-weather/107487?year=2025",
  "https://www.accuweather.com/es/co/bogota/107487/april-weather/107487?year=2025",
  "https://www.accuweather.com/es/co/bogota/107487/may-weather/107487?year=2025",
  "https://www.accuweather.com/es/co/bogota/107487/june-weather/107487?year=2025",
  "https://www.accuweather.com/es/co/bogota/107487/july-weather/107487?year=2025",
  "https://www.accuweather.com/es/co/bogota/107487/august-weather/107487?year=2025",
  "https://www.accuweather.com/es/co/bogota/107487/september-weather/107487?year=2025",
  "https://www.accuweather.com/es/co/bogota/107487/october-weather/107487?year=2025",
  "https://www.accuweather.com/es/co/bogota/107487/november-weather/107487?year=2025",
  "https://www.accuweather.com/es/co/bogota/107487/december-weather/107487?year=2025"
)

user_agent_navegador <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)

obtener_html <- function(url, tiempo_max = 15) {
  
  respuesta <- httr2::request(url) %>%
    httr2::req_user_agent(user_agent_navegador) %>%
    httr2::req_headers(`accept-language` = "en-US,en;q=0.9") %>%
    httr2::req_timeout(tiempo_max) %>%
    httr2::req_perform()
  
  if (httr2::resp_status(respuesta) != 200) {
    warning("Status ", httr2::resp_status(respuesta), " en: ", url)
  }
  
  respuesta %>% httr2::resp_body_html()
}

diagnosticar <- function(url) {
  t0 <- Sys.time()
  pagina <- obtener_html(url)
  nodos <- pagina %>% html_elements("a.monthly-daypanel")
  cat(
    url, "->", length(nodos), "day-panels encontrados",
    "(", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "seg )\n"
  )
  nodos
}

scrapear_AccuWeather <- function(url) {
  
  pagina <- obtener_html(url)
  dias <- pagina %>% html_elements("a.monthly-daypanel")
  
  if (length(dias) == 0) {
    warning("No se encontraron day-panels en: ", url)
    return(tibble())
  }
  
  tibble(
    url = url,
    href = dias %>% html_attr("href"),
    texto_completo = dias %>% html_text2()
  )
}

paginas_AW <- map_dfr(url_2025, scrapear_AccuWeather)

meses_en <- c(
  "january", "february", "march", "april", "may", "june",
  "july", "august", "september", "october", "november", "december"
)

limpiar_calendario <- function(paginas_AW) {
  
  paginas_AW %>%
    mutate(
      mes_texto = str_extract(url, paste(meses_en, collapse = "|")),
      mes = match(mes_texto, meses_en),
      anio = str_extract(url, "(?<=year=)\\d{4}") %>% as.integer()
    ) %>%
    group_by(url) %>%
    mutate(
      posicion = row_number(),
      primer_dia = as.Date(sprintf("%d-%02d-01", anio, mes)),
      relleno_inicial = as.integer(format(primer_dia, "%w")),
      dias_mes = lubridate::days_in_month(primer_dia)
    ) %>%
    filter(
      posicion > relleno_inicial,
      posicion <= relleno_inicial + dias_mes
    ) %>%
    ungroup() %>%
    mutate(
      dia = posicion - relleno_inicial,
      fecha = as.Date(sprintf("%d-%02d-%02d", anio, mes, dia)),
      numeros = str_extract_all(texto_completo, "-?\\d+"),
      dia_texto = map_dbl(numeros, ~ as.numeric(.x[1])),
      tmax = map_dbl(numeros, ~ as.numeric(.x[2])),
      tmin = map_dbl(numeros, ~ as.numeric(.x[3]))
    ) %>%
    select(fecha, tmax, tmin, dia, dia_texto, texto_completo)
}

clima_2025 <- limpiar_calendario(paginas_AW)

clima_2025 %>% 
  filter(dia != dia_texto)

clima_2025$texto_completo[1:5]

nrow(clima_2025)

clima_2025_final <- clima_2025 %>% 
  select(fecha, tmax, tmin)

saveRDS(clima_2025_final, "clima_bogota_2025.rds")

write.csv(
  clima_2025_final,
  "clima_bogota_2025.csv",
  row.names = FALSE
)

