paquetes <- c(
  "rvest", "xml2", "dplyr", "stringr",
  "purrr", "janitor", "readr", "knitr"
)
# Verificamos qué paquetes faltan. La instalación se hace por fuera de la
# compilación para evitar cambios inesperados en el entorno del estudiante.
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  stop(
    "Faltan paquetes: ", paste(pendientes, collapse = ", "),
    ". Instálelos con install.packages(c(",
    paste(sprintf('"%s"', pendientes), collapse = ", "), "))"
  )
}

# Cargamos los paquetes sin mostrar mensajes
invisible(lapply(paquetes, library, character.only = TRUE))

url_quotes <- "https://www.accuweather.com/es/co/bogota/107487/august-weather/107487?year=2026"

user_agent_navegador <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
                              "AppleWebKit/537.36 (KHTML, like Gecko)",
                              "Chrome/122.0.0.0 Safari/537.36")

# Leemos el HTML de la página
pagina_quotes <- request(url_quotes) %>% req_user_agent(user_agent_navegador)
pagina_quotes








#############Pruebas#############################

pagina_quotes %>%
  html_element("title") %>%
  html_text2()

citas <- pagina_quotes %>%
  html_elements(".quote .text") %>%
  html_text2()

citas

autores <- pagina_quotes %>%
  html_elements(".quote .author") %>%
  html_text2()

autores

tabla_citas <- tibble(
  cita = citas,
  autor = autores
)

# Mostramos las citas en una tabla
# mostrar_tabla(tabla_citas, n = 10, caption = "Primeras citas extraídas")

nodos_citas <- pagina_quotes %>%
  html_elements(".quote")
length(nodos_citas)

tabla_citas_completa <- map_dfr(nodos_citas, function(nodo) {
  cita <- nodo %>% html_element(".text") %>% html_text2()
  autor <- nodo %>% html_element(".author") %>% html_text2()
  tags <- nodo %>% html_elements(".tags .tag") %>% html_text2()
  
  tibble(
    cita = cita,
    autor = autor,
    tags = paste(tags, collapse = ", ")
  )
})
