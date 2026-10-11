library("tidyverse")

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

########

url_quotes <- "https://quotes.toscrape.com/"

# Leemos el HTML de la página
pagina_quotes <- read_html(url_quotes)
pagina_quotes

##########
pagina_quotes %>%
  html_element("title") %>%
  html_text2()

#######

citas <- pagina_quotes %>%
  html_elements(".quote .text") %>%
  html_text2()

citas

#######

autores <- pagina_quotes %>%
  html_elements(".quote .author") %>%
  html_text2()

autores

########

# Creamos la tabla
tabla_citas <- tibble(
  cita = citas,
  autor = autores
)

# Mostramos las citas en una tabla
mostrar_tabla(tabla_citas, n = 10, caption = "Primeras citas extraídas")