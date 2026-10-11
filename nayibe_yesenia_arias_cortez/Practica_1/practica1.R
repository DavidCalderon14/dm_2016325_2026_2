# CARGA DE PAQUETES NECESARIOS ------------

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



# pagina ejemplo para trabajar -----------------++++-

url_quotes <- "https://quotes.toscrape.com/"

# Leemos el HTML de la página
pagina_quotes <- read_html(url_quotes)
pagina_quotes


# EXTRAE TITULO PAGINA -------------------------
pagina_quotes %>%
  html_element("title") %>%
  html_text2()

# EXTRAER TODOS LOS TEXTOS DE CITAS------------
citas <- pagina_quotes %>%
  html_elements(".quote .text") %>%
  html_text2()

citas

# EXTRAE AUTORES---------------------------------
autores <- pagina_quotes %>%
  html_elements(".quote .author") %>%
  html_text2()

autores


# CREAR TABLA PARA COLOCAR CITAS-------------------
# Creamos la tabla
tabla_citas <- tibble(
  cita = citas,
  autor = autores
)

# Mostramos las citas en una tabla
print(tabla_citas, n = 10, caption = "Primeras citas extraídas")


# CITAS, AUTORES Y ETIQUETAS ------------------------
nodos_citas <- pagina_quotes %>%
  html_elements(".quote")
length(nodos_citas)

# EXTRAER INFORMACION DEL BLOQUE -----------------------
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

print(tabla_citas_completa, n = 10, caption = "Citas, autores y etiquetas")

#LIMPIEZA BASICA DEL TEXTO EXTRAIDO--------------------------------
tabla_citas_limpia <- tabla_citas_completa %>%
  mutate(
    # Quitamos las comillas tipográficas de la cita
    cita = str_replace_all(cita, "[\u201c\u201d]", ""),
    # Eliminamos espacios sobrantes en el texto de la cita
    cita = str_squish(cita),
    # Limpiamos espacios innecesarios en el nombre del autor
    autor = str_squish(autor),
    # Calculamos cuántas etiquetas tiene cada cita
    n_tags = if_else(tags == "", 0L, str_count(tags, ",") + 1L)
  )

print(tabla_citas_limpia, n = 10, caption = "Citas limpias con número de etiquetas")

# RESUMEN RAPIDO----------------------------------------------------

tabla_citas_limpia %>%
  count(autor, sort = TRUE) %>%
  mostrar_tabla(n = 10, caption = "Autores con más citas")

#-----------------------Scraping de un catálogo de libros---------------------------

## Leer la página principal------------------------
url_books <- "https://books.toscrape.com/catalogue/page-1.html"
pagina_books <- read_html(url_books)
pagina_books


#Identificar los bloques de producto--------------------
nodos_libros <- pagina_books %>%
  html_elements("article.product_pod")
length(nodos_libros)


# Extraer datos de cada libro-----------------------------

# Recorremos cada bloque de libro y unimos los resultados en una tabla
libros_pagina_1 <- map_dfr(nodos_libros, function(nodo) {
  # Extraemos el título del libro desde el atributo title del enlace
  titulo <- nodo %>%
    html_element("h3 a") %>%
    html_attr("title")
  
  # Extraemos el precio como texto
  precio <- nodo %>%
    html_element(".price_color") %>%
    html_text2()
  
  # Extraemos el texto de disponibilidad del libro
  disponibilidad <- nodo %>%
    html_element(".availability") %>%
    html_text2()
  
  # Extraemos la clase CSS donde está codificado el rating
  rating_clase <- nodo %>%
    html_element("p.star-rating") %>%
    html_attr("class")
  
  # Extraemos el enlace relativo hacia el detalle del libro
  enlace_relativo <- nodo %>%
    html_element("h3 a") %>%
    html_attr("href")
  
  # Construimos una fila con las variables extraídas
  tibble(
    titulo          = titulo,
    precio_texto    = precio,
    disponibilidad  = disponibilidad,
    rating_raw      = rating_clase,
    enlace_relativo = enlace_relativo
  )
})

mostrar_tabla(libros_pagina_1, n = 10, caption = "Libros extraídos de una página")



#Limpieza del catálogo--------------------------------------------------


# Definimos una función para limpiar la clase CSS del rating
limpiar_rating <- function(x) {
  # Quitamos el prefijo "star-rating" y dejamos solo la categoría
  x %>%
    str_remove("star-rating\\s+")
}

# Transformamos las variables extraídas a un formato más útil para análisis
libros_pagina_1_limpia <- libros_pagina_1 %>%
  mutate(
    # Convertimos el precio de texto a número
    precio = precio_texto %>%
      str_replace_all("[^0-9\\.]", "") %>%
      as.numeric(),
    
    # Limpiamos la clase del rating para dejar solo su valor
    rating = limpiar_rating(rating_raw),
    
    # Creamos una variable lógica que indica si el libro está disponible
    disponible = str_detect(str_to_lower(disponibilidad), "in stock"),
    
    # Construimos el enlace completo hacia el detalle del libro
    enlace = paste0("https://books.toscrape.com/catalogue/", enlace_relativo)) %>%
  
  # Seleccionamos las variables finales que queremos conservar
  select(titulo, precio, disponibilidad, disponible, rating, enlace)

# Mostramos la tabla con las variables ya transformadas
mostrar_tabla(libros_pagina_1_limpia, n = 10, caption = "Libros con variables transformadas")


#---Scrapear varias páginas del catálogo----------------------------
##Vamos a automatizar la extracción en varias páginas.

urls_books <- paste0(
  "https://books.toscrape.com/catalogue/page-",
  1:3,
  ".html"
)

urls_books



#FUNCION GENERAL PARA LIBROS-------------------------------------------


scrapear_libros <- function(url) {
  
  # Leemos el HTML de la página indicada
  pagina <- read_html(url)
  
  # Seleccionamos todos los bloques que representan libros
  nodos  <- pagina %>%
    html_elements("article.product_pod")
  
  if (length(nodos) == 0) {
    stop("No se encontraron libros en: ", url)
  }
  
  # Recorremos cada bloque y unimos los resultados en una sola tabla
  map_dfr(nodos, function(nodo) {
    # Extraemos el título del libro desde el atributo title
    titulo <- nodo %>%
      html_element("h3 a") %>%
      html_attr("title")
    
    # Extraemos el precio tal como aparece en la página
    precio <- nodo %>%
      html_element(".price_color") %>%
      html_text2()
    
    # Extraemos el texto de disponibilidad
    disponibilidad <- nodo %>%
      html_element(".availability") %>%
      html_text2()
    # Extraemos la clase CSS que contiene la información del rating
    rating_raw <- nodo %>%
      html_element("p.star-rating") %>%
      html_attr("class")
    
    # Extraemos el enlace relativo hacia la página de detalle
    enlace_relativo <- nodo %>%
      html_element("h3 a") %>%
      html_attr("href")
    
    # Construimos una fila con los datos limpios y listos para analizar
    tibble(
      url_origen     = url,
      titulo         = titulo,
      precio         = precio %>% str_replace_all("[^0-9\\.]", "") %>% as.numeric(),
      disponibilidad = str_squish(disponibilidad),
      rating         = rating_raw %>% str_remove("star-rating\\s+"),
      enlace         = paste0("https://books.toscrape.com/catalogue/", enlace_relativo)
    )
  })
}

#EJECUTAMOS SCRAPING MULTI PAGINA-----------------------

libros_varias_paginas <- map_dfr(urls_books, scrapear_libros)
mostrar_tabla(libros_varias_paginas, n = 10, caption = "Libros combinados de varias páginas")


#CONTEO POR RATING---------------------------------------
# Cuántos libros hay en cada categoría
libros_varias_paginas %>%
  count(rating, sort = TRUE) %>%
  mostrar_tabla(n = 10, caption = "Conteo de libros por rating")

# Resumen de precios ------------------------------------
libros_varias_paginas %>%
  summarise(
    n_libros        = n(),
    precio_promedio = mean(precio, na.rm = TRUE),
    precio_min      = min(precio, na.rm = TRUE),
    precio_max      = max(precio, na.rm = TRUE)
  ) %>%
  mostrar_tabla(n = 10, caption = "Resumen de precios del catálogo")

# --- Extraer tablas HTML directamente-----------------------------

# Definimos la URL de una página que contiene tablas HTML
url_tabla <- "https://www.w3schools.com/html/html_tables.asp"
# Leemos el HTML de la página
pagina_tabla <- read_html(url_tabla)

# Extraemos todas las tablas HTML y las convertimos en data frames
tablas <- pagina_tabla %>%
  html_table(fill = TRUE)

if (length(tablas) == 0) {
  stop("No se encontraron tablas en: ", url_tabla)
}
# Contamos cuántas tablas fueron encontradas en la página
length(tablas)

# VER PRIMER TABLA --------------------------------------

# Tomamos la primera tabla extraída y limpiamos sus nombres de columnas
tabla_ejemplo <- tablas[[1]] %>%
  janitor::clean_names()

# Mostramos la tabla con formato amigable para el reporte
mostrar_tabla(tabla_ejemplo, n = 10, caption = "Ejemplo de tabla HTML extraída directamente")


# 

