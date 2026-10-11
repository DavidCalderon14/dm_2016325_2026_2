## Taller práctico Web Scraping
## Mineria de datos
## Maria Camila Herrera Cortés

## Cargamos los paquetes necesarios
paquetes <- c(
  "rvest", "xml2", "dplyr", "stringr",
  "purrr", "janitor", "readr", "knitr"
)
# Verificamos qué paquetes faltan. La instalación se hace por fuera de la
# compilación para evitar cambios inesperados en el entorno del estudiante.
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(purrr)
library(janitor)
library(readr)
library(knitr)

if (length(pendientes) > 0) {
  stop(
    "Faltan paquetes: ", paste(pendientes, collapse = ", "),
    ". Instálelos con install.packages(c(",
    paste(sprintf('"%s"', pendientes), collapse = ", "), "))"
  )
}

# Cargamos los paquetes sin mostrar mensajes
invisible(lapply(paquetes, library, character.only = TRUE))

## Usaremos un sitio hecho para practicar scraping: Quotes to Scrape.
url_quotes <- "https://quotes.toscrape.com/"

# Leemos el HTML de la página
pagina_quotes <- read_html(url_quotes)
pagina_quotes

## Extraer el título de la página
pagina_quotes %>%
  html_element("title") %>%
  html_text2()

## Extraer todos los textos de las citas
citas <- pagina_quotes %>%
html_elements(".quote .text") %>%
  html_text2()

citas

## Extraer los autores
autores <- pagina_quotes %>%
  html_elements(".quote .author") %>%
  html_text2()

autores

## Construir una tabla para almacenar las citas
# Creamos la tabla
tabla_citas <- tibble(
  cita = citas,
  autor = autores
)

# Mostramos las citas en una tabla
# mostrar_tabla(tabla_citas, n = 10, caption = "Primeras citas extraídas")
print(tabla_citas, n = 10, caption = "Primeras citas extraídas")

## Ahora construiremos una tabla completa. Además del texto y el autor, 
## extraeremos las etiquetas asociadas a cada cita.
nodos_citas <- pagina_quotes %>%
  html_elements(".quote")
length(nodos_citas)

## Extraer información por bloque
## En vez de buscar cada dato por separado en toda la página, tomamos cada 
## bloque que representa una unidad lógica y extraemos sus partes internas.
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

## Limpieza básica del texto extraído
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

## Resumen
tabla_citas_limpia %>%
  count(autor, sort = TRUE) %>%
  print(n = 10, caption = "Autores con más citas")

## Navegar varias páginas
## Muchas veces la información no está en una sola URL. En este sitio hay 
## paginación. Vamos a scrapear varias páginas y unir los resultados.
## Crear vector de URLs

urls_paginas_quotes <- c(
  "https://quotes.toscrape.com/page/1/",
  "https://quotes.toscrape.com/page/2/",
  "https://quotes.toscrape.com/page/3/"
)

urls_paginas_quotes

## Creamos una función para scrapear una página
# Definimos una función que recibe la URL de una página
scrapear_quotes <- function(url) {
  # Leemos el HTML de la página
  pagina <- read_html(url)
  # Seleccionamos todos los bloques que contienen citas
  nodos <- pagina %>% html_elements(".quote")
  
  if (length(nodos) == 0) {
    stop("No se encontraron citas en: ", url)
  }
  
  # Recorremos cada bloque de cita y unimos los resultados en una tabla
  map_dfr(nodos, function(nodo) {
    # Extraemos el texto de la cita
    cita <- nodo %>% html_element(".text") %>% html_text2()
    # Extraemos el nombre del autor
    autor <- nodo %>% html_element(".author") %>% html_text2()
    # Extraemos todas las etiquetas asociadas a la cita
    tags <- nodo %>% html_elements(".tag") %>% html_text2()
    
    # Construimos una fila con la información extraída
    tibble(
      # Guardamos la URL de origen
      url_origen = url,
      # Limpiamos comillas y espacios del texto de la cita
      cita = str_squish(str_replace_all(cita, "[\u201c\u201d]", "")),
      # Limpiamos espacios extra en el nombre del autor
      autor = str_squish(autor),
      # Unimos las etiquetas en un solo texto separado por comas
      tags = paste(tags, collapse = ", ")
    )
  })
}

## Aplicar la función a varias páginas
quotes_tres_paginas <- map_dfr(urls_paginas_quotes, scrapear_quotes)
print(quotes_tres_paginas, n = 10, caption = "Resultados combinados de varias páginas")

## Validación básica
quotes_tres_paginas %>%
  summarise(
    filas = n(),
    autores_unicos = n_distinct(autor),
    citas_duplicadas = sum(duplicated(cita))
  ) %>%
  print(n = 10, caption = "Validación básica del scraping de citas")

## Scraping de un catálogo de libros
## Leer la página principal
url_books <- "https://books.toscrape.com/catalogue/page-1.html"
pagina_books <- read_html(url_books)
pagina_books

## Identificar los bloques de producto
nodos_libros <- pagina_books %>%
  html_elements("article.product_pod")
length(nodos_libros)

## Extraer datos de cada libro
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

print(libros_pagina_1, n = 10, caption = "Libros extraídos de una página")

## Limpieza del catálogo
## El scraping rara vez termina al extraer texto. Casi siempre 
## hace falta transformar variables. Vamos a crear entonces una 
## función que limpie los datos extraídos.
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
print(libros_pagina_1_limpia, n = 10, caption = "Libros con variables transformadas")

## Scrapear varias páginas del catálogo
urls_books <- paste0(
  "https://books.toscrape.com/catalogue/page-",
  1:3,
  ".html"
)

urls_books

## Función general para libros
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

## Ejecutamos scraping multi-página
libros_varias_paginas <- map_dfr(urls_books, scrapear_libros)
print(libros_varias_paginas, n = 10, caption = "Libros combinados de varias páginas")

## Conteo por rating
# Cuántos libros hay en cada categoría
libros_varias_paginas %>%
  count(rating, sort = TRUE) %>%
  print(n = 10, caption = "Conteo de libros por rating")

## Resumen de precios
libros_varias_paginas %>%
  summarise(
    n_libros        = n(),
    precio_promedio = mean(precio, na.rm = TRUE),
    precio_min      = min(precio, na.rm = TRUE),
    precio_max      = max(precio, na.rm = TRUE)
  ) %>%
  print(n = 10, caption = "Resumen de precios del catálogo")

## Extraer tablas HTML directamente
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

## Ver primera tabla
# Tomamos la primera tabla extraída y limpiamos sus nombres de columnas
tabla_ejemplo <- tablas[[1]] %>%
  janitor::clean_names()

# Mostramos la tabla con formato amigable para el reporte
print(tabla_ejemplo, n = 10, caption = "Ejemplo de tabla HTML extraída directamente")

## Buenas prácticas en scraping con R
# Calculamos un resumen rápido para validar la calidad del scraping
libros_varias_paginas %>%
  summarise(
    filas             = n(),
    titulos_unicos    = n_distinct(titulo),
    faltantes_precio  = sum(is.na(precio)),
    duplicados_titulo = sum(duplicated(titulo))
  )

## Respetar el sitio y limitar las solicitudes
# Recorremos cada URL del catálogo
for (u in urls_books) {
  # Hacemos una pausa de 1 segundo entre peticiones
  Sys.sleep(1)
  # Mostramos en consola la URL que se está procesando
  print(u)
  # Aquí iría la lectura de la página
  # read_html(u)
}

## Guardar resultados intermedios
# Guardamos las citas extraídas en un archivo CSV
write_csv(quotes_tres_paginas, "quotes_tres_paginas.csv")
# Guardamos los libros extraídos en otro archivo CSV
write_csv(libros_varias_paginas, "libros_varias_paginas.csv")

## Manejo básico de errores
# Capturamos tanto el resultado como el error para poder diagnosticarlo
scrapear_libros_seguro <- purrr::safely(scrapear_libros)
respuestas <- map(urls_books, scrapear_libros_seguro)

# Unimos las respuestas exitosas
resultado_seguro <- respuestas %>%
  map("result") %>%
  compact() %>%
  list_rbind()

# Registramos la URL y el mensaje de cada error
errores_scraping <- tibble(
  url = urls_books,
  error = map_chr(
    respuestas,
    ~ if (is.null(.x$error)) NA_character_ else conditionMessage(.x$error)
  )
) %>%
  filter(!is.na(error))

resultado_seguro

errores_scraping

## Ejercicio de clase
## Escribe un script que extraiga las temperaturas máxima y mínima 
## diarias de Bogotá para los años 2024 y 2025. Usa las páginas históricas
## de AccuWeather y expresa las temperaturas en grados Celsius.

urls_clima <- c(
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
