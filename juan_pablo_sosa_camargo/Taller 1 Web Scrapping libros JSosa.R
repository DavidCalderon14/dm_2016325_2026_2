paquetes <- c(
  "rvest",
  "xml2",
  "dplyr",
  "stringr",
  "purrr",
  "janitor",
  "readr",
  "knitr"
)

paquetes_disponibles <- rownames(installed.packages())
paquetes_pendientes <- setdiff(paquetes, paquetes_disponibles)

if (length(paquetes_pendientes) > 0) {
  stop(
    "Faltan los siguientes paquetes: ",
    paste(paquetes_pendientes, collapse = ", "),
    "\nInstálelos con:\n",
    "install.packages(c(",
    paste(sprintf('"%s"', paquetes_pendientes), collapse = ", "),
    "))"
  )
}

invisible(
  lapply(paquetes, library, character.only = TRUE)
)


# ============================================================
# CITAS
# ============================================================

url_citas <- "https://quotes.toscrape.com/"

pagina_citas <- read_html(url_citas)

titulo <- pagina_citas %>%
  html_element("title") %>%
  html_text2()

titulo


citas_extraidas <- pagina_citas %>%
  html_elements(".quote .text") %>%
  html_text2()

citas_extraidas


autores_extraidos <- pagina_citas %>%
  html_elements(".quote .author") %>%
  html_text2()

autores_extraidos


tabla_citas <- tibble(
  cita = citas_extraidas,
  autor = autores_extraidos
)

knitr::kable(
  head(tabla_citas, 10),
  caption = "Citas y autores extraídos"
)


bloques_citas <- pagina_citas %>%
  html_elements(".quote")

length(bloques_citas)


datos_citas <- map_dfr(
  bloques_citas,
  function(bloque) {
    
    texto_cita <- bloque %>%
      html_element(".text") %>%
      html_text2()
    
    nombre_autor <- bloque %>%
      html_element(".author") %>%
      html_text2()
    
    etiquetas <- bloque %>%
      html_elements(".tags .tag") %>%
      html_text2()
    
    tibble(
      cita = texto_cita,
      autor = nombre_autor,
      etiquetas = paste(etiquetas, collapse = ", ")
    )
  }
)

knitr::kable(
  head(datos_citas, 10),
  caption = "Citas, autores y etiquetas"
)


datos_citas_limpios <- datos_citas %>%
  mutate(
    cita = str_replace_all(cita, "[\u201c\u201d]", ""),
    cita = str_squish(cita),
    autor = str_squish(autor),
    cantidad_etiquetas = if_else(
      etiquetas == "",
      0L,
      str_count(etiquetas, ",") + 1L
    )
  )

knitr::kable(
  head(datos_citas_limpios, 10),
  caption = "Citas después de la limpieza"
)


resumen_autores <- datos_citas_limpios %>%
  count(autor, sort = TRUE)

knitr::kable(
  head(resumen_autores, 10),
  caption = "Autores con mayor número de citas"
)


# ============================================================
# CATÁLOGO DE LIBROS
# ============================================================

url_libros <- "https://books.toscrape.com/catalogue/page-1.html"

pagina_libros <- read_html(url_libros)

bloques_libros <- pagina_libros %>%
  html_elements("article.product_pod")

length(bloques_libros)


libros_primera_pagina <- map_dfr(
  bloques_libros,
  function(bloque) {
    
    nombre_libro <- bloque %>%
      html_element("h3 a") %>%
      html_attr("title")
    
    precio_libro <- bloque %>%
      html_element(".price_color") %>%
      html_text2()
    
    estado_disponibilidad <- bloque %>%
      html_element(".availability") %>%
      html_text2()
    
    categoria_rating <- bloque %>%
      html_element("p.star-rating") %>%
      html_attr("class")
    
    enlace_libro <- bloque %>%
      html_element("h3 a") %>%
      html_attr("href")
    
    tibble(
      nombre = nombre_libro,
      precio_texto = precio_libro,
      disponibilidad = estado_disponibilidad,
      rating_texto = categoria_rating,
      enlace_relativo = enlace_libro
    )
  }
)

knitr::kable(
  head(libros_primera_pagina, 10),
  caption = "Libros extraídos de la primera página"
)


libros_limpios <- libros_primera_pagina %>%
  mutate(
    precio = precio_texto %>%
      str_replace_all("[^0-9.]", "") %>%
      as.numeric(),
    
    rating = rating_texto %>%
      str_remove("^star-rating\\s+"),
    
    disponible = str_detect(
      str_to_lower(disponibilidad),
      "in stock"
    ),
    
    enlace = xml2::url_absolute(
      enlace_relativo,
      url_libros
    )
  ) %>%
  select(
    nombre,
    precio,
    disponibilidad,
    disponible,
    rating,
    enlace
  )

knitr::kable(
  head(libros_limpios, 10),
  caption = "Catálogo de libros después de la limpieza"
)


# ============================================================
# MÚLTIPLES PÁGINAS
# ============================================================

paginas_catalogo <- paste0(
  "https://books.toscrape.com/catalogue/page-",
  1:3,
  ".html"
)

paginas_catalogo


extraer_libros <- function(url_pagina) {
  
  contenido_html <- read_html(url_pagina)
  
  bloques <- contenido_html %>%
    html_elements("article.product_pod")
  
  if (length(bloques) == 0) {
    warning("No se encontraron libros en: ", url_pagina)
    return(tibble())
  }
  
  map_dfr(
    bloques,
    function(bloque) {
      
      nombre_libro <- bloque %>%
        html_element("h3 a") %>%
        html_attr("title")
      
      precio_libro <- bloque %>%
        html_element(".price_color") %>%
        html_text2()
      
      estado_disponibilidad <- bloque %>%
        html_element(".availability") %>%
        html_text2()
      
      categoria_rating <- bloque %>%
        html_element("p.star-rating") %>%
        html_attr("class")
      
      enlace_relativo <- bloque %>%
        html_element("h3 a") %>%
        html_attr("href")
      
      tibble(
        pagina_origen = url_pagina,
        nombre = nombre_libro,
        precio = precio_libro %>%
          str_replace_all("[^0-9.]", "") %>%
          as.numeric(),
        disponibilidad = estado_disponibilidad %>%
          str_squish(),
        disponible = str_detect(
          str_to_lower(estado_disponibilidad),
          "in stock"
        ),
        rating = categoria_rating %>%
          str_remove("^star-rating\\s+"),
        enlace = xml2::url_absolute(
          enlace_relativo,
          url_pagina
        )
      )
    }
  )
}


catalogo_completo <- map_dfr(
  paginas_catalogo,
  extraer_libros
)

knitr::kable(
  head(catalogo_completo, 10),
  caption = "Libros extraídos de múltiples páginas"
)


# ============================================================
# ANÁLISIS DEL CATÁLOGO
# ============================================================

libros_por_rating <- catalogo_completo %>%
  count(rating, sort = TRUE)

knitr::kable(
  libros_por_rating,
  caption = "Cantidad de libros por rating"
)


resumen_precios <- catalogo_completo %>%
  summarise(
    cantidad_libros = n(),
    precio_promedio = mean(precio, na.rm = TRUE),
    precio_minimo = min(precio, na.rm = TRUE),
    precio_maximo = max(precio, na.rm = TRUE)
  )

knitr::kable(
  resumen_precios,
  caption = "Resumen de precios del catálogo"
)


# ============================================================
# EXTRACCIÓN DE TABLAS HTML
# ============================================================

url_tabla_html <- "https://www.w3schools.com/html/html_tables.asp"

pagina_tabla_html <- read_html(url_tabla_html)

tablas_html <- pagina_tabla_html %>%
  html_table(fill = TRUE)

if (length(tablas_html) == 0) {
  stop("No se encontraron tablas en: ", url_tabla_html)
}

length(tablas_html)


tabla_principal <- tablas_html[[1]] %>%
  janitor::clean_names()

knitr::kable(
  tabla_principal,
  caption = "Tabla HTML extraída de la página"
)
