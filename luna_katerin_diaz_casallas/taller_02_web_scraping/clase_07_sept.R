paquetes <- c(
  "rvest", "xml2", "httr2", "dplyr", "stringr",
  "purrr", "tibble", "janitor", "readr", "knitr", "chromote"
)

instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  tryCatch(
    install.packages(pendientes),
    error = function(e) stop(
      "No fue posible instalar: ", paste(pendientes, collapse = ", "),
      ". Solicite apoyo al docente. Detalle: ", conditionMessage(e)
    )
  )
}

if (requireNamespace("rvest", quietly = TRUE) && packageVersion("rvest") < "1.0.5") {
  install.packages("rvest")
}

invisible(lapply(paquetes, library, character.only = TRUE))

##############################################################################

mostrar_tabla <- function(x, n = 10, caption = NULL) {
  x_mostrar <- head(x, n)
  
  tabla_html <- knitr::kable(
    x_mostrar,
    format = "html",
    caption = caption,
    table.attr = 'class="table table-striped table-hover table-condensed"'
  )
  
  knitr::asis_output(
    paste0(
      '<div class="table-scroll">',
      as.character(tabla_html),
      '</div>'
    )
  )
}

diagnostico <- tibble(
  elemento = c("Sistema operativo", "Versión de R", "Versión de rvest", "Versión de chromote"),
  valor = c(
    Sys.info()[["sysname"]],
    R.version.string,
    as.character(packageVersion("rvest")),
    as.character(packageVersion("chromote"))
  )
)

mostrar_tabla(diagnostico, n = nrow(diagnostico), caption = "Entorno de ejecución")

###################TERMINO CERVEZA

termino_busqueda <- "cerveza"

url_exito <- paste0("https://www.exito.com/s?q=", URLencode(termino_busqueda))
url_exito

url_carulla <- paste0("https://www.carulla.com/s?q=", URLencode(termino_busqueda))
url_carulla

url_jumbo <- paste0("https://www.jumbocolombia.com/search?query=cerveza&type=term")
url_jumbo

pagina_simple_exito <- tryCatch(
  read_html(url_exito),
  error = function(e) NULL
)

pagina_simple_carulla <- tryCatch(
  read_html(url_carulla),
  error = function(e) NULL
)

pagina_simple_jumbo <- tryCatch(
  read_html(url_jumbo),
  error = function(e) NULL
)

if (is.null(pagina_simple_carulla)) {
  "La lectura directa falló, pero el taller puede continuar."
} else {
  pagina_simple_carulla
}

if (is.null(pagina_simple_exito)) {
  "La lectura directa falló, pero el taller puede continuar."
} else {
  pagina_simple_exito
}

if (is.null(pagina_simple_jumbo)) {
  "La lectura directa falló, pero el taller puede continuar."
} else {
  pagina_simple_jumbo
}

construir_url_exito <- function(termino, pagina = 0) {
  paste0(
    "https://www.exito.com/s?q=", URLencode(termino),
    "&sort=score_desc&page=", pagina
  )
}

tabla_productos_vacia <- function() {
  tibble(
    titulo = character(),
    enlace = character(),
    precio = character(),
    rating = character(),
    n_resenas = character(),
    pagina = integer()
  )
}

# Definimos una función que recibe el nodo HTML de un producto
extraer_producto_exito <- function(nodo) {
  # Extraemos el título visible del producto
  titulo <- nodo %>%
    html_element(".styles_name__qQJiK") %>%
    html_text2()
  
  # Extraemos el enlace relativo hacia la página del producto
  enlace_relativo <- nodo %>%
    html_element("h2 a") %>%
    html_attr("href")
  
  # Extraemos el precio mostrado en el resultado de búsqueda
  precio <- nodo %>%
    html_element(".a-price .a-offscreen") %>%
    html_text2()
  
  # Extraemos el texto de la valoración, por ejemplo '4.5 out of 5 stars'
  rating <- nodo %>%
    html_element(".a-icon-alt") %>%
    html_text2()
  
  # Extraemos el número de reseñas o valoraciones asociadas al producto
  n_resenas <- nodo %>%
    html_element("[aria-label$='ratings'], .s-link-style .s-underline-text") %>%
    html_text2()
  
  # Construimos una fila con los campos extraídos para este producto
  tibble(
    titulo = titulo,
    # Si el enlace no existe, dejamos un valor faltante; si existe, construimos la URL completa
    enlace = ifelse(
      is.na(enlace_relativo),
      NA_character_,
      xml2::url_absolute(enlace_relativo, "https://www.amazon.com")
    ),
    precio = precio,
    rating = rating,
    n_resenas = n_resenas
  )
}


scrapear_pagina_exito <- function(termino, pagina = 1, pausa = 8) {
  # Construimos la URL de búsqueda usando el término y el número de página
  url <- construir_url_exito(termino, pagina)
  
  # Introducimos una pausa para no hacer solicitudes demasiado seguidas
  Sys.sleep(pausa)
  
  # Enviamos la solicitud HTTP usando un user-agent y
  # encabezados similares a los del navegador
  respuesta <- tryCatch(
    request(url) %>%
   #   req_user_agent(user_agent_navegador) %>%
      req_headers(`accept-language` = "es-CO") %>%
      req_timeout(seconds = 30) %>%
      req_error(is_error = function(resp) FALSE) %>%
      req_perform(),
    error = function(e) {
      message("Página ", pagina, ": ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(respuesta) || resp_status(respuesta) >= 400) {
    message("Página ", pagina, ": no se obtuvo una respuesta utilizable.")
    return(tabla_productos_vacia())
  }
  
  # Convertimos la respuesta recibida en un documento HTML analizable
  pagina_html <- tryCatch(resp_body_html(respuesta), error = function(e) NULL)
  
  if (is.null(pagina_html)) {
    return(tabla_productos_vacia())
  }
  
  # Seleccionamos los nodos que corresponden a productos 
  #reales en la página de resultados
  nodos <- pagina_html %>%
    html_elements(".layout__content[data-component-type=class='productCard_productCard__M0677 productCard_column__Lp3OF']")
  
  if (length(nodos) == 0) {
    message("Página ", pagina, ": no se encontraron productos.")
    return(tabla_productos_vacia())
  }
  
  # Extraemos la información de cada producto, limpiamos nombres 
  #de variables y añadimos la página de origen
  map_dfr(nodos, extraer_producto_exito) %>%
    clean_names() %>%
    mutate(pagina = pagina)
}
