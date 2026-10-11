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

diagnostico <- tibble(
  elemento = c("Sistema operativo", "Versión de R", "Versión de rvest", "Versión de chromote"),
  valor = c(
    Sys.info()[["sysname"]],
    R.version.string,
    as.character(packageVersion("rvest")),
    as.character(packageVersion("chromote"))
  )
)

####TERMINO CERVEZA

####Construir URL
construir_url_exito <- function(termino, pagina = 1) {
  paste0(
    "https://www.exito.com/s?q=", URLencode(termino),
    "&page=", pagina
  )
}

construir_url_carulla <- function(termino, pagina = 1) {
  paste0(
    "https://www.carulla.com/s?q=", URLencode(termino),
    "&page=", pagina
  )
}

construir_url_jumbo <- function(termino, pagina = 1) {
  paste0(
    "https://www.jumbocolombia.com", URLencode(termino),
    "&page=", pagina
  )
}

##### Termino de busqueda 
termino_busqueda <- "cervezas&sort"
url_exito <- paste0("https://www.exito.com/s?q=", URLencode(termino_busqueda))
url_exito

pagina_simple_exito <- tryCatch(
  read_html(url_exito),
  error = function(e) NULL
)


termino_busqueda <- "cerveza"
url_carulla <- paste0("https://www.carulla.com/s?q=", URLencode(termino_busqueda))
url_carulla

pagina_simple_carulla <- tryCatch(
  read_html(url_carulla),
  error = function(e) NULL
)


termino_busqueda <- "cerveza&type"
url_jumbo <- paste0("https://www.jumbocolombia.com", URLencode(termino_busqueda))
url_jumbo

pagina_simple_jumbo <- tryCatch(
  read_html(url_jumbo),
  error = function(e) NULL
)

####Pag simple

pagina_simple <- tryCatch(
  read_html(url_exito),
  error = function(e) NULL
)

if (is.null(pagina_simple)) {
  "La lectura directa falló, pero el taller puede continuar."
} else {
  pagina_simple
}


# Definimos un user-agent similar al de un navegador real
user_agent_navegador <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)

# Construimos la solicitud HTTP y añadimos encabezados para 
# que parezca una visita normal
respuesta_exito <- tryCatch(
  request(url_exito) %>%
    req_user_agent(user_agent_navegador) %>%
    req_headers(`accept-language` = "en-US,en;q=0.9") %>%
    req_timeout(seconds = 30) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform(),
  error = function(e) NULL
)

estado_http <- if (is.null(respuesta_exito)) NA_integer_ else resp_status(respuesta_exito)

# Convertimos el cuerpo de la respuesta en un documento HTML
# analizable con rvest/xml2
pagina_exito <- tryCatch(
  if (is.null(respuesta_exito)) stop("No hubo respuesta") else resp_body_html(respuesta_exito),
  error = function(e) read_html("<html><head><title>Sin respuesta</title></head><body></body></html>")
)

titulo_respuesta <- pagina_exito %>%
  html_element("title") %>%
  html_text2()

posible_bloqueo <- is.na(estado_http) || estado_http >= 400 ||
  str_detect(str_to_lower(titulo_respuesta), "captcha|robot check|sorry|verific|sin respuesta")

tibble(
  estado_http = estado_http,
  titulo_pagina = titulo_respuesta,
  posible_bloqueo = posible_bloqueo
)

