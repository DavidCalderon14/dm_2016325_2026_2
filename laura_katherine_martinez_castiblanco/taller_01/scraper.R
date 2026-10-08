# ==============================================================================
# MÓDULO DE WEB SCRAPING Y ADQUISICIÓN DE DATOS - TALLER 1
# Asignatura: Minería de Datos (KDD)
# Comercio: OXXO Domicilios Colombia
# Categoría: Bebidas (Excluye bebidas deportivas y soluciones de rehidratación)
# ==============================================================================

# 1. Carga de Librerías Requeridas
suppressPackageStartupMessages({
  library(httr2)
  library(rvest)
  library(jsonlite)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(DBI)
  library(RSQLite)
  library(lubridate)
})

# ==============================================================================
# 2. Funciones Auxiliares y Reglas de Negocio
# ==============================================================================

#' Evalúa si un producto pertenece a las exclusiones de la categoría "Bebidas"
#' Regla KDD: Excluye bebidas deportivas (Gatorade, Powerade) y sueros rehidratantes.
es_bebida_valida <- function(nombre_producto) {
  if (is.na(nombre_producto) || nombre_producto == "") return(FALSE)
  
  # Patrón con expresiones regulares de términos a EXCLUIR
  patron_exclusión <- "(?i)gatorade|powerade|squash|pedialyte|electrolit|hydraflex|suero|rehidratant|isotonic|deportiv"
  
  # Si detecta alguno de los términos excluidos, retorna FALSE
  return(!str_detect(nombre_producto, patron_exclusión))
}

#' Guarda la respuesta cruda en la carpeta raw/ para garantizar reproducibilidad offline
guardar_respuesta_raw <- function(contenido, comercio = "oxxo", extension = "json") {
  dir.create("raw", showWarnings = FALSE)
  timestamp <- format(Sys.time(), "%Y%m%m_%H%M%S")
  ruta_archivo <- file.path("raw", sprintf("%s_bebidas_%s.%s", comercio, timestamp, extension))
  
  if (extension == "json") {
    writeLines(toJSON(contenido, auto_unbox = TRUE, pretty = TRUE), ruta_archivo)
  } else {
    writeLines(as.character(contenido), ruta_archivo)
  }
  message(sprintf("Copia cruda guardada exitosamente en: %s", ruta_archivo))
}

# ==============================================================================
# 3. Función Principal de Scraping para OXXO Colombia
# ==============================================================================

#' Extrae el catálogo completo de Bebidas desde OXXO Domicilios
#' @param url_categoria URL oficial de la categoría de Bebidas en OXXO Colombia
#' @return tibble estandarizado con los productos crudos extraídos
scrapear_oxxo_bebidas <- function(url_categoria = "https://colombia.oxxodomicilios.com/search-products/bebidas/bebidas"){
  
  message("Iniciando extracción en OXXO Domicilios Colombia...")
  
  # Configuración del User-Agent idéntico a un navegador real
  user_agent_valido <- paste(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
    "AppleWebKit/537.36 (KHTML, like Gecko)",
    "Chrome/124.0.0.0 Safari/537.36"
  )
  
  # Captura defensiva de la respuesta HTTP
  respuesta_http <- tryCatch({
    request(url_categoria) %>%
      req_user_agent(user_agent_valido) %>%
      req_headers(
        `accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        `accept-language` = "es-ES,es;q=0.9"
      ) %>%
      req_timeout(seconds = 30) %>%
      req_perform()
  }, error = function(e) {
    message(sprintf("Error de conexión al consultar OXXO: %s", conditionMessage(e)))
    return(NULL)
  })
  
  if (is.null(respuesta_http) || resp_status(respuesta_http) >= 400) {
    message("No se pudo obtener una respuesta válida del servidor de OXXO.")
    return(tibble())
  }
  
  # Convertir el cuerpo a HTML analizable
  html_doc <- resp_body_html(respuesta_http)
  
  # Estrategia A: Extracción desde datos Next.js embebidos (__NEXT_DATA__)
  nodo_json <- html_doc %>% html_element("script#__NEXT_DATA__")
  
  productos_raw <- list()
  
  if (!is.null(nodo_json)){
    texto_json <- html_text(nodo_json)
    data_parsed <- fromJSON(texto_json, simplifyVector = FALSE)
    
    # Guardar copia cruda en raw/
    guardar_respuesta_raw(data_parsed, comercio = "oxxo", extension = "json")
    
    # Navegar dinámicamente el árbol JSON de Next.js
    tryCatch({
      # Ajustar la ruta del payload según la estructura del JSON interno de la app
      page_props <- data_parsed(props)pageProps
      if (!is.null(page_props$products)){
        productos_raw <- page_props$products
      }else if(!is.null(page_props(initialData)products)){
        productos_raw <- page_props(initialData)products
      }
    }, error = function(e) {
      message("No se pudo parsear directamente el objeto __NEXT_DATA__, intentando fallback HTML.")
    })
  }
  
  # Estrategia B: Fallback vía Selectores CSS si la estructura JSON no retorna elementos
  if (length(productos_raw) == 0) {
    nodos_productos <- html_doc %>% html_elements("[class*='product-card'], [class*='ProductCard']")
    
    if (length(nodos_productos) == 0) {
      message("⚠️ No se encontraron nodos de productos mediante HTML ni JSON.")
      return(tibble())
    }
    
    df_resultados <- map_dfr(nodos_productos, function(nodo) {
      nombre <- nodo %>% html_element("[class*='title'], h3, h2") %>% html_text2()
      precio_txt <- nodo %>% html_element("[class*='price']") %>% html_text2()
      
      tibble(
        comercio = "OXXO",
        fecha_captura = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        categoria_asignada = "Bebidas",
        producto_raw = str_squish(nombre),
        marca_raw = NA_character_,
        precio_raw = str_squish(precio_txt),
        presentacion_raw = NA_character_,
        url_producto = url_categoria
      )
    })
  } else{
    # Procesamiento a partir del JSON extraído
    df_resultados <- map_dfr(productos_raw, function(p){
      tibble(
        comercio = "OXXO",
        fecha_captura = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        categoria_asignada = "Bebidas",
        producto_raw = str_squish(p(name %||% p)title %||% NA_character_),
        marca_raw = str_squish(p$brand %||% NA_character_),
        precio_raw = as.character(p(price %||% p)finalPrice %||% NA_character_),
        presentacion_raw = str_squish(p(description %||% p)grammage %||% NA_character_),
        url_producto = paste0("https://colombia.oxxodomicilios.com/product/", p$slug %||% "")
      )
    })
  }
  
  # Aplicación de la Regla de Negocio (Filtrado de Exclusiones KDD)
  df_filtrado <- df_resultados %>%
    filter(!is.na(producto_raw), producto_raw != "") %>%
    filter(map_lgl(producto_raw, es_bebida_valida))
  
  message(sprintf("craping finalizado en OXXO: %d productos recolectados (%d excluidos por reglas de negocio).", 
                  nrow(df_filtrado), nrow(df_resultados) - nrow(df_filtrado)))
  
  # Pausa respetuosa para evitar sobrecarga
  Sys.sleep(2)
  
  return(df_filtrado)
}

# ==============================================================================
# 4. Orquestación e Inserción en SQLite
# ==============================================================================

#' Ejecuta la extracción global e inserta las observaciones en la base SQLite
ejecutar_recoleccion_completa <- function(db_path = "comparador_precios.sqlite"){
  
  # 1. Ejecutar extractor de OXXO
  datos_oxxo <- scrapear_oxxo_bebidas()
  
  if (nrow(datos_oxxo) == 0){
    message("No se obtuvieron registros para guardar en SQLite.")
    return(NULL)
  }
  
  # 2. Conectar a SQLite
  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  # 3. Registrar la Ejecución (Tabla Meta)
  id_ejecucion <- as.numeric(Sys.time())
  df_ejecucion <- tibble(
    id_ejecucion = id_ejecucion,
    fecha_hora = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    comercio = "OXXO",
    registros_extraidos = nrow(datos_oxxo)
  )
  
  dbWriteTable(con, "ejecuciones", df_ejecucion, append = TRUE)
  
  # 4. Asignar ID de ejecución a los productos e insertar sin sobrescribir
  datos_oxxo <- datos_oxxo %>% mutate(id_ejecucion = id_ejecucion)
  dbWriteTable(con, "productos_raw", datos_oxxo, append = TRUE)
  
  message("Datos consolidados e insertados exitosamente en SQLite.")
}

