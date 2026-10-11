library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(readr)

# ------------------------------------------------------------------------------
# PASO 1: DEFINICIÓN DE MARCAS Y PARÁMETROS
# ------------------------------------------------------------------------------
marcas_cerveza <- c("michelob", "stella artois", "club colombia")
supermercados <- c("exito", "carulla", "jumbo")

# Selectores CSS actualizados y más robustos para VTEX (Éxito, Carulla, Jumbo)
selectores <- list(
  exito = list(
    contenedor = "[class*='galleryItem'], [class*='productSummaryContainer'], article",
    titulo     = "[class*='productName'], [class*='productBrand'], h3, h2",
    precio     = "[class*='currencyContainer'], [class*='sellingPrice'], [class*='Value'], [class*='price']"
  ),
  carulla = list(
    contenedor = "[class*='galleryItem'], [class*='productSummaryContainer'], article",
    titulo     = "[class*='productName'], [class*='productBrand'], h3, h2",
    precio     = "[class*='currencyContainer'], [class*='sellingPrice'], [class*='Value'], [class*='price']"
  ),
  jumbo = list(
    contenedor = "[class*='galleryItem'], [class*='productSummaryContainer'], article",
    titulo     = "[class*='productName'], [class*='productBrand'], h3, h2",
    precio     = "[class*='currencyContainer'], [class*='sellingPrice'], [class*='Value'], [class*='price']"
  )
)

# ------------------------------------------------------------------------------
# CONSTRUCTOR DE URLS 
# ------------------------------------------------------------------------------
construir_url_super <- function(supermercado, termino) {
  termino_enc <- URLencode(termino)
  if (supermercado == "exito") {
    return(paste0("https://www.exito.com/s?q=", termino_enc, "&sort=score_desc&page=0"))
  } else if (supermercado == "carulla") {
    return(paste0("https://www.carulla.com/s?q=", termino_enc, "&sort=score_desc&page=0"))
  } else if (supermercado == "jumbo") {
    return(paste0("https://www.jumbocolombia.com/search?query=", termino_enc, "&type=term"))
  }
}

# ------------------------------------------------------------------------------
# PASO 2: FUNCIÓN CENTRAL DE EXTRACCIÓN DINÁMICA (CORRECCIÓN DE PRECIO TOTAL)
# ------------------------------------------------------------------------------
scrapear_supermercado <- function(supermercado, marca, pausa = 8) {
  url <- construir_url_super(supermercado, marca)
  message("-> Scrapeando: ", toupper(supermercado), " | Cerveza: ", toupper(marca))
  
  sesion <- tryCatch({
    rvest::read_html_live(url)
  }, error = function(e) {
    message("  [!] Error de conexión: ", conditionMessage(e))
    return(NULL)
  })
  
  if (is.null(sesion)) return(tibble())
  on.exit(sesion$session$close(), add = TRUE)
  
  Sys.sleep(pausa)
  
  for (i in 1:4) {
    sesion$scroll_by(top = 800)
    Sys.sleep(2)
  }
  
  sel <- selectores[[supermercado]]
  nodos_prod <- sesion %>% html_elements(sel$contenedor)
  
  if (length(nodos_prod) == 0) {
    message("  [!] No se detectaron contenedores.")
    return(tibble())
  }
  
  datos_crudos <- map_dfr(nodos_prod, function(nodo) {
    titulo_txt <- nodo %>% html_element(sel$titulo) %>% html_text2() %>% str_squish()
    
    # --- CORRECCIÓN CRÍTICA DE PRECIOS ---
    # 1. Prioridad: Buscar la clase específica del precio final de venta (sellingPrice)
    nodo_selling <- nodo %>% html_element("[class*='sellingPrice'], [class*='price_sellingPrice']")
    
    if (!is.na(nodo_selling) && html_text2(nodo_selling) != "") {
      texto_precio <- html_text2(nodo_selling)
      precio_total <- suppressWarnings(as.numeric(str_remove_all(texto_precio, "[^0-9]")))
    } else {
      # 2. Respaldo: Si VTEX cambió el nombre de la clase, extraemos todos los precios y filtramos
      nodos_precios <- nodo %>% html_elements("[class*='price'], [class*='Value']")
      textos_precios <- nodos_precios %>% html_text2() %>% str_squish()
      numeros_precios <- suppressWarnings(as.numeric(str_remove_all(textos_precios, "[^0-9]")))
      
      # FILTRO DE REALIDAD: Una cerveza en Colombia cuesta entre $1.000 y $1.000.000.
      # Esto descarta mágicamente el precio por Ml (955), los descuentos (15) y los SKUs gigantes.
      precios_validos <- numeros_precios[!is.na(numeros_precios) & numeros_precios >= 1000 & numeros_precios <= 1000000]
      precio_total <- if(length(precios_validos) > 0) precios_validos[1] else NA
    }
    
    tibble(
      presentacion = titulo_txt,
      precio_raw   = if(exists("texto_precio")) texto_precio else paste(textos_precios, collapse = " | "),
      precio       = precio_total, # ¡El precio ya viene como número real!
      marca_buscada = marca,
      supermercado  = supermercado
    )
  })
  
  if (nrow(datos_crudos) > 0) {
    message("  [*] Éxito: ", nrow(datos_crudos), " productos. Primer precio TOTAL: $", datos_crudos$precio[1])
  }
  
  return(datos_crudos)
}

scrapear_seguro <- purrr::possibly(scrapear_supermercado, otherwise = tibble())
# ------------------------------------------------------------------------------
# PASO 3: EJECUCIÓN DEL BUCLE DE EXTRACCIÓN
# ------------------------------------------------------------------------------
resultados_lista <- list()

for (super in supermercados) {
  for (marca in marcas_cerveza) {
    clave <- paste(super, marca, sep = "_")
    resultados_lista[[clave]] <- scrapear_seguro(super, marca, pausa = 8)
    Sys.sleep(3) # Pausa ética aumentada
  }
}

datos_consolidados <- bind_rows(resultados_lista)

# ------------------------------------------------------------------------------
# PASO 4: LIMPIEZA EXHAUSTIVA Y ANÁLISIS DE PRECIOS POR ML
# ------------------------------------------------------------------------------
datos_limpios <- datos_consolidados %>%
  filter(!is.na(presentacion), presentacion != "") %>%
  mutate(
    titulo_min = str_to_lower(presentacion)
  ) %>%
  
  # Filtrar filas donde el precio no se pudo extraer correctamente
  filter(!is.na(precio)) %>% 
  
  # Filtrado de ruidos (Las llantas siguen saliendo en la extracción inicial, las matamos aquí)
  filter(!str_detect(titulo_min, "llanta|neumatico|rin|accesorios|repuesto|cubeta|hielo")) %>%
  filter(str_detect(titulo_min, "cerveza|michelob|stella|club colombia|dorada|negra|trigo|double|pack|latas|botellas")) %>%
  
  # Extracción de volumen en ml
  mutate(
    vol_extraido = str_extract(titulo_min, "\\d+\\s*(ml|cm3|mililitros)"),
    volumen_unitario_ml = as.numeric(str_extract(vol_extraido, "\\d+")),
    volumen_unitario_ml = ifelse(is.na(volumen_unitario_ml), 330, volumen_unitario_ml) # Default
  ) %>%
  
  # Extracción de cantidad de unidades (sin look-behinds)
  mutate(
    fragmento_unidades = str_extract(titulo_min, "pack\\s*(?:x|de)?\\s*\\d+|x\\s*\\d+|\\d+\\s*(?:unidades|und|latas|botellas)"),
    unidades_pack = case_when(
      str_detect(titulo_min, "six[ -]?pack") ~ 6,
      str_detect(titulo_min, "twelve[ -]?pack") ~ 12,
      !is.na(fragmento_unidades) ~ as.numeric(str_extract(fragmento_unidades, "\\d+")),
      TRUE ~ 1 
    )
  ) %>%
  
  # Cálculos de rendimiento
  mutate(
    volumen_total_ml = volumen_unitario_ml * unidades_pack,
    precio_por_ml = precio / volumen_total_ml,
    precio_por_ml = round(precio_por_ml, 4)
  ) %>%
  
  select(supermercado, marca_buscada, presentacion, precio, volumen_unitario_ml, unidades_pack, volumen_total_ml, precio_por_ml)

# ------------------------------------------------------------------------------
# PASO 5: EXPORTACIÓN
# ------------------------------------------------------------------------------
dir.create("resultados", showWarnings = FALSE)
ruta_cervezas <- file.path("resultados", "cervezas_supermercados_resultados.csv")

if (nrow(datos_limpios) > 0) {
  write_csv(datos_limpios, ruta_cervezas)
  message("\n✅ ¡Proceso completado con éxito!")
  message("📁 Archivo guardado en: ", ruta_cervezas)
  message("📊 Total de registros válidos: ", nrow(datos_limpios))
  View(datos_limpios)
}
# ------------------------------------------------------------------------------
# PASO 5: EXPORTACIÓN Y VISUALIZACIÓN
# ------------------------------------------------------------------------------
dir.create("resultados", showWarnings = FALSE)
ruta_cervezas <- file.path("resultados", "cervezas_supermercados_resultados.csv")

if (nrow(datos_limpios) > 0) {
  write_csv(datos_limpios, ruta_cervezas)
  message("\n✅ ¡Proceso completado con éxito!")
  message("📁 Archivo guardado en: ", ruta_cervezas)
  message("📊 Total de registros válidos: ", nrow(datos_limpios))
  View(datos_limpios)
} else {
  message("\n❌ El proceso terminó, pero NO se obtuvieron datos válidos.")
  message("💡 Revisa la consola: si el 'Primer precio crudo' estaba vacío o decía 'Verificar que eres humano', el sitio bloqueó el scraper.")
}



# ------------------------------------------------------------------------------
# ANÁLISIS: ¿DÓNDE SALE MÁS BARATA CADA CERVEZA? (PRECIO TOTAL)
# ------------------------------------------------------------------------------

library(dplyr)

# 1. Agrupamos por marca y supermercado, y buscamos el precio MÁS BAJO encontrado
precio_minimo_por_super <- datos_limpios %>%
  group_by(marca_buscada, supermercado) %>%
  summarise(
    precio_mas_bajo = min(precio, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Para cada marca, encontramos en qué supermercado tiene el precio más bajo absoluto
resultado_final <- precio_minimo_por_super %>%
  group_by(marca_buscada) %>%
  slice_min(order_by = precio_mas_bajo, n = 1) %>%
  ungroup() %>%
  arrange(marca_buscada)

# 3. Visualización limpia del resultado
print(resultado_final)


