# =============================================================
# WEB SCRAPING DINÁMICO - CERVEZAS (Éxito, Carulla, Jumbo)
# Michelob Ultra | Stella Artois | Club Colombia
# =============================================================
# Patrón alineado al ejercicio del taller (parte Amazon):
# read_html_live() + Sys.sleep() + purrr::possibly() + on.exit()
#
# Instalar una sola vez:
#   install.packages(c("rvest", "chromote", "dplyr", "purrr", "stringr", "readr", "tibble"))
# Requiere tener Google Chrome o Chromium instalado en el equipo.
# =============================================================

library(rvest)
library(chromote)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)

# -------------------------------------------------------------
# 0) SUBIR EL TIMEOUT DE CHROMOTE (por defecto son solo 10 seg,
#    insuficiente para sitios pesados como Carulla/Éxito/Jumbo)
# -------------------------------------------------------------
chrome_obj <- chromote::default_chromote_object()
chrome_obj$default_timeout <- 60
chromote::set_default_chromote_object(chrome_obj)

# -------------------------------------------------------------
# 1) TÉRMINOS DE BÚSQUEDA
# -------------------------------------------------------------
terminos <- c("michelob", "stella artois", "club colombia")

# -------------------------------------------------------------
# 2) ESTRUCTURA VACÍA (se usa cuando un sitio falla o no trae nada)
# -------------------------------------------------------------
tabla_vacia <- function() {
  tibble(
    supermercado     = character(),
    termino_busqueda = character(),
    presentacion     = character(),
    precio           = character(),
    url_busqueda     = character()
  )
}

# -------------------------------------------------------------
# 3) CONFIGURACIÓN DE SUPERMERCADOS
# -------------------------------------------------------------
# tipo_extraccion:
#   "css"      -> nombre y precio se leen como TEXTO de selectores CSS
#                 dentro de la ficha (Éxito y Carulla)
#   "atributo" -> nombre y precio vienen como ATRIBUTOS HTML de la
#                 propia ficha (Jumbo: data-cnstrc-item-name / -price)
#
# Nota: los selectores usan [class*='...'] (coincidencia PARCIAL) en
# vez de la clase completa con hash (ej. productCard_productCard__M0677),
# porque ese sufijo lo genera el bundler y puede cambiar entre despliegues.

supermercados <- list(
  
  exito = list(
    nombre = "Éxito",
    construir_url = function(termino) {
      paste0("https://www.exito.com/s?q=", URLencode(termino), "&sort=score_desc&page=0")
    },
    tipo_extraccion = "css",
    selector_ficha  = "article[class*='productCard_productCard']",
    selector_nombre = "h3[class*='styles_name']",
    selector_precio = "p[data-fs-container-price-otros='true']"
  ),
  
  carulla = list(
    nombre = "Carulla",
    construir_url = function(termino) {
      paste0("https://www.carulla.com/s?q=", URLencode(termino), "&sort=score_desc&page=0")
    },
    tipo_extraccion = "css",
    selector_ficha  = "article[class*='productCard_productCard']",
    selector_nombre = "h3[class*='styles_name']",
    selector_precio = "p[data-fs-container-price-otros='true']"
  ),
  
  jumbo = list(
    nombre = "Jumbo",
    construir_url = function(termino) {
      paste0("https://www.jumbocolombia.com/search?query=", URLencode(termino), "&type=term")
    },
    tipo_extraccion = "atributo",
    selector_ficha   = "div[class*='custom-search-0-x-product__container']",
    atributo_nombre  = "data-cnstrc-item-name",
    atributo_precio  = "data-cnstrc-item-price"
  )
)

# -------------------------------------------------------------
# 4) FUNCIÓN DE SCRAPING PARA UN (supermercado, término)
# -------------------------------------------------------------
scrapear_busqueda <- function(supermercado_id, termino, pausa = 5) {
  
  cfg <- supermercados[[supermercado_id]]
  url <- cfg$construir_url(termino)
  
  message(sprintf("-> Consultando %s | término: '%s'", cfg$nombre, termino))
  
  sesion <- read_html_live(url)
  on.exit(sesion$session$close(), add = TRUE)  # se cierra siempre, aunque falle algo después
  
  # Le damos tiempo al navegador para que JS pinte los productos
  Sys.sleep(pausa)
  
  nodos <- sesion %>% html_elements(cfg$selector_ficha)
  
  if (length(nodos) == 0) {
    message(sprintf("   %s | '%s': no se encontraron fichas de producto.", cfg$nombre, termino))
    return(tabla_vacia())
  }
  
  if (cfg$tipo_extraccion == "css") {
    nombres <- nodos %>% html_element(cfg$selector_nombre) %>% html_text2()
    precios <- nodos %>% html_element(cfg$selector_precio) %>% html_text2()
  } else {
    nombres <- nodos %>% html_attr(cfg$atributo_nombre)
    precios <- nodos %>% html_attr(cfg$atributo_precio)
  }
  
  tibble(
    supermercado     = cfg$nombre,
    termino_busqueda = termino,
    presentacion     = nombres,
    precio           = precios,
    url_busqueda     = url
  )
}

# Versión "segura": si algo truena (timeout, sitio caído, selector roto, etc.)
# devuelve tabla vacía en vez de detener todo el proceso.
scrapear_busqueda_segura <- purrr::possibly(scrapear_busqueda, otherwise = tabla_vacia())

# -------------------------------------------------------------
# 5) EJECUCIÓN: recorre supermercado x término, con pausas
# -------------------------------------------------------------
resultados <- list()

for (super_id in names(supermercados)) {
  for (termino in terminos) {
    
    res <- scrapear_busqueda_segura(super_id, termino, pausa = 5)
    resultados[[length(resultados) + 1]] <- res
    
    # Pausa entre solicitudes para no saturar el sitio
    Sys.sleep(runif(1, min = 2, max = 4))
  }
}

# -------------------------------------------------------------
# 6) CONSOLIDAR + ASEGURAR QUE SEA CERVEZA + LIMPIAR PRECIO
# -------------------------------------------------------------
cervezas_total <- bind_rows(resultados)

if (nrow(cervezas_total) > 0) {
  cervezas_total <- cervezas_total %>%
    filter(str_detect(str_to_lower(presentacion), "cerveza")) %>%
    mutate(
      precio_numerico = precio %>%
        str_remove_all("[^0-9]") %>%
        as.numeric()
    )
}

print(cervezas_total)

if (nrow(cervezas_total) == 0) {
  message("⚠️  No se obtuvo NINGÚN resultado. Revisa mensajes de arriba: ",
          "¿algún sitio mostró 'no se encontraron fichas'? Eso indica selector roto o bloqueo del sitio.")
}

# -------------------------------------------------------------
# 7) GUARDAR RESULTADOS (readr::write_csv maneja bien UTF-8)
# -------------------------------------------------------------
dir.create("resultados", showWarnings = FALSE)
ruta_salida <- file.path("resultados", "cervezas_exito_carulla_jumbo.csv")
write_csv(cervezas_total, ruta_salida)

cat("\n✅ Proceso terminado. Filas obtenidas:", nrow(cervezas_total), "\n")
cat("Archivo guardado en:", ruta_salida, "\n")




library(readr)
cervezas_exito_carulla_jumbo <- read_csv("resultados/cervezas_exito_carulla_jumbo.csv")
View(cervezas_exito_carulla_jumbo)




#### Analisis por cerveza 
# =============================================================
# ANÁLISIS: precio total, precio por ml y comparación entre
# Éxito, Carulla y Jumbo para Michelob, Stella Artois y Club Colombia
# =============================================================

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

cervezas <- read_csv("resultados/cervezas_exito_carulla_jumbo.csv")

# Por si el CSV se generó con una versión anterior del scraper y precio_numerico
# quedó mal calculado, lo recalculamos aquí mismo de forma robusta:
limpiar_precio <- function(x) {
  con_signo <- str_extract(x, "\\$\\s?[\\d\\.,]+")
  monto     <- ifelse(!is.na(con_signo), con_signo, x)
  monto %>% str_remove_all("[^0-9]") %>% as.numeric()
}

cervezas <- cervezas %>%
  mutate(precio_numerico = limpiar_precio(precio)) %>%
  select(-precio)  # ya no la necesitamos: todo el análisis usa precio_numerico

# -------------------------------------------------------------
# 1) EXTRAER VOLUMEN EN ML DESDE LA PRESENTACIÓN (con regex)
# -------------------------------------------------------------
# Maneja 3 formatos que aparecen en los datos:
#   a) "... sixpack (1980  ml)"      -> volumen TOTAL ya viene entre paréntesis
#   b) "... Botella 330 ml x6"        -> volumen unitario x cantidad de unidades
#   c) "... Lata 355 ml"               -> una sola unidad
#   d) "... 1.5 L" / "1,5 L"           -> en litros, se convierte a ml
extraer_volumen_ml <- function(texto) {
  texto <- str_to_lower(texto)
  
  # En los datos, el volumen SIEMPRE es un número entero de ml (ej. "1.980 ml"
  # o "7.920 ml"), y el punto ahí es separador de MILES (formato colombiano),
  # no un decimal. Por eso quitamos únicamente los puntos, sin tratarlos como
  # coma decimal (eso fue lo que causaba el bug: "7.920" se leía como 7.92).
  quitar_separador_miles <- function(x) as.numeric(str_remove_all(x, "\\."))
  
  # a) Total explícito entre paréntesis: (1.980 ml) / (1980  ml)
  total_parentesis <- str_match(texto, "\\(([\\d\\.]+)\\s*ml\\)")[, 2]
  
  # b) Volumen unitario x cantidad de unidades: "330 ml x6"
  unidad_por_cantidad <- str_match(texto, "([\\d\\.]+)\\s*ml\\s*x\\s*(\\d+)")
  
  # c) Solo un número seguido de ml, sin multiplicador
  solo_ml <- str_match(texto, "([\\d\\.]+)\\s*ml")[, 2]
  
  # d) En litros: aquí sí puede haber un decimal REAL, escrito con coma (ej. "1,5 l")
  en_litros <- str_match(texto, "(\\d+(?:,\\d+)?)\\s*l\\b")[, 2]
  
  case_when(
    !is.na(total_parentesis)        ~ quitar_separador_miles(total_parentesis),
    !is.na(unidad_por_cantidad[,2]) ~ quitar_separador_miles(unidad_por_cantidad[,2]) * as.numeric(unidad_por_cantidad[,3]),
    !is.na(solo_ml)                 ~ quitar_separador_miles(solo_ml),
    !is.na(en_litros)               ~ as.numeric(str_replace(en_litros, ",", ".")) * 1000,
    TRUE                            ~ NA_real_
  )
}

cervezas_analisis <- cervezas %>%
  mutate(
    volumen_ml    = extraer_volumen_ml(presentacion),
    precio_por_ml = precio_numerico / volumen_ml
  )

cat("Filas sin volumen detectado (revisar manualmente):\n")
cervezas_analisis %>% filter(is.na(volumen_ml)) %>% select(supermercado, termino_busqueda, presentacion) %>% print()

# -------------------------------------------------------------
# PREGUNTA 1: más barato por precio TOTAL (sin ver presentación)
# -------------------------------------------------------------
precio_total_promedio <- cervezas_analisis %>%
  group_by(termino_busqueda, supermercado) %>%
  summarise(precio_promedio = mean(precio_numerico, na.rm = TRUE), n_productos = n(), .groups = "drop") %>%
  arrange(termino_busqueda, precio_promedio)

cat("\n=== PREGUNTA 1: precio TOTAL promedio por cerveza y supermercado ===\n")
print(precio_total_promedio)

mas_barato_total <- precio_total_promedio %>%
  group_by(termino_busqueda) %>%
  slice_min(precio_promedio, n = 1)

cat("\nMás barato por precio TOTAL (por cerveza):\n")
print(mas_barato_total)

# -------------------------------------------------------------
# PREGUNTA 2: precio promedio por ml (comparación justa entre tamaños)
# -------------------------------------------------------------
precio_ml_promedio <- cervezas_analisis %>%
  filter(!is.na(volumen_ml)) %>%
  group_by(supermercado) %>%
  summarise(precio_ml_promedio = mean(precio_por_ml, na.rm = TRUE), n_productos = n(), .groups = "drop") %>%
  arrange(precio_ml_promedio)

cat("\n=== PREGUNTA 2: precio promedio por ML (general, todas las cervezas juntas) ===\n")
print(precio_ml_promedio)

precio_ml_por_cerveza <- cervezas_analisis %>%
  filter(!is.na(volumen_ml)) %>%
  group_by(termino_busqueda, supermercado) %>%
  summarise(precio_ml_promedio = mean(precio_por_ml, na.rm = TRUE), .groups = "drop") %>%
  arrange(termino_busqueda, precio_ml_promedio)

cat("\nPrecio promedio por ML, desglosado por cerveza:\n")
print(precio_ml_por_cerveza)

# -------------------------------------------------------------
# PREGUNTA 3: ¿el más barato cambia según la cerveza, o es siempre el mismo?
# -------------------------------------------------------------
mas_conveniente_por_cerveza <- precio_ml_por_cerveza %>%
  group_by(termino_busqueda) %>%
  slice_min(precio_ml_promedio, n = 1)

cat("\n=== PREGUNTA 3: supermercado más conveniente (por ml) en cada cerveza ===\n")
print(mas_conveniente_por_cerveza)

es_consistente <- n_distinct(mas_conveniente_por_cerveza$supermercado) == 1
cat("\n¿Es el mismo supermercado el más barato en las 3 cervezas?:", es_consistente, "\n")

# -------------------------------------------------------------
# PREGUNTA 4: comparar solo presentaciones equivalentes
# (la unidad/lata individual más pequeña de cada cerveza, por supermercado)
# -------------------------------------------------------------
presentacion_mas_pequena <- cervezas_analisis %>%
  filter(!is.na(volumen_ml)) %>%
  group_by(termino_busqueda, supermercado) %>%
  slice_min(volumen_ml, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(termino_busqueda, supermercado, presentacion, volumen_ml, precio_numerico, precio_por_ml) %>%
  arrange(termino_busqueda, precio_por_ml)

cat("\n=== PREGUNTA 4: presentación más pequeña por cerveza y supermercado ===\n")
print(presentacion_mas_pequena)

mas_barato_equivalente <- presentacion_mas_pequena %>%
  group_by(termino_busqueda) %>%
  slice_min(precio_por_ml, n = 1)

cat("\nMás barato comparando solo presentaciones equivalentes:\n")
print(mas_barato_equivalente)

# -------------------------------------------------------------
# PREGUNTA 5: filtro de resultados irrelevantes (ya aplicado en el scraping)
# -------------------------------------------------------------
cat("\n=== PREGUNTA 5 ===\n")
cat("El filtro se aplicó en el script de scraping con:\n")
cat("  filter(str_detect(str_to_lower(presentacion), 'cerveza'))\n")
cat("Esto descarta cualquier producto cuyo nombre no contenga la palabra 'cerveza',\n")
cat("como llantas u otros artículos que coincidan por nombre de marca.\n")

# -------------------------------------------------------------
# GRÁFICO: precio promedio por ml, por supermercado y cerveza
# -------------------------------------------------------------
grafico_precio_ml <- ggplot(precio_ml_por_cerveza,
                            aes(x = termino_busqueda, y = precio_ml_promedio, fill = supermercado)) +
  geom_col(position = "dodge") +
  labs(
    title = "Precio promedio por mililitro de cerveza",
    x = "Cerveza",
    y = "Precio por ml (COP)",
    fill = "Supermercado"
  ) +
  theme_minimal()

print(grafico_precio_ml)

ggsave("resultados/comparativa_precio_por_ml.png", grafico_precio_ml, width = 8, height = 5)
cat("\n✅ Gráfico guardado en: resultados/comparativa_precio_por_ml.png\n")