# =====================================================================
#  DEPENDENCIAS DEL PROYECTO  (este archivo NO se ejecuta)
#  Existe para que renv::snapshot() detecte TODOS los paquetes, incluidos los
#  que el scraper carga con lapply(paquetes, library, ...) y renv no ve.
#  Si agregas un paquete al proyecto, agrégalo aqui y vuelve a hacer snapshot.
# =====================================================================
if (FALSE) {
  # cuaderno (.qmd)
  library(knitr);   library(rmarkdown)
  library(DBI);     library(RSQLite)
  library(dplyr);   library(tidyr)
  library(ggplot2); library(scales); library(tidytext)

  # scraper_general_1.R / sqlite_scraper.R
  library(rvest);   library(xml2);  library(stringr)
  library(purrr);   library(tibble)
  library(chromote)   # solo se usa en modo "linea" (scraping en vivo)
}
