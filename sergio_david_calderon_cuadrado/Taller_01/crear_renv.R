# =====================================================================
#  GENERA renv.lock (versiones fijas). Se corre UNA vez, en el equipo donde
#  el proyecto ya funciona, con la carpeta del proyecto como directorio de trabajo.
#
#     setwd("<carpeta del proyecto>")
#     source("crear_renv.R")
#
#  Despues, sube renv.lock, renv/activate.R y .Rprofile junto con el proyecto.
# =====================================================================
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# 1) Crea la biblioteca privada del proyecto (renv/) y .Rprofile
renv::init(bare = TRUE, restart = FALSE)

# 2) Instala en ella los paquetes que declara dependencias.R
renv::install(c("knitr", "rmarkdown", "DBI", "RSQLite", "dplyr", "tidyr", "ggplot2",
                "scales", "tidytext", "rvest", "xml2", "stringr", "purrr", "tibble",
                "chromote"))

# 3) Congela las versiones exactas -> renv.lock
renv::snapshot(type = "implicit", prompt = FALSE)   # escanea los .R/.qmd, incl. dependencias.R

cat("\nListo. Revisa renv.lock (R y paquetes con su version) y verifica con renv::status()\n")
