
# Carga de paquetes -------------------------------------------------------

library(
  "rvest", "xml2", "dplyr", "stringr",
  "purrr", "janitor", "readr", "knitr",
  "httr"
)

# Creacion lista de URLS --------------------------------------------------



URL_base_1 <- "https://www.accuweather.com/es/co/bogota/107487/"
URL_base_2 <- "-weather/107487?year=2025"
meses <- c("january", "february", "march",
           "april", "may", "june",
           "july", "august", "september",
           "october", "November", "december")

URLS <- vector()
for (i in 1:12){
  URLS[i] <- paste0(URL_base_1, meses[i], URL_base_2)
}





# Agente ------------------------------------------------------------------



# lectura html ------------------------------------------------------------

# clima <- GET(URLS[1],
             user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"),
             add_headers(`Accept-Language` = "en-US,en;q=0.9"))

clima_html <- read_html(content(clima, "text"))
status_code(clima)



#/html/body/div/div[7]/div[1]/div[1]/div[2]/div/div[2]


