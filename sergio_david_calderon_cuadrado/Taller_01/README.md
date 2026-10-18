# Comparador de precios — Cuidado corporal (Éxito, Farmatodo, La Rebaja)

Recolección por *web scraping* de productos de cuidado corporal en tres comercios
colombianos, estandarización a una base común, almacenamiento en SQLite y análisis
comparativo en un cuaderno Quarto.

## Lenguaje y herramientas

- **Lenguaje:** R (≥ 4.1; la versión exacta queda registrada en `renv.lock`).
- **Cuaderno:** Quarto (≥ 1.4) con motor knitr → `taller_1.html`.
- **Base de datos:** SQLite (`processed/precios.sqlite`), consultada con SQL desde R (DBI + RSQLite).
- **Scraping:** rvest + chromote (navegador controlado), solo en modo en línea.
- **Dependencias con versión fija:** `renv.lock`.

## Estructura de archivos

```
Taller_1/
├── taller_1.qmd             Cuaderno: documentación, consultas SQL, gráficos
├── scraper_general_1.R      Scraper + estandarización + control de calidad + comparables
├── sqlite_scraper.R         Esquema SQLite, carga idempotente, vistas
├── dependencias.R           Lista de paquetes (para que renv los detecte; no se ejecuta)
├── crear_renv.R             Genera renv.lock (se corre una vez)
├── renv.lock                Versiones exactas de R y paquetes
├── renv/ , .Rprofile        Infraestructura de renv (se crea con crear_renv.R)
├── raw/                     Respaldos CSV por comercio (resultado_seguro_*.csv) y log_consultas.csv
├── processed/               precios.sqlite, precios_ultima_fecha.csv, comparativa_precios.csv
└── taller_1.html            Resultado del render
```

`raw/` y `processed/` se crean solas si no existen. **`raw/` es imprescindible para
correr sin conexión**: es de donde se reconstruye la base.

## Instalación (una vez)

1. Instala [R](https://cran.r-project.org/) y [Quarto](https://quarto.org/docs/get-started/).
2. Abre R en la carpeta del proyecto (con `renv` instalado se activa solo) y restaura las versiones fijas:
   ```r
   install.packages("renv")   # si no lo tienes
   renv::restore()
   ```
   Esto necesita internet **solo la primera vez** (o una caché de renv previa).

## Ejecución sin conexión (modo `respaldo`, por defecto)

No scrapea: reconstruye `processed/precios.sqlite` desde los CSV de `raw/` y analiza.

```bash
quarto render taller_1.qmd
```

Solo requiere que `raw/` contenga los `resultado_seguro_<comercio>.csv`.

## Ejecución en línea (modo `linea`)

Scrapea los tres comercios en vivo (tarda varios minutos), agrega las filas a los
respaldos de `raw/`, actualiza la base y analiza.

```bash
quarto render taller_1.qmd -P modo:linea
```

Requisitos: internet y un navegador **Brave, Chrome, Chromium o Edge** instalado
(Windows, macOS o Linux). El scraper busca Brave primero y luego los demás; si tu
navegador está en una ruta no estándar, define la variable de entorno
`CHROMOTE_CHROME` con la ruta del ejecutable.

También puedes cambiar el valor por defecto en el encabezado del `.qmd`:

```yaml
params:
  modo: "respaldo"          # "respaldo" | "linea"
  reconstruir_base: true    # false = reutiliza processed/precios.sqlite si ya está vigente
```

## Otras opciones

| Qué | Cómo |
|---|---|
| Usar otra carpeta de trabajo | variable de entorno `TALLER_DIR` antes de renderizar |
| Render rápido con base ya construida | `-P reconstruir_base:false` |
| Correr solo el scraper | definir `MODO` (`"linea"`/`"respaldo"`) y `source("scraper_general_1.R")` desde la carpeta del proyecto |

Si el `.qmd` se mueve dentro de un proyecto Quarto (`_quarto.yml`), la carpeta de
trabajo por defecto es la raíz del proyecto; en ese caso define `TALLER_DIR`.

## Reproducibilidad

- Versiones de R y paquetes fijas en `renv.lock` (`renv::restore()`).
- La base `precios.sqlite` es derivada: se reconstruye de forma idempotente desde `raw/`
  (correrla dos veces con los mismos datos no duplica observaciones).
- Cada corrida del scraper registra `id_ejecucion` y fecha/hora ISO 8601 (zona America/Bogota),
  y cada observación guarda la URL de extracción, para trazabilidad.
- Los precios dependen del momento de la captura; el modo en línea genera una corrida nueva y
  los resultados pueden cambiar frente a la anterior. El modo `respaldo` reproduce la última base guardada.

## Actualizar dependencias

Tras instalar o actualizar un paquete: `renv::snapshot()` y sube el `renv.lock` nuevo.
