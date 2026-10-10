# Taller 1 — Comparador de precios: Primeros auxilios y curación

**Autor:** Johan Steven Avilán Peñaloza  
**Curso:** Minería de Datos  
**Lenguaje elegido: R** (todo el proceso: extracción, limpieza, carga en SQLite, consultas y gráficos)

## Qué hace este proyecto

Extrae los productos de la categoría *Primeros auxilios y curación* (gasas, vendas, apósitos, curitas, esparadrapos, antisépticos para heridas y kits de curación) de tres comercios en línea de Colombia, los guarda en una base SQLite y compara el precio de ocho productos o presentaciones comunes a los tres comercios.

| Comercio | Sección consultada | Técnica |
|---|---|---|
| Farmatodo | `/categorias/salud-y-medicamentos/botiquin-y-primeros-auxilios` (subcategorías curitas-algodones-adhesivos-y-botiquin, antisepticos, vendas, compresas) | Chrome automatizado con `rvest::read_html_live()` (botón "Cargar más") |
| La Rebaja | `/drogueria/cuidado-de-la-herida` | Servicio JSON de VTEX (`/api/catalog_system/pub/products/search`) |
| Locatel | `/drogueria/cuidado-heridas` | Servicio JSON de VTEX |

## Estructura de archivos

```
parcial_01/
├── taller_1.qmd                  # informe reproducible (llama a las funciones de scraper.R)
├── taller_1.html                 # informe renderizado
├── scraper.R                     # módulo de extracción, carga en SQLite y correspondencia
├── comparables.csv               # correspondencia de los 8 productos (generada por reglas, para auditoría)
├── comparador_precios.sqlite     # base de datos
├── raw/                          # copias crudas de las respuestas, por comercio y fecha
│   ├── farmatodo/AAAA-MM-DD/*.html.gz
│   ├── la_rebaja/AAAA-MM-DD/pag_*.json
│   └── locatel/AAAA-MM-DD/pag_*.json
├── exploracion.R                 # script de trabajo usado durante el desarrollo
├── renv.lock                     # dependencias con versiones fijas
└── README.md
```

## Requisitos

- R 4.1 o superior.
- **Google Chrome** instalado (lo usa `read_html_live()` para Farmatodo).
- Dependencias con versiones fijas en `renv.lock`:

```r
install.packages("renv")
renv::restore()
```

## Ejecución en línea (consulta los sitios)

Desde la carpeta del proyecto, en una consola de R:

```r
source("scraper.R")
ejecutar_recoleccion_completa(".", modo = "online")
```

Hace lo siguiente: recolecta los tres comercios, guarda las copias crudas en `raw/<comercio>/<fecha>/`, registra cada corrida en la tabla `ejecuciones` (completa, parcial o fallida) y agrega las observaciones a la base **sin sobrescribir** las anteriores. Cada ejecución en una fecha distinta suma nuevas observaciones.

Para generar el informe consultando los sitios:

```bash
MODO=online quarto render taller_1.qmd
```

## Ejecución sin conexión (reprocesa `raw/`)

No consulta ningún sitio. Reconstruye la base a partir de las copias crudas:

```bash
quarto render taller_1.qmd
```

(`MODO` es `offline` por defecto.) Si se borra `comparador_precios.sqlite`, el informe la reconstruye a partir de `raw/` y de `comparables.csv`.

## Organización del módulo `scraper.R`

- Una función por comercio, que devuelve **siempre las mismas columnas** (`columnas_estandar`): `scrape_farmatodo()`, `scrape_la_rebaja()` y `scrape_locatel()`. Cada una admite `modo = "online"` o `"offline"`.
- Una función general, `ejecutar_recoleccion_completa()`, que recolecta los tres comercios, registra cada ejecución, inserta en SQLite sin sobrescribir observaciones anteriores y aplica la delimitación de la categoría y la estandarización.
- Copias crudas comprimidas (`.html.gz` en Farmatodo y `.json.gz` en La Rebaja y Locatel), junto con la fecha-hora de captura y, en las tiendas VTEX, el total declarado por el sitio.

## Decisiones de diseño

- **Estandarización en campos propios:** marca, nombre y subcategoría estandarizados (`anuncios.marca`, `nombre_estandar`, `subcategoria`) y contenido/unidad de los comparables; el texto original no se modifica.
- **Texto original conservado.** Nombre, marca, presentación y precio se guardan tal como aparecen en el sitio, y la limpieza va en columnas aparte.
- **Productos agotados:** se guardan con `precio = NULL`, nunca con 0, y se excluyen de los cálculos de precios.
- **Delimitación de la categoría:** reglas explícitas de exclusión (ostomías, guantes, tapabocas, jeringas, instrumentos, removedores de adhesivo, copitos y antisépticos de manos). Quedan en `anuncios.motivo_exclusion`.
- **Correspondencia de productos:** por reglas en código (`especificacion_comparables` en `scraper.R`), registrada en la tabla `productos_comparables` y en `comparables.csv`. En La Rebaja y Locatel se usa el código de barras (EAN); en Farmatodo, una expresión regular sobre el nombre (marca, producto y medida) que debe identificar un solo anuncio.
- **Unicidad:** un anuncio es único por comercio y SKU; una observación, por anuncio y fecha-hora de consulta.

## Consultas SQL

Las consultas del informe siguen la notación del taller de SQL de la clase: `INNER JOIN` y `LEFT JOIN`, subconsultas, CTEs encadenadas (`WITH`) y funciones de ventana (`RANK() OVER`, `LAG`). Se ejecutan con `dbGetQuery()` y se muestran con `mostrar_tabla()`.

## Limitaciones

- Los precios son los del momento de la captura y pueden variar por promociones o por la ciudad de entrega.
- Farmatodo no publica EAN ni descripción en el listado.
- Los sitios pueden cambiar su estructura. Si eso ocurre, el procesamiento puede verificarse con las copias de `raw/`.

## Cumplimiento ético

Pausa de 1 segundo entre peticiones, reintentos limitados, *user agent* que identifica el uso académico y consulta únicamente de información pública. Revisión de `robots.txt` y términos de uso de los tres sitios: **[COMPLETAR]**. Prueba desde otro entorno (Colab/GitHub Actions): **[COMPLETAR]**.
