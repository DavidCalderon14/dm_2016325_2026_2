# Taller 1 — Comparador de precios: Primeros auxilios y curación

**Autor:** Johan Steven Avilan Peñaloza  
**Curso:** Minería de Datos  
**Lenguaje elegido:** R (extracción, limpieza, carga en SQLite, consultas y gráficos)  

---

## Qué hace este proyecto

Extrae productos de la categoría *Primeros auxilios y curación* (gasas, vendas, apósitos, curitas, esparadrapos, antisépticos para heridas y kits de curación) de tres comercios en línea de Colombia, los almacena en una base de datos SQLite y compara el precio de ocho productos comunes a los tres comercios.

| Comercio | Sección consultada | Técnica de extracción |
|---|---|---|
| **Farmatodo** | `/categorias/salud-y-medicamentos/botiquin-y-primeros-auxilios` (subcategorías: curitas/algodones/adhesivos, antisépticos, vendas, compresas) | Chrome automatizado con `rvest::read_html_live()` (interacción con botón "Cargar más") |
| **La Rebaja** | `/drogueria/cuidado-de-la-herida` | Servicio API JSON de VTEX (`/api/catalog_system/pub/products/search`) |
| **Locatel** | `/drogueria/cuidado-heridas` | Servicio API JSON de VTEX (`/api/catalog_system/pub/products/search`) |

---

## Estructura de archivos

```text
parcial_01/
├── taller_1.qmd                # Informe reproducible (Quarto)
├── taller_1.html               # Informe renderizado interactivo
├── scraper.R                   # Módulo de extracción, limpieza, SQLite y correspondencia
├── comparables.csv             # Correspondencia de los 8 productos (generada por código)
├── comparador_precios.sqlite   # Base de datos SQLite consolidadora
├── raw/                        # Copias crudas por comercio y fecha de captura
│   ├── farmatodo/AAAA-MM-DD/*.html.gz
│   ├── la_rebaja/AAAA-MM-DD/pag_*.json(.gz)
│   └── locatel/AAAA-MM-DD/pag_*.json(.gz)
├── prueba_colab.ipynb          # Cuaderno para validación en Google Colab
├── renv.lock                   # Archivo de congelamiento de dependencias
└── README.md                   # Documentación del proyecto
```

---

## Prerrequisitos e instalación

1. **Entorno R:** requiere R v4.1 o superior.
2. **Quarto CLI:** instalado para la generación del reporte técnico en HTML.
3. **Navegador web:** Google Chrome, requerido únicamente para la captura en vivo de Farmatodo mediante `chromote`.

### Restaurar las dependencias del proyecto

Antes de ejecutar cualquier script, abre la terminal en la raíz del proyecto y restaura los paquetes instalados en `renv.lock`:

```r
renv::restore()
```

---

## Instrucciones de ejecución

El proyecto soporta dos flujos de trabajo: **Offline** (reprocesamiento rápido sin peticiones web) y **Online** (scraping en tiempo real).

### Resumen rápido de comandos

| Objetivo | Comando a ejecutar |
|---|---|
| **Instalar dependencias** | `Rscript -e "renv::restore()"` |
| **Generar informe (Offline)** | `quarto render taller_1.qmd` |
| **Actualizar datos desde internet** | `Rscript -e 'source("scraper.R"); ejecutar_recoleccion_completa(".", modo="online")'` |
| **Actualizar datos + generar informe** | `MODO=online quarto render taller_1.qmd` *(Linux/macOS)* |

---

### Opción 1: Ejecución sin conexión / Reprocesamiento (Offline)

Es el modo predeterminado. No realiza peticiones a internet ni requiere Chrome. Reconstruye la base de datos `comparador_precios.sqlite` (si fue eliminada) a partir de los archivos almacenados en `raw/` y genera el informe final HTML.

#### Desde la terminal del sistema (Bash / PowerShell / CMD)

```bash
quarto render taller_1.qmd
```

#### Desde la consola interactiva de R

```r
quarto::quarto_render("taller_1.qmd")
```

---

### Opción 2: Recolección de datos en vivo (Online)

Ejecuta el scraping sobre las tres tiendas, guarda los datos crudos en `raw/<comercio>/<fecha>/`, registra la auditoría de ejecución y añade las nuevas observaciones a SQLite sin sobrescribir las históricas.

#### A. Solo ejecutar la recolección y actualizar SQLite

Abre R en la raíz del proyecto y ejecuta:

```r
source("scraper.R")
ejecutar_recoleccion_completa(".", modo = "online")
```

#### B. Recolectar datos en vivo y generar el informe renderizado

**En Linux/macOS:**

```bash
MODO=online quarto render taller_1.qmd
```

**En Windows (PowerShell):**

```powershell
$env:MODO="online"; quarto render taller_1.qmd
```

**En Windows (CMD):**

```cmd
set MODO=online && quarto render taller_1.qmd
```

---

## Decisiones de diseño y arquitectura

- **Estandarización limpia:** la marca (`anuncios.marca`), el nombre (`nombre_estandar`), la subcategoría y las unidades de medida se limpian en campos dedicados. El texto original capturado se conserva intacto en columnas adyacentes.
- **Manejo de productos agotados:** los artículos sin stock conservan su registro, pero asignando `precio = NULL` (nunca cero) para evitar sesgar las métricas de precio.
- **Filtros de exclusión:** reglas explícitas descartan artículos fuera de alcance (guantes, tapabocas, jeringas, ostomías, etc.), registrando la razón en `anuncios.motivo_exclusion`.
- **Mapeo de comparables:** la vinculación de los ocho productos representativos utiliza el código de barras (EAN) para tiendas VTEX (La Rebaja y Locatel) y expresiones regulares sobre el nombre comercial para Farmatodo.
- **Identificación única:** un producto es único por `(comercio, SKU)`, mientras que una observación es única por `(anuncio, fecha_captura)`.

---

## Consultas y análisis de datos

Las analíticas incluidas en el informe combinan sentencias SQL avanzadas (`INNER/LEFT JOIN`, subconsultas, `WITH` / CTEs encadenadas y funciones de ventana como `RANK() OVER` y `LAG`) ejecutadas sobre la base SQLite mediante `DBI::dbGetQuery()`.

---

## Cumplimiento ético y límites

- **Frecuencia de peticiones:** pausas programadas entre peticiones HTTP (1 segundo en APIs VTEX y varios segundos en la interacción con el DOM de Farmatodo).
- **Identificación de cliente:** reintentos limitados y uso de un *User-Agent* académico explícito.
- **Revisión de `robots.txt`:** se verificaron las políticas de scraping de las tres tiendas, confirmando acceso permitido a las rutas públicas de consulta.