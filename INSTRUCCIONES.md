# Alquileres_Montevideo_Trabajo_Final_Lucas_Giudice_CDR
Trabajo final para la materia de Ciencia de Datos con R realizado por Lucas Giúdice sobre el mercado de alquileres de apartamentos en Montevideo. 

A continuación se describen los principales scripts y archivos que componen este proyecto:

* **`INFORME.qmd` / `INFORME.html`:** Documento fuente en Quarto y su versión renderizada en HTML. Contiene el análisis exploratorio de datos (EDA) en profundidad, la justificación metodológica y la explicación detallada de todo el proceso de desarrollo del trabajo.

* **`Armando_Base_MELI/`:** Contiene los scripts encargados de la limpieza y depuración de la base proveniente del *web scraping* (`apartamentos_final`), generando la base final limpia (**`base`**). Además, en esta etapa se calculan los **centroides geográficos por barrio** (`centroides_barrios`) para obtener las métricas de distancia, ubicadas ambas en la carpeta de la app.

* **`Entrenando_RF/`:** Incluye los scripts de R con las especificaciones, hiperparámetros y flujo de trabajo (`tidymodels`/`ranger`) utilizados para el entrenamiento del modelo de *Random Forest*. El modelo ajustado resultante se guarda en el archivo **`rf_alquileres_Montevideo`**, ubicada en la carpeta de la app.

* **`APP_ALQUILERES/APP_SHINY/`:** Código fuente de la aplicación interactiva construida en Shiny. Permite a los usuarios visualizar el mapa espacial de precios y desplegar el módulo predictivo para estimar el precio medio de alquiler según los insumos ingresados.
**La app se encuentra publicada en el siguiente link: https://lgiudice2004.shinyapps.io/alquileres-montevideo/**

* **Archivos Shapefile (`.shp`, `.dbf`, `.shx`, etc.):** Conjunto de datos geográficos que proveen las geometrías y capas espaciales necesarias para delimitar los barrios de Montevideo, calcular distancias y renderizar los mapas interactivos.

