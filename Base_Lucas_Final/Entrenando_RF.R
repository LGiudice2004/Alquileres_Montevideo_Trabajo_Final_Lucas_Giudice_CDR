# Librerías ----
library(tidymodels)
library(ranger)
library(readr)
library(dplyr)

# Cargamos los datos ----
base <- read_delim("APP_ALQUILERES/base.csv", delim = ";", 
                   escape_double = FALSE, trim_ws = TRUE)

# Conversión de los precios a pesos, seleccionamos las variables de entrenamiento  ----
base_rf <- base |> 
  mutate(
    precio_pesos = as.numeric(if_else(moneda == "USD(US$)", precio * 40, precio)),
    across(c(barrio, balcon, garage), ~as.factor(.x)),
    across(c(dormitorios, baños, metros_totales, gastos_comunes, starts_with("dist_")), ~as.numeric(.x)),
  ) |> 
  select(precio_pesos, dormitorios, baños, balcon, garage, metros_totales, gastos_comunes, starts_with("dist_"))

# Split de los datos ----
set.seed(123) 
split_rf <- initial_split(base_rf, prop = 0.80)
train_rf <- training(split_rf)
test_rf  <- testing(split_rf)

# Modelo ----
rf <- rand_forest(
  trees = 500,    # Cantidad de árboles
  min_n = 20,      # Mínimo de nodos por cada hoja
  mtry = 6         # Cantidad de variables evaluadas en cada bootstrap
) |> 
  set_engine("ranger", importance = "impurity", num.threads = 4) |> 
  set_mode("regression")

# Receta ----
receta_rf <- recipe(precio_pesos ~ ., data = train_rf) |> 
  step_zv(all_predictors()) 

# Workflow ----
workflow_rf <- workflow() |> 
  add_recipe(receta_rf) |> 
  add_model(rf)

modelo_rf <- fit(workflow_rf, data = train_rf)

# Predecimos y evaluamos las metricas para los datos de train ----

predicciones_train <- predict(modelo_rf, new_data = train_rf) |> 
  bind_cols(train_rf |> select(precio_pesos)) 

metricas_train <- metrics(predicciones_train, truth = precio_pesos, estimate = .pred)

print("--- Métricas de Train ---")
print(metricas_train)

# Predecimos y evaluamos las metricas para los datos de train ----
predicciones_test <- predict(modelo_rf, new_data = test_rf) |> 
  bind_cols(test_rf |> select(precio_pesos))

metricas_test <- metrics(predicciones_test, truth = precio_pesos, estimate = .pred)

print("--- Métricas de Test ---")
print(metricas_test)

# Guardar el modelo ----
saveRDS(modelo_rf, "APP_ALQUILERES/rf_alquileres_Montevideo.rds")
