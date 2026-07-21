# ARMADO DE LA BASE

# Librerias ----

library(dplyr)
library(leaflet)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(sf)
library(mapview)
library(janitor)

# Cargamos la base preeliminar ----

apartamentos_final <- read_csv("apartamentos_final.csv")

# Retiramos observaciones mal mapeadas ----
malas_observaciones <- apartamentos_final |> 
  filter(
    (
      (direccion == "Capurro 638, Montevideo, Capurro, Montevideo") |
        (direccion == "Maldonado, Palermo, Montevideo") |
        (direccion == "Buenos Aires 100, Ciudad Vieja, Montevideo") |
        (direccion == "Ciudad Vieja, Ciudad Vieja, Montevideo" & precio == "$ 33.900") |
        (direccion == "Paraguay 1000, Barrio Sur, Montevideo, Montevideo") |
        (direccion == "Rambla O´higgins 5100, Malvin, Montevideo") |
        (direccion == "Echev, Pocitos, Montevideo") |
        (direccion == "Puerto Del Buceo, Puerto Buceo, Montevideo" & (precio == "US$ 1.100" | precio == "US$ 3.290")) |
        (direccion == "Puerto Del Buceo, Buceo, Montevideo" & precio == "US$ 2.500") |
        (direccion == "Rambla Armenia 1624, Puerto Buceo, Montevideo") |
        (direccion == "Ciudad Vieja, Ciudad Vieja, Montevideo" & precio == "$ 24.900")
    )
  )

base <- anti_join(apartamentos_final, malas_observaciones)

# Asignamos la variable barrio de acuerdo a las variables geográficas ----
barrios_sf <- st_read(
  "v_sig_barrios.shp",
  options = "ENCODING=UTF-8"
)

barrios_sf <- barrios_sf |>
  mutate(
    BARRIO = BARRIO |>
      make_clean_names() |>
      str_replace_all("_", " ") |>
      str_to_title()
  )

barrios_sf <- barrios_sf |>
  mutate(
    BARRIO = recode(
      BARRIO,
      "Cordon" = "Cordón",
      "Parque Rodo" = "Parque Rodó",
      "Malvin" = "Malvín",
      "Malvin Norte" = "Malvín Norte",
      "Union" = "Unión",
      "Ituzaingo" = "Ituzaingó",
      "Penarol Lavalleja" = "Peñarol",
      "Banados De Carrasco" = "Bañados de Carrasco",
      "Villa Espanola" = "Villa Española",
      "Villa Munoz Retiro" = "Villa Muñoz",
      "Pque Batlle Villa Dolores" = "Parque Batlle Villa Dolores",
      "Mercado Modelo Y Bolivar" = "Mercado Modelo",
      "Colon Centro Y Noroeste" = "Colón",
      "Colon Sureste Abayuba" = "Colón",
      "Nuevo Paris" = "Nuevo París",
      "Paso De Las Duranas" = "Paso de las Duranas",
      "Paso De La Arena" = "Paso de la Arena",
      "Conciliacion" = "Conciliación",
      "Flor De Maronas" = "Flor de Maroñas",
      "Larranaga" = "Larrañaga",
      "Maronas Parque Guarani" = "Maroñas Parque Guarani",
      "Tres Ombues Pblo Victoria" = "Tres Ombúes Pueblo Victoria"
      
    )
  )

base_sf <- base |>
  mutate(
    latitud_Y = parse_double(latitud_Y),
    longitud_X = parse_double(longitud_X)
  ) |>
  filter(
    !is.na(latitud_Y),
    !is.na(longitud_X)
  ) |>
  st_as_sf(
    coords = c("longitud_X", "latitud_Y"),
    crs = 4326,
    remove = FALSE
  ) |>
  st_transform(st_crs(barrios_sf))

base_join <- st_join(
  base_sf,
  barrios_sf["BARRIO"],
  join = st_within
) |> 
  mutate(BARRIO = str_to_title(BARRIO))

base <- base_join |>
  mutate(barrio = BARRIO) |>
  select(-BARRIO) |>
  st_drop_geometry()

# Verificamos los tipos de las variables ----

base |> glimpse()

# Definimos las variables de precios o de superficie como numericas ----
base <- base |> 
  mutate(
    precio = precio %>%
      str_remove("^US\\$\\s*") %>%
      str_remove("^\\$\\s*") %>%
      str_remove_all("\\.") %>%
      as.numeric(),
    
    gastos_comunes = gastos_comunes %>%
      str_remove("\\s*UYU") %>%
      str_remove_all("\\.") %>%
      na_if("") %>%
      as.numeric(),
    
    metros_totales = metros_totales %>%
      str_remove("\\s*m²") %>%
      na_if("") %>%
      as.numeric(),
    
    area_privada = area_privada %>%
      str_remove("\\s*m²") %>%
      na_if("") %>%
      as.numeric(),
    longitud_X = as.numeric(longitud_X),
    latitud_Y = as.numeric(latitud_Y)
  )

# Definimos las variables (salvo url y direccion) como caracter para la visualización
base <- base |> 
  mutate(across(c(moneda, dormitorios, baños, garage, admite_mascotas, disposicion, balcon, terraza,
                  ascensor, amueblado, aire_acondicionado, calefaccion, numero_piso, barrio), ~ as.character(.x)))

# Generamos conteos de variables para verificar consistencia y sentido en los datos ----
vars_cat <- base |> 
  select(where(~ is.character(.x) | is.factor(.x)))

conteos <- vector("list", ncol(vars_cat))
names(conteos) <- names(vars_cat)

for(i in seq_along(vars_cat)){
  
  conteos[[i]] <- vars_cat |> 
    count(.data[[names(vars_cat)[i]]], sort = TRUE)

}

# Verificamos aquellos datos con moneda no especificada
# Se observa que no tienen precio, al considerarla una variable fundamental para el análisis a realizar,
# y tratandose de pocos datos, los eliminamos
conteos$moneda
base |> filter(moneda == "No especificado")
base <- base |> filter(moneda != "No especificado")

# Verificamos el conteo de dormitorios, contrartamos con los datos de la web oficial y se decide eliminar las 
# dos últimas observaciones mostradas (de 5 por ser pensión y la de 25 porque se considera error de la publicación)
conteos$dormitorios
base |>  filter(dormitorios %in% c("5", "6", "25")) |>  select(precio, dormitorios, direccion, barrio)
base <-  base |> 
  filter(!((direccion == "Carlos Reyles 1831, La Comercial, Montevideo" & dormitorios == "5")) & dormitorios != "25")

# Se eliminan obsevaciones que se verifica error en la cantidad de baños
conteos$baños
base |>  filter(baños %in% c("5", "6", "11", "-17")) |>  select(precio,moneda, baños, direccion, barrio)
base <-  base |>  filter(!(baños %in% c("11", "-17")))

# Valores observados coherentes a lo corroborado
conteos$garage
base |>  filter(garage %in% c("3", "4")) |>  select(precio,moneda, garage, direccion, barrio)

# Imputamos todas las categorias que no sean frente, convirtiendo la variable en una indicatriz de frente
conteos$disposicion
base <-  base |>
  mutate(disposicion = ifelse(disposicion == "Frente", "Si", "No"))

# Imputamos las siguientes no especificaciones suponiendo que ante la no especificación el valor real 
# es la negativa. En el caso de la variable balcon hay valores describiendo los metros cuadrados erroneamente,
# pero indicando que hay un balcon disponible, corregimos en favor de "Si". 
base <- base |> 
  mutate(
    balcon = case_when(
      balcon %in% c("No", "No especificado") ~ "No",
      balcon == "Si" ~ "Si",
      TRUE ~ "Si"
    ),
    terraza = if_else(terraza == "No especificado", "No", terraza),
    amueblado = if_else(amueblado == "No especificado", "No", amueblado),
    aire_acondicionado = if_else(aire_acondicionado == "No especificado", "No", aire_acondicionado),
    calefaccion = if_else(calefaccion == "No especificado", "No", calefaccion)
  )

# Imputamos la no especificación de ascensor suponiendo que ante la no especificación 
# el valor real es la positiva
base <- base |> 
  mutate(
    ascensor = if_else(ascensor == "No especificado", "Si", ascensor)
  )

# Convertimos garage en una variable binaria
base <- base |> 
  mutate(
    garage = if_else(garage == "0", "No", "Si")
  )

# Convertimos numero de piso en numerica para la visualización
base <- base |> 
  mutate(
    numero_piso = as.numeric(numero_piso)
  ) 
# Los valores 1000, 500 y 100 los imputamos como piso 10, 5, y 1
base <- base |> 
  mutate(
    numero_piso = case_when(numero_piso == 1006 ~ 10,
                            numero_piso == 501 ~ 5,
                            numero_piso == 110 ~ 1,
                            .default = numero_piso)
  ) 

# Unica variable que no imputamos es la aceptación de mascotas. Se considera lógico que pueda no haberse aclarado
# dicho atributo a pesar de poder llegar a permitirlo. No así con por ejemplo tener aire acondicionado, es decir,
# se presupone que quien alquila si tiene aire acondicionado lo declara. 

# Ajustamos nuevamente los tipos de las variables ----

base <- base |> 
  mutate(across(c(moneda, garage, admite_mascotas, disposicion, balcon, terraza,
                  ascensor, amueblado, aire_acondicionado, calefaccion, barrio), ~ as.character(.x)),
         across(c(precio, metros_totales, area_privada, dormitorios, baños), ~ as.numeric(.x)))

# Retiramos observaciones con 0 baños ----
base <- base |> 
  filter(baños != 0)

# Filtramos aquellas observaciones que tengan gastos comunes poco razonables ----
base <- base |> 
  filter(gastos_comunes >= 250)

# Quitamos Na en dormitorios ----

base <- base |> 
  filter(!is.na(dormitorios))

# Editamos la moneda ----

base <- base |> 
  select(-tipo) |> 
  mutate(moneda = ifelse(moneda == "Pesos Uruguayos (UYU)", "UYU($)", "USD(US$)" ))

# Mapa preeliminar para visualizar observaciones raras ----

apartamentos_map <- base |> 
  mutate(
    latitud_Y = as.numeric(latitud_Y),
    longitud_X = as.numeric(longitud_X)
  ) |> 
  filter(!is.na(latitud_Y),
         !is.na(longitud_X))

leaflet(apartamentos_map) |> 
  addProviderTiles(providers$CartoDB.Positron) |> 
  addCircleMarkers(
    lng = ~longitud_X,
    lat = ~latitud_Y,
    radius = 5,
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste0(
      "<b>Precio:</b> ", precio,
      "<br><b>Barrio:</b> ", barrio,
      "<br><b>Dirección:</b> ", direccion,
      "<br><b>Dormitorios:</b> ", dormitorios,
      "<br><b>Baños:</b> ", baños,
      "<br><a href='", url, "' target='_blank'>Ver publicación</a>"
    )
  )

# Quitamos valores de metraje que parezcan ilígicos o por muy pequeños o muy grandes.
# Tras un análisis comparativo en la página base se decide extraer los menores a 20 metros cuadrados totales 
# y los mayores de 400 metros cuadrados. Se quitan también aquellos que no tienen metraje.

base <- base |> 
  filter(metros_totales %in% c(20:400)) |> 
  drop_na(metros_totales)

# También se decide que se van a retirar las opciones que tengan duplicados para un numero de catergorias particulares
# ya que se han observado casos que con una distnta dirección se trata de la misma publicación. Se toman como identificador
# los datos de precio, moneda, dormitorios, barrio, baños, metros totales.

base <- base |> 
  distinct(precio, moneda, barrio, metros_totales, area_privada, dormitorios, baños, garage, .keep_all = TRUE) 

# Excluimos aquellas observaciones cuya area privada sea menor a los metros totales, no guarda sentido.
base <- base |> 
  filter(metros_totales >= area_privada)

# Eliminamos las observaciones que tengan precios que son más plausible para la compra de las propiedades ----
# El umbral máximo lo establecemos en 20000 dólares o su simil en pesos con un dolar a 40.

base <- base |> 
  filter((precio <= 20000 & moneda == "USD(US$)") | (precio <= (20000*40) & moneda == "UYU($)"))

# Agregamos variables de distancia a avenidas y rambla para el random forest ----

# La idea fue obtener para cada punto x e y que nosotros tenemos en el data frame la distancia lineal 
# respecto de ciertas avenidas o vías de interés que pueden ser relevantes a la hora de 
# alimentar un modelo predictivo para precio medio de determinados alquileres.
vias_sf <- st_read(
  "v_sig_vias.shp",
  options = "ENCODING=UTF-8"
)

vias_sf <- vias_sf |> 
  group_by(NOM_CALLE, TIPO) |> 
  summarise(geometry = st_union(geometry)) |> 
  ungroup() |> 
  filter(NOM_CALLE %in% c("AV ITALIA", "AV 8 DE OCTUBRE", "AV 18 DE JULIO", 
                          "AV AGRACIADA", "BV JOSE BATLLE Y ORDOÑEZ", "BV GRAL ARTIGAS",
                          "AV GRAL EUGENIO GARZON", "AV BOLIVIA"))

municipios_sf <- st_read(
  "sig_municipios.shp",
  options = "ENCODING=UTF-8"
) |> 
  filter(MUNICIPIO %in% c("B", "CH", "E"))

rambla_estructurada <- vias_sf |> 
  filter(TIPO == "RAMBLA") |> 
  st_intersection(municipios_sf) |> 
  st_union() |> 
  st_as_sf() |> 
  mutate(
    NOM_CALLE = "RAMBLA MONTEVIDEO",
    TIPO = "RAMBLA"
  ) |> 
  rename(geometry = x) 
 
vias_finales <- vias_sf |> 
  bind_rows(rambla_estructurada)

base_sf <- st_as_sf(
  base, 
  coords = c("longitud_X", "latitud_Y"), 
  crs = 4326,                            
  remove = FALSE                     
)

base_sf <- st_transform(base_sf, st_crs(vias_finales))

nombres_vias <- unique(vias_finales$NOM_CALLE)
for (via in nombres_vias) {
  
  geom_via <- vias_finales |> 
    filter(NOM_CALLE == via) |> 
    st_geometry() |> 
    st_union()
  
  nombre_columna <- paste0("dist_", tolower(gsub(" ", "_", via)))
  
  base[[nombre_columna]] <- as.numeric(st_distance(base_sf, geom_via))
}

# Calculamos las distancias de las vías seleccionadas a los centroides de los barrios ----

# La idea de esta base es la de tener los centroides de los barrios y sus distancias a las vías, de tal forma de que
# al seleccionar un barrio en el predictor, tengamos los insumos correspondientes a las distancias.

centroides_sf <- barrios_sf |> 
  st_centroid()

centroides_barrios <- centroides_sf |> 
  st_drop_geometry() |> 
  as_tibble()

nombres_vias <- unique(vias_finales$NOM_CALLE)

for (via in nombres_vias) {
  geom_via <- vias_finales |> 
    filter(NOM_CALLE == via) |> 
    st_geometry() |> 
    st_union()
  
  nombre_columna <- paste0("dist_", tolower(gsub(" ", "_", via)))
  
  centroides_barrios[[nombre_columna]] <- as.numeric(st_distance(centroides_sf, geom_via))
}

# Mantiene solo la primera fila si un barrio está repetido
centroides_barrios <- centroides_barrios |> 
  select(-GID, -NROBARRIO, -CODBA) |> 
  mutate(BARRIO = case_when(
    BARRIO == "Paso de las Duranas" ~ "Paso De Las Duranas",
    BARRIO == "Bañados de Carrasco" ~ "Bañados De Carrasco",
    tolower(BARRIO) == "colón" | BARRIO == "Colon" ~ "Colón",
    .default = BARRIO
  )) |> 
  distinct(BARRIO, .keep_all = TRUE) 

# Guardamos las bases ----
write.table(
  base,
  "APP_ALQUILERES/base.csv",
  sep = ";",
  row.names = FALSE
)

write.table(
  centroides_barrios,
  "APP_ALQUILERES/centroides_barrios.csv",
  sep = ";",
  row.names = FALSE
)
