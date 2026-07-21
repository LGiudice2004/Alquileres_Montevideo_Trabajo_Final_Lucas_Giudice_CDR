library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(readr)
library(tibble)
library(workflows) 
library(parsnip)

# Cargamos los datos ----
base <- read_delim("base.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
centroides_barrios <- read_delim("centroides_barrios.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
modelo_rf <- readRDS("rf_alquileres_Montevideo.rds")

opciones_moneda <- c("UYU($)", "USD(US$)")

# UI ----
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  ## Estética CSS ----
  tags$style(HTML("
    html, body { 
      margin: 0; padding: 0; 
      overflow-x: hidden; 
      overflow-y: auto !important;
      height: auto; 
      background-color: #f8f9fa;
      font-family: sans-serif;
    }
    
    .navbar-fija {
      position: fixed; top: 0; left: 0; width: 100%; height: 50px;
      background-color: #1a2a3a; color: white; z-index: 2000;
      display: flex; align-items: center; padding-left: 20px;
      box-shadow: 0 2px 5px rgba(0,0,0,0.2);
    }
    .navbar-fija h3 { margin: 0; font-size: 19px; font-weight: bold; color: #ffffff; }
    .texto-verde-agua { color: white; font-weight: normal !important; }
    
    .mapa-fondo {
      position: fixed; top: 50px; left: 0; width: 100vw; height: calc(100vh - 50px); 
      z-index: 1;
    }
    
    .contenido-desplazable {
      position: relative;
      width: 100%;
      min-height: 215vh; 
      z-index: 10;
      padding-top: 50px;
      pointer-events: none;
    }
    
    .panel-flotante-blanco {
      background-color: rgba(255, 255, 255, 0.96);
      border-radius: 8px;
      box-shadow: 0 6px 20px rgba(0,0,0,0.15);
      border: 1px solid rgba(0,0,0,0.05);
      pointer-events: auto;
    }
    
    .titulo-seccion-azul {
      color: #2c3e50; font-weight: bold; border-left: 4px solid #0052cc;
      padding-left: 10px; margin-top: 0;
    }
    
    .resultado-box {
      display: flex; flex-direction: column; justify-content: center;
      align-items: center; min-height: 120px; border-left: 2px solid #1abc9c;
    }
  ")),
  
  ## Cabezal UI ----
  div(class = "navbar-fija",
      h3(
        "Mercado Inmobiliario en Montevideo | ",
        span("Alquiler de Apartamentos", class = "texto-verde-agua")
      )
  ),
  
  ## Mapa UI ----
  div(
    class = "mapa-fondo",
    leafletOutput("mapa_alquileres", width = "100%", height = "100%")
  ),
  
  ### Contenido sobre el mapa ----
  div(
    class = "contenido-desplazable",
    
    #### Panel de Filtros ----
    div(
      class = "panel-flotante-blanco",
      style = "position: absolute; top: 65px; left: 5px; width: 360px; padding: 20px;",
      
      h4("Filtrar apartamentos", style = "margin-top:0; color:#2c3e50; font-weight:bold;"),
      hr(style = "margin-top:10px; margin-bottom:15px;"),
      
      # MONEDA Y DÓLAR
      fluidRow(
        column(
          width = 7,
          radioButtons(
            "moneda",
            "Moneda base:",
            choices = opciones_moneda,
            selected = opciones_moneda[1],
            inline = TRUE
          )
        ),
        column(
          width = 5,
          style = "padding-top: 0px;", 
          numericInput(
            "precio_dolar",
            "Dólar ($):",
            value = 40.00, 
            min = 20.00,
            max = 100.00,
            step = 0.10
          )
        )
      ),
      
      # BARRIOS
      selectizeInput(
        "barrios_mapa",
        "Barrios:",
        choices = c("Todos los barrios", sort(unique(as.character(base$barrio)))),
        selected = "Todos los barrios",
        multiple = TRUE,
        options = list(
          placeholder = "Seleccione uno o varios barrios",
          plugins = list("remove_button")
        )
      ),
      
      # PRECIO REACTIVO
      uiOutput("slider_precio_reactivo"),
      
      # METROS CUADRADOS
      sliderInput(
        "m2_mapa",
        "Metros cuadrados:",
        min = 20,
        max = 400,
        value = c(
          floor(quantile(base$metros_totales, .05, na.rm = TRUE)),
          ceiling(quantile(base$metros_totales, .25, na.rm = TRUE))
        )
      ),
      
      # DORMITORIOS
      checkboxGroupInput(
        "dormitorios",
        "Dormitorios:",
        choices = c("0", "1", "2", "3", "4+"),
        inline = TRUE
      ),
      
      # BAÑOS (Sin opción 0)
      checkboxGroupInput(
        "banos",
        "Baños:",
        choices = c("1", "2", "3", "4+"),
        inline = TRUE
      ),
      
      hr(style = "margin:10px 0;"),
      
      fluidRow(
        column(6, checkboxInput("garage", "Tiene garage", FALSE)),
        column(6, checkboxInput("balcon", "Tiene balcón", FALSE))
      ),
      
      # BÚSQUEDA AVANZADA 
      tags$details(
        style = "margin-top:15px;",
        tags$summary(
          style = "cursor:pointer; font-weight:bold; color:#2c3e50; font-size:15px;",
          "🔎 Búsqueda avanzada"
        ),
        tags$div(
          style = "margin-top:15px;",
          fluidRow(
            column(6, checkboxInput("frente", "Al frente", FALSE)),
            column(6, checkboxInput("mascotas", "Permite mascotas", FALSE))
          ),
          fluidRow(
            column(6, checkboxInput("terraza", "Terraza", FALSE)),
            column(6, checkboxInput("amueblado", "Amueblado", FALSE))
          ),
          fluidRow(
            column(6, checkboxInput("calefaccion", "Calefacción", FALSE)),
            column(6, checkboxInput("aire_acondicionado", "Aire acondicionado", FALSE))
          )
        )
      )
    ),
    
    ### PANEL DE PREDICCIÓN ----
    div(
      class = "panel-flotante-blanco",
      style = "position: absolute; top: 150vh; left: 2.5%; width: 95%; padding: 30px; margin-bottom: 5vh;",
      
      h3(class = "titulo-seccion-azul", "Estimador de Precio Medio"),
      p("Modificá los parámetros de la propiedad para calcular la predicción en tiempo real."),
      p(tags$i("Nota: El modelo fue entrenado con un dólar de referencia a $40 UYU. La predicción resultante se genera originalmente en pesos uruguayos (UYU)."), 
        style = "font-size: 12px; color: #7f8c8d; margin-top: -5px;"),
      hr(),
      
      fluidRow(
        column(
          width = 9,
          fluidRow(
            column(width = 4, selectizeInput("barrio_pred", "Barrio Objetivo:", choices = sort(unique(as.character(base$barrio))))),
            column(width = 4, numericInput("dormitorios_pred", "Dormitorios:", value = 1, min = 0, max = 5)),
            column(width = 4, numericInput("banos_pred", "Baños:", value = 1, min = 1, max = 5))
          ),
          fluidRow(
            column(width = 4, sliderInput("rango_m2_pred", "Rango Metros Cuadrados:", min = 20, max = 400, value = c(40, 70), step = 5)),
            column(width = 4, sliderInput("rango_gc_pred", "Rango Gastos Comunes (UYU):", min = 1000, max = 80000, value = c(3000, 6000), step = 500)),
            column(
              width = 4, 
              style = "padding-top: 35px;",
              fluidRow(
                column(width = 6, checkboxInput("balcon_pred", "¿Balcón?", value = FALSE)),
                column(width = 6, checkboxInput("garage_pred", "¿Garage?", value = FALSE))
              )
            )
          )
        ),
        column(
          width = 3,
          class = "resultado-box",
          style = "background: #f8f9fa; padding: 20px; border-radius: 8px; border: 1px solid #e9ecef; text-align: center;",
          span("PRECIO ESTIMADO MEDIO", style = "font-size: 11px; font-weight: bold; letter-spacing: 1px; color: #7f8c8d;"),
          uiOutput("precio_estimado_gigante")
        )
      )
    )
  )
)

# SERVER ----
server <- function(input, output, session) {
  
  ## Datos reactivos ----
  datos_filtrados <- reactive({
    datos <- base
    
    moneda_actual <- if (!is.null(input$moneda)) input$moneda else opciones_moneda[1]
    
    if (!is.null(input$barrios_mapa) && !("Todos los barrios" %in% input$barrios_mapa)) {
      datos <- datos |> filter(barrio %in% input$barrios_mapa)
    }
    
    tipo_cambio <- if (!is.null(input$precio_dolar) && is.numeric(input$precio_dolar) && input$precio_dolar > 0) {
      input$precio_dolar
    } else {
      40.00
    }
    
    ui_quiere_uyu <- grepl("UYU", moneda_actual, ignore.case = TRUE)
    ui_quiere_usd <- grepl("USD", moneda_actual, ignore.case = TRUE)
    
    datos <- datos |> 
      mutate(
        es_usd_propiedad = grepl("USD", moneda, ignore.case = TRUE),
        precio_convertido = case_when(
          (ui_quiere_uyu & !es_usd_propiedad) ~ precio,
          (ui_quiere_usd & es_usd_propiedad)  ~ precio,
          (ui_quiere_uyu & es_usd_propiedad)  ~ precio * tipo_cambio,
          (ui_quiere_usd & !es_usd_propiedad) ~ precio / tipo_cambio,
          .default = precio
        )
      )
    
    if (!is.null(input$precio_mapa) && length(input$precio_mapa) == 2) {
      datos <- datos |> 
        filter(
          precio_convertido >= input$precio_mapa[1],
          precio_convertido <= input$precio_mapa[2]
        )
    }
    
    if (!is.null(input$m2_mapa) && length(input$m2_mapa) == 2) {
      datos <- datos |> 
        filter(
          metros_totales >= input$m2_mapa[1],
          metros_totales <= input$m2_mapa[2]
        )
    }
    
    if (length(input$dormitorios) > 0) {
      if ("4+" %in% input$dormitorios) {
        datos <- datos |> filter(dormitorios %in% input$dormitorios | as.numeric(dormitorios) >= 4)
      } else {
        datos <- datos |> filter(dormitorios %in% input$dormitorios)
      }
    }
    
    if (length(input$banos) > 0) {
      if ("4+" %in% input$banos) {
        datos <- datos |> filter(baños %in% input$banos | as.numeric(baños) >= 4)
      } else {
        datos <- datos |> filter(baños %in% input$banos)
      }
    }
    
    if (isTRUE(input$garage)) { 
      datos <- datos |> filter(tolower(garage) %in% c("sí", "si")) 
    }
    if (isTRUE(input$balcon)) { 
      datos <- datos |> filter(tolower(balcon) %in% c("sí", "si")) 
    }
    
    if (isTRUE(input$frente)) { 
      datos <- datos |> filter(tolower(disposicion) == "si") 
    }
    if (isTRUE(input$mascotas)) { 
      datos <- datos |> filter(tolower(admite_mascotas) %in% c("sí", "si")) 
    }
    if (isTRUE(input$terraza)) { 
      datos <- datos |> filter(tolower(terraza) %in% c("sí", "si")) 
    }
    if (isTRUE(input$amueblado)) { 
      datos <- datos |> filter(tolower(amueblado) %in% c("sí", "si")) 
    }
    if (isTRUE(input$aire_acondicionado)) { 
      datos <- datos |> filter(tolower(aire_acondicionado) %in% c("sí", "si")) 
    }
    if (isTRUE(input$calefaccion)) { 
      datos <- datos |> filter(tolower(calefaccion) %in% c("sí", "si")) 
    }
    
    return(datos)
  })
  
  ### Mapa Base ----
  output$mapa_alquileres <- renderLeaflet({
    leaflet() %>% 
      setView(lng = -56.1645, lat = -34.9011, zoom = 12) %>%
      addProviderTiles(providers$OpenStreetMap.HOT)
  })
  
  ### Actualización del Mapa ----
  observe({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      leafletProxy("mapa_alquileres") %>% clearMarkers()
      return()
    }
    
    moneda_actual <- if (!is.null(input$moneda)) input$moneda else "UYU"
    
    leafletProxy("mapa_alquileres", data = datos) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~longitud_X,
        lat = ~latitud_Y,
        radius = 7,
        color = "#2c3e50",
        weight = 1,
        fillColor = "#0033aa",
        fillOpacity = 0.85,
        popup = ~paste0(
          "<b>Barrio:</b> ", barrio, "<br>",
          "<b>Precio:</b> ", ifelse(grepl("UYU", moneda_actual, ignore.case = TRUE), "$", "US$"), " ", format(round(precio_convertido), big.mark = ".", decimal.mark = ","), "<br>",
          "<b>Dormitorios:</b> ", dormitorios, "<br>",
          "<b>Baños:</b> ", baños, "<br>",
          "<b>Superficie:</b> ", metros_totales, " m²<br><br>",
          ifelse(!is.na(url), 
                 paste0("<a href='", url, "' target='_blank' style='background:#0033aa; color:white; padding:8px 12px; border-radius:6px; text-decoration:none; font-weight:bold; display:inline-block; text-align:center;'>Ver publicación</a>"), 
                 ""),
          "</div>"
        )
      )
  })
  
  ### Slider reactivo según Moneda ----
  output$slider_precio_reactivo <- renderUI({
    moneda_actual <- if (!is.null(input$moneda)) input$moneda else "UYU"
    
    if (grepl("UYU", moneda_actual, ignore.case = TRUE)) {
      sliderInput("precio_mapa", "Precio Alquiler (UYU):", min = 5000, max = 300000, value = c(20000, 45000))
    } else {
      sliderInput("precio_mapa", "Precio Alquiler (USD):", min = 250, max = 10000, value = c(500, 1100))
    }
  })
  
  ### Predicción con Random Forest ----
  output$precio_estimado_gigante <- renderUI({
    req(input$moneda, input$precio_dolar)
    
    barrio_sel  <- if (!is.null(input$barrio_pred)) input$barrio_pred else base$barrio[1]
    
    # Mínimo 1 baño, 0 dormitorios; Maximo 5 baños, 5 dormitorios
    dormitorios <- if (!is.null(input$dormitorios_pred)) min(max(as.numeric(input$dormitorios_pred), 0), 5) else 1
    baños       <- if (!is.null(input$banos_pred)) min(max(as.numeric(input$banos_pred), 1), 5) else 1
    
    balcon <- if (isTRUE(input$balcon_pred)) "Si" else "No"
    garage <- if (isTRUE(input$garage_pred)) "Si" else "No"
    
    rango_m2 <- if (!is.null(input$rango_m2_pred)) input$rango_m2_pred else c(40, 60)
    rango_gc <- if (!is.null(input$rango_gc_pred)) input$rango_gc_pred else c(3000, 5000)
    
    m2_medio <- mean(rango_m2)
    gc_medio <- mean(rango_gc)
    
    datos_barrio <- centroides_barrios |> 
      filter(BARRIO == barrio_sel) |> 
      select(-BARRIO)
    
    req(nrow(datos_barrio) > 0) 
    
    # Observación a predecir
    prediccion_nueva <- tibble(
      dormitorios = dormitorios,
      baños = baños,
      balcon = balcon,
      garage = garage,
      metros_totales = m2_medio,
      gastos_comunes = gc_medio
    ) |> 
      bind_cols(datos_barrio |> select(starts_with("dist_")))
    
    # Predicción en pesos
    prediccion <- predict(modelo_rf, new_data = prediccion_nueva)
    precio_base_pesos <- as.numeric(prediccion$.pred)
    
    # Conversión a la moneda seleccionada en la UI
    tipo_cambio <- if (is.numeric(input$precio_dolar) && input$precio_dolar > 0) input$precio_dolar else 40.00
    
    if (grepl("UYU", input$moneda, ignore.case = TRUE)) {
      valor_final <- precio_base_pesos
      simbolo <- "$ UYU"
    } else {
      valor_final <- precio_base_pesos / tipo_cambio
      simbolo <- "US$"
    }
    
    valor_formateado <- format(round(valor_final), big.mark = ".", decimal.mark = ",")
    
    tagList(
      span(paste0(simbolo, " ", valor_formateado), 
           style = "font-size: 36px; font-weight: bold; color: #2ecc71; margin: 5px 0; display: block; text-shadow: 0 1px 3px rgba(0,0,0,0.1);"),
      span("Monto Estimado / Mes", style = "font-size: 13px; color: #7f8c8d; font-weight: bold;")
    )
  })
}

# Correr app ----
shinyApp(ui, server)