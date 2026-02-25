# ------------------------
# Install
install.packages("shiny")
install.packages("shinydashboard")
install.packages("ggplot2")
install.packages("rlang")
install.packages(c("rlang", "ggplot2", "vctrs"), type = "binary")
packageVersion("rlang")
# ------------------------

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

# ------------------------
# UI - Interfaz de Usuario
# ------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Comorbilidad UNICA"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard & Hipótesis", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Agregar Estudiante", tabName = "agregar", icon = icon("user-plus")),
      menuItem("Consultar Estudiante", tabName = "consultar", icon = icon("search")),
      menuItem("Actualizar/Eliminar", tabName = "actualizar", icon = icon("edit")),
      menuItem("Mostrar Tabla", tabName = "mostrar", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Dashboard con Contraste de Hipótesis
      tabItem(tabName = "dashboard",
              fluidRow(
                box(width = 12, title = "Contraste de Hipótesis Estadístico", status = "info", solidHeader = TRUE,
                    column(width = 6,
                           tags$h4(tags$b("Hipótesis Nula (H0):")),
                           tags$p("No existe correlación entre el IMC y la Morbilidad. (ρ = 0)"),
                           tags$hr(),
                           tags$h4(tags$b("Hipótesis Alternativa (H1):")),
                           tags$p("Existe una correlación positiva entre el IMC y la Morbilidad. (ρ > 0)")
                    ),
                    column(width = 6,
                           tags$h4(tags$b("Interpretación Visual:")),
                           tags$p("Si la línea de tendencia roja es plana, mantenemos H0."),
                           tags$p("Si la línea tiene una pendiente ascendente clara, aportamos evidencia a favor de H1.")
                    )
                )
              ),
              fluidRow(
                valueBoxOutput("totalEstudiante"),
                valueBoxOutput("promEdad"),
                valueBoxOutput("morbilidadAlta")
              ),
              fluidRow(
                box(width = 8, title = "Análisis de Regresión: Contraste H0 vs H1", status = "success", solidHeader = TRUE, 
                    plotOutput("grafHipotesis")),
                box(width = 4, title = "Estado de Morbilidad", status = "warning", solidHeader = TRUE, 
                    plotOutput("grafMorbilidad"))
              ),
              fluidRow(
                box(width = 6, title = "Frecuencia por Enfermedad", status = "primary", solidHeader = TRUE, 
                    plotOutput("grafEnfermedad")),
                box(width = 6, status = "danger", 
                    actionButton("cerrar_app", "Cerrar Aplicación", 
                                 style = "color: white; background-color: red; width: 100%;"))
              )
      ),
      
      # Agregar Estudiante
      tabItem(tabName = "agregar",
              box(width = 6, status = "primary", solidHeader = TRUE, title = "Nuevo Registro",
                  numericInput("id", "ID", value = 1, min = 1),
                  textInput("nombre", "Nombre"),
                  selectInput("sexo", "Sexo", choices = c("Masculino", "Femenino", "Otro")),
                  numericInput("edad", "Edad", value = 18, min = 0),
                  dateInput("fecha", "Fecha de nacimiento", value = Sys.Date()),
                  numericInput("peso", "Peso (kg)", value = 0, min = 0),
                  numericInput("talla", "Talla (m)", value = 0, step = 0.01, min = 0),
                  selectInput("sangre", "Tipo de sangre", choices = c("A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-")),
                  numericInput("morbilidad", "Morbilidad (%)", value = 0, min = 0, max = 100),
                  textInput("enfermedad", "Enfermedad"),
                  textInput("medicamentos", "Medicamentos"),
                  textInput("alergias", "Alergias"),
                  textInput("contacto", "Contacto de emergencia"),
                  actionButton("add", "Agregar Paciente", style = "background-color: green; color: white;")
              )
      ),
      
      # Consultar
      tabItem(tabName = "consultar",
              box(width = 6, status = "info", solidHeader = TRUE, title = "Búsqueda por ID",
                  numericInput("id_consulta", "ID del Estudiante", value = 1),
                  actionButton("buscar", "Consultar"),
                  br(), br(),
                  verbatimTextOutput("resultado_consulta")
              )
      ),
      
      # Actualizar/Eliminar
      tabItem(tabName = "actualizar",
              box(width = 6, status = "warning", solidHeader = TRUE, title = "Modificar Registros",
                  numericInput("id_upd", "ID del Estudiante a Modificar", value = 1),
                  actionButton("buscar_upd", "Cargar Datos"),
                  hr(),
                  textInput("nombre_upd", "Nombre"),
                  selectInput("sexo_upd", "Sexo", choices = c("Masculino", "Femenino", "Otro")),
                  numericInput("edad_upd", "Edad", value = 0),
                  dateInput("fecha_upd", "Fecha nacimiento"),
                  numericInput("peso_upd", "Peso (kg)", value = 0),
                  numericInput("talla_upd", "Talla (m)", value = 0, step = 0.01),
                  selectInput("sangre_upd", "Tipo de sangre", choices = c("A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-")),
                  numericInput("morbilidad_upd", "Morbilidad (%)", value = 0),
                  textInput("enfermedad_upd", "Enfermedad"),
                  textInput("medicamentos_upd", "Medicamentos"),
                  textInput("alergias_upd", "Alergias"),
                  textInput("contacto_upd", "Contacto de emergencia"),
                  actionButton("actualizar", "Actualizar Registro", style = "background-color: orange; color: white;"),
                  actionButton("eliminar", "Eliminar Estudiante", style = "background-color: red; color: white;")
              )
      ),
      
      # Mostrar Tabla
      tabItem(tabName = "mostrar",
              box(width = 12, status = "success", solidHeader = TRUE, title = "Base de Datos",
                  DTOutput("tabla_Estudiante"),
                  downloadButton("export_csv", "Exportar a CSV")
              )
      )
    )
  )
)

# -------------------------
# SERVER - Lógica
# -------------------------
server <- function(input, output, session) {
  
  file_path <- "datos_estudiantes.csv"
  
  # Carga inicial
  initial_data <- if (file.exists(file_path)) {
    read.csv(file_path, stringsAsFactors = FALSE)
  } else {
    data.frame(ID=integer(), Nombre=character(), Sexo=character(), Edad=numeric(),
               FechaNacimiento=as.character(), Peso=numeric(), Talla=numeric(), IMC=numeric(),
               Sangre=character(), Morbilidad=numeric(), Enfermedad=character(),
               Medicamentos=character(), Alergias=character(), Contacto=character(),
               stringsAsFactors=FALSE)
  }
  
  Estudiante <- reactiveVal(initial_data)
  
  guardar_datos <- function(df) {
    write.csv(df, file_path, row.names = FALSE)
    Estudiante(df)
  }
  
  # AGREGAR
  observeEvent(input$add, {
    df <- Estudiante()
    peso_val <- if(is.na(input$peso)) 0 else input$peso
    talla_val <- if(is.na(input$talla)) 0 else input$talla
    imc <- if(talla_val > 0) round(peso_val / (talla_val^2), 1) else 0
    
    nuevo <- data.frame(
      ID = input$id, Nombre = input$nombre, Sexo = input$sexo, Edad = input$edad,
      FechaNacimiento = as.character(input$fecha), Peso = peso_val, Talla = talla_val,
      IMC = imc, Sangre = input$sangre, Morbilidad = input$morbilidad,
      Enfermedad = input$enfermedad, Medicamentos = input$medicamentos,
      Alergias = input$alergias, Contacto = input$contacto, stringsAsFactors = FALSE
    )
    guardar_datos(rbind(df, nuevo))
    showNotification("Estudiante agregado", type = "message")
  })
  
  # INDICADORES
  output$totalEstudiante <- renderValueBox({
    valueBox(nrow(Estudiante()), "Total Estudiantes", icon = icon("users"), color = "blue")
  })
  
  output$promEdad <- renderValueBox({ 
    df <- Estudiante()
    prom <- if(nrow(df) > 0) round(mean(df$Edad, na.rm = TRUE), 1) else 0
    valueBox(prom, "Edad Promedio", icon = icon("hourglass-half"), color = "purple") 
  })
  
  output$morbilidadAlta <- renderValueBox({ 
    df <- Estudiante()
    conteo <- if(nrow(df) > 0) sum(df$Morbilidad > 70, na.rm = TRUE) else 0
    valueBox(conteo, "Morbilidad Crítica (>70%)", icon = icon("exclamation-triangle"), color = "red") 
  })
  
  # CONSULTAR
  observeEvent(input$buscar,{
    df <- Estudiante()
    res <- df[df$ID == input$id_consulta, ]
    output$resultado_consulta <- renderPrint({ if(nrow(res) == 0) "No encontrado" else res })
  })
  
  # CARGAR PARA ACTUALIZAR
  observeEvent(input$buscar_upd,{
    df <- Estudiante()
    idx <- which(df$ID == input$id_upd)
    if(length(idx) == 0) { 
      showNotification("ID no encontrado", type = "warning")
      return() 
    }
    updateTextInput(session, "nombre_upd", value = df$Nombre[idx])
    updateSelectInput(session, "sexo_upd", selected = df$Sexo[idx])
    updateNumericInput(session, "edad_upd", value = df$Edad[idx])
    updateDateInput(session, "fecha_upd", value = as.Date(df$FechaNacimiento[idx]))
    updateNumericInput(session, "peso_upd", value = df$Peso[idx])
    updateNumericInput(session, "talla_upd", value = df$Talla[idx])
    updateSelectInput(session, "sangre_upd", selected = df$Sangre[idx])
    updateNumericInput(session, "morbilidad_upd", value = df$Morbilidad[idx])
    updateTextInput(session, "enfermedad_upd", value = df$Enfermedad[idx])
    updateTextInput(session, "medicamentos_upd", value = df$Medicamentos[idx])
    updateTextInput(session, "alergias_upd", value = df$Alergias[idx])
    updateTextInput(session, "contacto_upd", value = df$Contacto[idx])
  })
  
  # ACTUALIZAR
  observeEvent(input$actualizar,{
    df <- Estudiante()
    idx <- which(df$ID == input$id_upd)
    if(length(idx) == 0) return()
    imc <- if(input$talla_upd > 0) round(input$peso_upd/(input$talla_upd^2), 1) else 0
    df[idx,] <- list(input$id_upd, input$nombre_upd, input$sexo_upd, input$edad_upd, 
                     as.character(input$fecha_upd), input$peso_upd, input$talla_upd, 
                     imc, input$sangre_upd, input$morbilidad_upd, input$enfermedad_upd, 
                     input$medicamentos_upd, input$alergias_upd, input$contacto_upd)
    guardar_datos(df)
    showNotification("Registro actualizado", type = "message")
  })
  
  # ELIMINAR
  observeEvent(input$eliminar,{
    df <- Estudiante()
    idx <- which(df$ID == input$id_upd)
    if(length(idx) == 0) return()
    guardar_datos(df[-idx,])
    showNotification("Registro eliminado", type = "warning")
  })
  
  # GRÁFICOS
  output$tabla_Estudiante <- renderDT({ datatable(Estudiante(), options = list(scrollX = TRUE)) })
  
  output$export_csv <- downloadHandler(
    filename = function() { paste0("Estudiante_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(Estudiante(), file, row.names = FALSE) }
  )
  
  output$grafHipotesis <- renderPlot({
    df <- Estudiante()
    if(nrow(df) < 2) return(NULL)
    ggplot(df, aes(x = IMC, y = Morbilidad)) +
      geom_point(aes(color = Sexo), size = 4) +
      geom_smooth(method = "lm", color = "red", fill = "pink") +
      theme_minimal() + 
      labs(title = "Evidencia Empírica de la Relación Nutricional",
           x = "IMC (Indice de Masa Corporal)", 
           y = "Porcentaje de Morbilidad")
  })
  
  output$grafEnfermedad <- renderPlot({
    df <- Estudiante()
    if(nrow(df) == 0) return(NULL)
    ggplot(df, aes(x = Enfermedad)) + geom_bar(fill = "#3c8dbc") + theme_minimal()
  })
  
  output$grafMorbilidad <- renderPlot({
    df <- Estudiante()
    if(nrow(df) == 0) return(NULL)
    df$Estado <- ifelse(df$Morbilidad > 70, "Alta", "Normal")
    ggplot(df, aes(x = factor(1), fill = Estado)) + geom_bar(width = 1) + coord_polar("y") + theme_void()
  })
  
  observeEvent(input$cerrar_app,{ stopApp() })
}

# ---------------------- 
# Lanzar la APP
# ----------------------
shinyApp(ui, server)
