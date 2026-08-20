# ============================================================================
# APP: Análisis de imágenes de defecación
# ----------------------------------------------------------------------------
# Esta aplicación Shiny permite:
#   1) Seleccionar una carpeta con los CSV exportados desde Fiji/ImageJ.
#   2) Procesar esos CSV agrupándolos por patrón/grupo experimental y
#      generar un resumen por muestra y/o los datos punto a punto.
#   3) Descargar los datos procesados en Excel, eligiendo qué muestras y
#      parámetros incluir.
#   4) Correr tests estadísticos (t-test, ANOVA, no paramétricos, etc.)
#      sobre una variable elegida, con chequeo de supuestos (normalidad y
#      homogeneidad de varianzas) y recomendación automática de test.
#   5) Graficar los resultados (barras, boxplot, violin, scatter) con las
#      comparaciones estadísticas significativas marcadas, y descargar el
#      gráfico como PNG.
#
# La lógica de procesamiento, gráficos y estadística vive en los scripts
# externos que se cargan con source() más abajo; este archivo solo define
# la interfaz (ui) y la orquestación reactiva (server).
# ============================================================================


library(shiny)
library(shinyFiles)
library(shinyjs)
library(shinyWidgets)
library(zip)
library(ggplot2)
library(pzfx)

source("descarga_datos.R")  # Funciones para filtrar y exportar datos a Excel
source("graficos.R")        # Funciones que arman los gráficos (barras, boxplot, etc)
source("estadistica.R")     # Funciones de test estadísticos y supuestos


# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Análisis de imágenes - Fiji"),
  
  tags$script(HTML("
    Shiny.addCustomMessageHandler('copiarPortapapeles', function(texto) {
      navigator.clipboard.writeText(texto).catch(function(err) {
        console.error('Error al copiar:', err);
      });
    });
  ")),
  
  tabsetPanel(
    id = "tabs_principal",
    
    # ---- Pestaña 1: Configuración -------------------------------------------
    # Selección de carpeta, grupo control/experimentales, área ROI y
    # disparo del procesamiento de los CSV.
    tabPanel("Configuración",
      sidebarLayout(
        sidebarPanel(
          shinyDirButton("carpeta", "Seleccionar carpeta", "Elegir la carpeta donde están los CSV"),
          br(), br(),
          verbatimTextOutput("rutaSeleccionada"),
          hr(),
          uiOutput("ui_control"),  # Selector de grupo control
          uiOutput("ui_grupos"),   # Checkboxes de grupos experimentales
          hr(),
          
          radioButtons("modo_area", "Área ROI",
                       choices = c("Introducir un valor" = "manual",
                                   "Seleccionar archivo de áreas (.txt)" = "archivo"),
                       selected = "manual"),
          conditionalPanel(
            condition = "input.modo_area == 'manual'",
            numericInput("areaROI", "Área ROI (px²)", NA, min = 1)
          ),
          conditionalPanel(
            condition = "input.modo_area == 'archivo'",
            selectInput("archivo_areas_sel", "Archivo de áreas (.txt)", choices = NULL)
          ),
          
          checkboxInput("filtrar","Aplicar filtro por área",FALSE),
          checkboxGroupInput("archivos", "Archivos a generar", 
            choices = c(
              "Resumen por muestra" = "resumen",
              "Datos punto a punto" = "puntos"), selected = c("resumen", "puntos")),
          actionButton("procesar","Procesar datos"),
          hr(),
          uiOutput("ui_download")  # Botón de descarga, solo aparece si ya hay resultados
        ),
        mainPanel(
          h4("Resumen"),
          uiOutput("resumen")
        )
      )
    ),
    
    # ---- Pestaña 2: Estadística ---------------------------------------------
    # Elección de variable/grupos, chequeo de supuestos, elección y
    # ejecución de test, y visualización de resultados (incluye ANOVA de
    # dos factores como caso especial con inputs adicionales).
    tabPanel("Estadística",
      sidebarLayout(
        sidebarPanel(
          uiOutput("ui_stat_variable"),
          uiOutput("ui_stat_nivel"),
          uiOutput("ui_stat_grupos"),
          hr(),
          uiOutput("ui_stat_test_elegido"),
          uiOutput("ui_stat_modo_comparacion"),
          uiOutput("ui_stat_grupo_referencia"),
          
          # Inputs que solo tienen sentido si se eligió ANOVA de dos factores:
          # nombre y niveles de cada factor, para poder asignar cada patrón
          # a una combinación de niveles.
          conditionalPanel(
            condition = "input.stat_test_elegido == 'ANOVA de dos factores'",
            textInput("factor1_nombre", "Nombre del Factor 1", value = "Factor 1"),
            textInput("factor1_niveles", "Niveles del Factor 1 (separados por coma)",
                      placeholder = "ej: control, mutante"),
            textInput("factor2_nombre", "Nombre del Factor 2", value = "Factor 2"),
            textInput("factor2_niveles", "Niveles del Factor 2 (separados por coma)",
                      placeholder = "ej: vehiculo, droga")
          ),
          
          actionButton("ejecutar_test_btn", "Ejecutar test"),
          hr(),
          uiOutput("ui_btn_graficar")  # Aparece si el test corrió sin error
        ),
        mainPanel(
          h4("Supuestos"),
          tableOutput("tabla_normalidad"),
          verbatimTextOutput("texto_homogeneidad"),
          verbatimTextOutput("texto_recomendacion"),
          hr(),
          uiOutput("resultado_test_ui")
        )
      )
    ),
    
    # ---- Pestaña 3: Gráficos -------------------------------------------------
    # Visualización final de los datos ya testeados estadísticamente,
    # con distintos tipos de gráfico y opción de descarga en PNG.
    tabPanel("Gráficos",
     sidebarLayout(
       sidebarPanel(
         selectInput(
           inputId = "tipo_grafico",
           label = "Tipo de gráfico",
           choices = c(
             "Barras (media ± SEM)" = "barras",
             "Boxplot" = "boxplot",
             "Violin plot" = "violin",
             "Scatter plot" = "scatter"
           ),
           selected = "barras"
         ),
         conditionalPanel(
           condition = "input.tipo_grafico != 'scatter'",
           checkboxInput("grafico_mostrar_puntos","Mostrar puntos individuales",value = TRUE)
         ),
         checkboxInput("grafico_mostrar_sig", "Mostrar significancia", value = TRUE),
         hr(),
         downloadButton("descargar_grafico", "Descargar gráfico (PNG)")
       ),
       mainPanel(
         plotOutput("plot_grafico", height = "500px")
       )
     )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session){
  
  # ---- Pestaña Configuración: selección de carpeta -------------------------
  
  # Accesos rápidos (roots) que ofrece el selector de carpetas de Windows.
  volumes <- c(Escritorio = file.path(Sys.getenv("USERPROFILE"), "Desktop"),
               Usuario = Sys.getenv("USERPROFILE"))
  
  shinyDirChoose(input,"carpeta",roots = volumes,session = session, allowDirCreate = FALSE)
  
  # Ruta completa de la carpeta elegida (NULL hasta que el usuario elija una).
  ruta <- reactive({
    if (is.null(input$carpeta)) return(NULL)
    parseDirPath(volumes, input$carpeta)
  })
  
  # Estados reactivos centrales de la app: se resetean cada vez que cambia
  # algo que invalida los resultados ya calculados (carpeta, filtros, grupos).
  grupos              <- reactiveVal(NULL)   # Nombres de los patrones/grupos detectados en la carpeta
  resultados          <- reactiveVal(NULL)   # Resultado de "Resumen por muestra"
  tablas              <- reactiveVal(NULL)   # Tablas comparativas armadas a partir de "resultados"
  wb_puntos           <- reactiveVal(NULL)   # Resultados de "Datos punto a punto
  descarga_habilitada <- reactiveVal(FALSE)  # Controla si se muestra el botón de descarga
  
  # Al cambiar la carpeta: se limpia todo lo anterior y se recalculan los
  # grupos disponibles a partir de los archivos de la nueva carpeta.
  observeEvent(ruta(),{
    resultados(NULL)
    tablas(NULL)
    wb_puntos(NULL)
    descarga_habilitada(FALSE)
    req(ruta())
    req(dir.exists(ruta()))
    setwd(ruta())
    
    grupos_detectados <- nombres()
    grupos(grupos_detectados)
    if (length(grupos_detectados) == 0) {
      showNotification(
        "Esta carpeta no contiene archivos con el formato esperado (*.tif-results-azul.csv). Verifique que sea la carpeta correcta.",
        type = "warning",
        duration = NULL
      )
    }
    
    archivos_txt <- list.files(ruta(), pattern = "\\.txt$", full.names = FALSE)
    updateSelectInput(session, "archivo_areas_sel",
                      choices  = archivos_txt,
                      selected = if (length(archivos_txt) > 0) archivos_txt[1] else NULL)
  })
  
  # Selector de grupo control: se arma dinámicamente con los grupos
  # detectados en la carpeta elegida.
  output$ui_control <- renderUI({
    req(grupos())
    selectInput("control","Grupo control",choices=grupos())
  })
  
  # Checkboxes de grupos experimentales: son todos los grupos menos el
  # elegido como control. Incluye el checkbox "Seleccionar todos".
  output$ui_grupos <- renderUI({
    req(grupos(), input$control)
    checkboxGroupInput(
      "experimentales",
      label = tagList(
        "Grupos experimentales",
        tags$div(
          style = "font-weight: normal; margin-top: 4px;",
          checkboxInput("seleccionar_todos_grupos", "Seleccionar todos", value = FALSE)
        )
      ),
      choices = setdiff(grupos(), input$control)
    )
  })
  
  # Muestra solo el nombre de la carpeta (no la ruta completa) como feedback.
  output$rutaSeleccionada <- renderText({
    req(ruta())
    nombre_carpeta <- basename(ruta())
    paste(nombre_carpeta)
  })
  
  # Si cambia cualquier parámetro de entrada relevante (carpeta, área ROI,
  # filtro, control o grupos experimentales), los resultados calculados
  # anteriormente quedan obsoletos y se invalidan.
  observeEvent(
    list(
      ruta(),
      input$modo_area,
      input$areaROI,
      input$archivo_areas_sel,
      input$filtrar,
      input$control,
      input$experimentales
    ),
    {
      resultados(NULL)
      tablas(NULL)
      wb_puntos(NULL)
      descarga_habilitada(FALSE)
    },
    ignoreInit = TRUE
  )
  
  # -- Funcionalidad del botón "Seleccionar todos" para los grupos ------------
  # Estos dos "flags" evitan el loop infinito que se daría si el checkbox
  # maestro y el checkboxGroup se actualizaran mutuamente sin control:
  # cada vez que uno actualiza al otro mediante update*(), se marca el flag
  # correspondiente para que el observeEvent disparado por esa actualización
  # se ignore una única vez.
  ignorar_master <- reactiveVal(FALSE)
  ignorar_grupos <- reactiveVal(FALSE)  
  
  # Click en "Seleccionar todos" -> selecciona o deselecciona todos los
  # grupos experimentales disponibles.
  observeEvent(input$seleccionar_todos_grupos, {
    if (isTRUE(ignorar_master())) {
      ignorar_master(FALSE)
      return()
    }
    req(grupos(), input$control)
    opciones <- setdiff(grupos(), input$control)
    
    ignorar_grupos(TRUE)  # La próxima vez que se dispare el observerEvent de "experimentales", ignorarlo
    if (isTRUE(input$seleccionar_todos_grupos)) {
      updateCheckboxGroupInput(session, "experimentales", selected = opciones)
    } else {
      updateCheckboxGroupInput(session, "experimentales", selected = character(0))
    }
  })
  
  # Si el usuario tilda/destilda grupos a mano, se actualiza el estado del
  # checkbox "Seleccionar todos" para que refleje si están todos marcados.
  observeEvent(input$experimentales, {
    if (isTRUE(ignorar_grupos())) {
      ignorar_grupos(FALSE)
      return()
    }
    req(grupos(), input$control)
    opciones <- setdiff(grupos(), input$control)
    todos_seleccionados <- setequal(input$experimentales, opciones)
    
    if (!identical(isTRUE(input$seleccionar_todos_grupos), todos_seleccionados)) {
      ignorar_master(TRUE)  # La próxima vez que se dispare el observerEvent de "seleccionar_todos", ignorarlo
      updateCheckboxInput(session, "seleccionar_todos_grupos", value = todos_seleccionados)
    }
  }, ignoreNULL = FALSE)
  
  # La descarga se habilita apenas exista alguno de los dos resultados.
  observeEvent(list(resultados(), wb_puntos()), {
    descarga_habilitada(!is.null(resultados()) || !is.null(wb_puntos()))
  })
  
  
  # -- Procesamiento principal ------------------------------------------------
  
  observeEvent(input$procesar,{
    
    # Validaciones de los inputs necesarios antes de procesar.
    if (is.null(ruta()) || length(ruta()) == 0) {
      showNotification("Seleccione una carpeta.", type = "error")
      return()
    }
    if (is.null(input$control) || input$control == "") {
      showNotification("Seleccione un grupo control.", type = "error")
      return()
    }
    if (length(input$experimentales) == 0) {
      showNotification("Seleccione al menos un grupo experimental.", type = "error")
      return()
    }
    if (length(input$archivos) == 0) {
      showNotification("Seleccione al menos un archivo para generar.", type = "error")
      return()
    }
    if (input$modo_area == "manual") {
      
      if (is.null(input$areaROI) || is.na(input$areaROI)) {
        showNotification("Ingrese el Área ROI.", type = "error")
        return()
      }
      
    }else {
      
      if (is.null(input$archivo_areas_sel) || input$archivo_areas_sel == "") {
        showNotification("Seleccione un archivo de áreas ROI.", type = "error")
        return()
      }
      ruta_archivo_areas <- file.path(ruta(), input$archivo_areas_sel)
      
      tabla_areas_check <- tryCatch(
        read.table(ruta_archivo_areas, header = TRUE, sep = "\t", stringsAsFactors = FALSE),
        error = function(e) NULL
      )
      
      if (is.null(tabla_areas_check) ||
          !all(c("Archivo", "Area_cm2") %in% names(tabla_areas_check))) {
        showNotification(
          paste0("El archivo '", input$archivo_areas_sel,
                 "' no tiene las columnas necesarias (Archivo, Area_cm2)."),
          type = "error"
        )
        return()
      }
    }
    
    req(ruta())
    req(input$control)
    patrones <- c(input$control,input$experimentales)
    setwd(ruta())
    
    # Procesa los CSV según lo que el usuario haya tildado en "archivos a
    # generar": resumen por muestra y/o datos punto a punto.
    withProgress(message = "Procesando datos...", value = 0, {
      
      if("resumen" %in% input$archivos){
        incProgress(0.2, detail = "Procesando resumen por muestra...")
        
        res <- if (input$modo_area == "manual") {
          procesar_CSVs(
            patrones = patrones,
            area_ROI = input$areaROI,
            filtrar  = input$filtrar
          )
        } else {
          procesar_CSVs(
            patrones      = patrones,
            archivo_areas = file.path(ruta(), input$archivo_areas_sel),
            filtrar       = input$filtrar
          )
        }
        
        tab <- crear_tablas_comparativas(res)
        resultados(res)
        tablas(tab)
      }
      
      if("puntos" %in% input$archivos){
        incProgress(0.6, detail = "Procesando datos punto a punto...")
        
        res_puntos <- procesar_puntos(patrones = patrones, filtrar = input$filtrar)
        wb_puntos(res_puntos)
      }
      
      incProgress(1, detail = "Finalizado")
    })
  })
  
  # Resumen de la configuración usada y de qué archivos se generaron.
  output$resumen <- renderUI({
    req(!is.null(resultados()) || !is.null(wb_puntos()))
    archivos_generados <- c()
    
    archivos_generados <- c(archivos_generados,
      if (!is.null(resultados())) "✔ Resumen por muestra"  else "✖ Resumen por muestra")
    archivos_generados <- c(archivos_generados,
      if (!is.null(wb_puntos())) "✔ Datos punto a punto"   else "✖ Datos punto a punto")
    
    info_area <- if (input$modo_area == "manual") {
      tags$p(tags$b("Área ROI: "),
             format(input$areaROI, big.mark = ".", scientific = FALSE), " px²")
    } else {
      tags$p(tags$b("Archivo de áreas: "), input$archivo_areas_sel)
    }
    
    tagList(
      h4("Configuración utilizada"),
      tags$p(tags$b("Grupo control: "),input$control),
      tags$p(tags$b("Grupos experimentales: "),tags$ul(lapply(input$experimentales, tags$li))),
      info_area,
      tags$p(tags$b("Filtro por área: "),if (input$filtrar) "Sí" else "No"),
      hr(),
      
      h4("Archivos procesados"),
      tags$ul(lapply(archivos_generados, tags$li)),
      hr(),
      
      h4("Cantidad de archivos por grupo"),
      tableOutput("tabla_archivos")
    )
  })
  
  # Cantidad de archivos (o réplicas, en el caso de "puntos") por grupo.
  output$tabla_archivos <- renderTable({
    if (!is.null(resultados())) {
      data.frame(
        Grupo = names(resultados()$grupos),
        Archivos = sapply(resultados()$grupos, nrow),
        check.names = FALSE
      )
    } else {
      data.frame(
        Grupo = names(wb_puntos()$grupos),
        Archivos = sapply(wb_puntos()$grupos,function(df) length(unique(df$replica))),
        check.names = FALSE
      )
    }
  }, rownames = FALSE, striped = TRUE, bordered = TRUE, spacing = "s")

  
  # ---- Descarga de los datos ------------------------------------------------
  
  # Botón que abre el modal de descarga (solo visible si hay algo para descargar).
  output$ui_download <- renderUI({
    if (!descarga_habilitada()) return(NULL)
    actionButton("abrir_descarga", "Descargar datos")
  })
  
  # Modal con las opciones de descarga: tipo de archivo, muestras y
  # parámetros a incluir, más una previsualización de la tabla resultante.
  observeEvent(input$abrir_descarga, {
    req(patrones_disponibles()) 
    showModal(modalDialog(
      title = tags$div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        tags$h4("Opciones de descarga", style = "margin: 0;"),
        tags$button(
          type = "button",
          class = "close",
          `data-dismiss` = "modal",
          `aria-label` = "Close",
          tags$span(HTML("&times;"), `aria-hidden` = "true")
        )
      ),
      size = "l",
      easyClose = TRUE,
      
      radioButtons("descarga_tipo", "Tipo de archivo",
                   choices = c("Promedios por muestra" = "promedios",
                               "Datos punto a punto"   = "puntos",
                               "Ambos (combinados)"    = "ambos"),
                   selected = "promedios"),
      
      fluidRow(
        column(
          width = 6,
          h5("Muestras a incluir"),
          pickerInput(
            "descarga_muestras",
            label = NULL,
            choices = patrones_disponibles(),
            selected = patrones_disponibles(),
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,   # Seleccionar / deseleccionar todo
              `live-search` = TRUE,   # Buscador
              `selected-text-format` = "count > 3",
              size = 10
            )
          )
        ),
        
        column(
          width = 6,
          h5("Parámetros a incluir"),
          uiOutput("ui_parametros_descarga")
        )
      ),
      
      tags$hr(),
      
      # Ubicación de los botones
        tags$div(
          style = "display: flex; flex-direction: column; gap: 8px; align-items: flex-end;",
          uiOutput("ui_copiar_portapapeles"),
          tags$div(
            style = "display: flex; gap: 8px;",
            downloadButton("descargar_excel", "Descargar Excel"),
            downloadButton("descargar_pzfx", "Descargar GraphPad (.pzfx)")
          )
        ),
      
      tags$hr(),
      h5("Previsualización"),
      uiOutput("preview_muestras_info"),
      DT::dataTableOutput("preview_descarga"),
      
      footer = NULL
    ))
  })
  
  # Texto fijo arriba de la previsualización con los grupos elegidos: como
  # el picker de muestras es un desplegable, no siempre se ve qué quedó
  # tildado — esto lo deja visible mientras se revisa la previsualización.
  output$preview_muestras_info <- renderUI({
    req(input$descarga_muestras)
    tags$p(
      style = "color:#555; font-size: 90%; margin-bottom: 4px;",
      tags$b("Grupos incluidos: "), paste(input$descarga_muestras, collapse = ", ")
    )
  })
  
  # Lista de parámetros disponibles: depende de si se eligió "promedios",
  # "puntos" o "ambos" (cada uno tiene su propio set de variables, definido en
  # vars_promedios / vars_puntos dentro de descarga_datos.R). 
  output$ui_parametros_descarga <- renderUI({
    if (input$descarga_tipo == "ambos") {
      choices_prom <- setNames(vars_promedios, paste0(vars_promedios, " (por replica)"))
      choices_pap  <- setNames(vars_puntos,    paste0(vars_puntos, " (por punto)"))
      choices <- c(choices_prom, choices_pap)
      
      preseleccion <- intersect(
        c("DefTotal_Rel", "Densidad", "area.mm2", "Mean", "IntDen"),
        choices
      )
      
      pickerInput(
        "descarga_parametros",
        label = NULL,
        choices = choices,
        selected = preseleccion,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `selected-text-format` = "count > 3",
          size = 10
        )
      )
      
    } else {
      vars <- if (input$descarga_tipo == "promedios") vars_promedios else vars_puntos
      pickerInput(
        "descarga_parametros",
        label = NULL,
        choices = vars,
        selected = vars,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `selected-text-format` = "count > 3",
          size = 10
        )
      )
    }
  })
  
  # Previsualización reactiva: se recalcula ante cualquier cambio de
  # muestras, parámetros o tipo de archivo elegido en el modal.
  output$preview_descarga <- DT::renderDataTable({
    req(input$descarga_muestras, input$descarga_parametros, input$descarga_tipo)
    if (length(input$descarga_muestras) == 0 || length(input$descarga_parametros) == 0) {
      return(data.frame(Mensaje = "Seleccione al menos una muestra y un parámetro"))
    }
    if (input$descarga_tipo == "promedios") {
      req(resultados())
      filtrado <- filtrar_resultados(resultados(), input$descarga_muestras, input$descarga_parametros)
      do.call(rbind, lapply(names(filtrado$grupos), function(p) {
        cbind(Grupo = p, filtrado$grupos[[p]])
      }))
    } else if (input$descarga_tipo == "puntos") {
      req(wb_puntos())
      filtrado <- filtrar_puntos(wb_puntos()$grupos, input$descarga_muestras, input$descarga_parametros)
      do.call(rbind, filtrado)
    } else {
      # "ambos": promedios y puntos tienen estructuras distintas (una fila
      # por archivo vs. una fila por partícula), así que no se arma una
      # tabla cruda combinada — se muestra de dónde sale cada variable
      # elegida para que el usuario confirme la selección antes de bajar.
      parametros_prom <- intersect(input$descarga_parametros, vars_promedios)
      parametros_pap  <- intersect(input$descarga_parametros, vars_puntos)
      data.frame(
        Variable = c(parametros_prom, parametros_pap),
        Fuente = c(rep("Promedio por muestra", length(parametros_prom)),
                   rep("Punto a punto", length(parametros_pap))),
        check.names = FALSE
      )
    }
  }, options = list(pageLength = 5, scrollX = TRUE))
  
  # Funcionalidad botón "Copiar al portapapeles"
  # Reactivos que arman el texto a copiar
  tabla_puntos_texto <- reactive({
    req(input$descarga_muestras, input$descarga_parametros, wb_puntos())
    tabla <- tryCatch(
      construir_tabla_comparacion_puntos(
        lista_datos = filtrar_puntos(wb_puntos()$grupos, input$descarga_muestras, input$descarga_parametros),
        patrones    = input$descarga_muestras,
        vars_comparar = input$descarga_parametros
      ),
      error = function(e) {
        showNotification(paste("No se pudo generar la tabla:", e$message), type = "error", duration = 10)
        NULL
      }
    )
    req(tabla)
    paste(capture.output(write.table(tabla, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)), collapse = "\n")
  })
  
  tabla_promedios_texto <- reactive({
    req(input$descarga_muestras, input$descarga_parametros, resultados())
    tablas <- crear_tablas_comparativas(resultados())
    tablas_filtradas <- filtrar_tablas_comparativas(tablas, input$descarga_muestras, input$descarga_parametros)
    tabla <- tryCatch(
      construir_tabla_comparativa_promedios(tablas_filtradas),
      error = function(e) {
        showNotification(paste("No se pudo generar la tabla:", e$message), type = "error", duration = 10)
        NULL
      }
    )
    req(tabla)
    paste(capture.output(write.table(tabla, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)), collapse = "\n")
  })
  
  # UI del botón
  output$ui_copiar_portapapeles <- renderUI({
    req(input$descarga_tipo)
    if (input$descarga_tipo == "puntos") {
      actionButton("copiar_puntos", "Copiar al portapapeles", icon = icon("copy"))
    } else if (input$descarga_tipo == "promedios") {
      actionButton("copiar_promedios", "Copiar al portapapeles", icon = icon("copy"))
    }
  })
  
  observeEvent(input$copiar_puntos, {
    req(tabla_puntos_texto())
    session$sendCustomMessage("copiarPortapapeles", tabla_puntos_texto())
    showNotification("Tabla copiada al portapapeles.", type = "message")
  })
  
  observeEvent(input$copiar_promedios, {
    req(tabla_promedios_texto())
    session$sendCustomMessage("copiarPortapapeles", tabla_promedios_texto())
    showNotification("Tabla copiada al portapapeles.", type = "message")
  })
  
  # Genera y descarga el Excel final con los datos filtrados.
  output$descargar_excel <- downloadHandler(
    filename = function() paste0("datos_", input$descarga_tipo, "_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(input$descarga_muestras, input$descarga_parametros, input$descarga_tipo)
      withProgress(message = "Generando archivo Excel...", value = 0.3, {
        wb <- tryCatch(
          generar_excel_descarga(
            tipo = input$descarga_tipo,
            muestras = input$descarga_muestras,
            parametros = input$descarga_parametros,
            resultados = if (input$descarga_tipo %in% c("promedios", "ambos")) resultados() else NULL,
            wb_puntos = if (input$descarga_tipo %in% c("puntos", "ambos")) wb_puntos() else NULL
          ),
          error = function(e) {
            showNotification(paste("No se pudo generar el Excel:", e$message),
                             type = "error", duration = 10)
            NULL
          }
        )
        req(wb)
        incProgress(0.7, detail = "Guardando archivo...")
      saveWorkbook(wb, file, overwrite = TRUE)
      })
    }
  )
  
  # Descarga en formato GraphPad Prism (.pzfx): usa exactamente la misma
  # selección de muestras/parámetros que la descarga a Excel, pero arma cada
  # variable como una tabla ancha (grupos en columnas) tal como espera Prism.
  # No vuelve a procesar los CSV: reutiliza resultados()/wb_puntos() que ya
  # se calcularon al presionar "Procesar datos".
  output$descargar_pzfx <- downloadHandler(
    filename = function() paste0("datos_", input$descarga_tipo, "_", Sys.Date(), ".pzfx"),
    content = function(file) {
      req(input$descarga_muestras, input$descarga_parametros, input$descarga_tipo)
      withProgress(message = "Generando archivo GraphPad...", value = 0.3, {
        tablas <- tryCatch(
          generar_tablas_pzfx(
            tipo = input$descarga_tipo,
            muestras = input$descarga_muestras,
            parametros = input$descarga_parametros,
            resultados = if (input$descarga_tipo %in% c("promedios", "ambos")) resultados() else NULL,
            wb_puntos = if (input$descarga_tipo %in% c("puntos", "ambos")) wb_puntos() else NULL
          ),
          error = function(e) {
            showNotification(paste("No se pudo generar el archivo GraphPad:", e$message),
                             type = "error", duration = 10)
            NULL
          }
        )
        req(tablas)
        incProgress(0.6, detail = "Escribiendo el .pzfx (puede tardar)...")
        write_pzfx(tablas, file, row_names = FALSE)
      })
    }    
  )
  
  # ---- Pestaña Estadística --------------------------------------------------
  
  # Patrones/grupos disponibles para el análisis: vienen del resumen por
  # muestra si existe, o de los datos punto a punto en su defecto.
  patrones_disponibles <- reactive({
    if (!is.null(resultados())) return(resultados()$patrones)
    if (!is.null(wb_puntos())) return(names(wb_puntos()$grupos))
    NULL
  })
  
  # Selector de variable a analizar (definidas en variables_graficos,
  # cargado desde graficos.R).
  output$ui_stat_variable <- renderUI({
    selectInput("stat_variable", "Variable a analizar", choices = names(variables_graficos))
  })
  
  # Algunas variables existen tanto a nivel réplica como a nivel punto;
  # para esas se muestra este selector de "nivel" de análisis.
  output$ui_stat_nivel <- renderUI({
    req(input$stat_variable)
    info <- variables_graficos[[input$stat_variable]]
    if (!isTRUE(info$niveles)) return(NULL)
    radioButtons("stat_nivel", "Mostrar como",
                 choices = c("Promedio por réplica" = "replica",
                             "Todos los puntos" = "punto"))
  })
  
  # Nivel efectivo a usar: si la variable no ofrece elección, se asume
  # "replica" por defecto.
  nivel_stat <- reactive({
    if (is.null(input$stat_nivel)) "replica" else input$stat_nivel
  })
  
  # Checkboxes de grupos a comparar (todos los patrones disponibles,
  # tildados por defecto).
  output$ui_stat_grupos <- renderUI({
    patrones <- patrones_disponibles()
    if (is.null(patrones)) return(NULL)
    checkboxGroupInput("stat_grupos", "Grupos a comparar",
                       choices = patrones, selected = patrones)
  })
  
  # Nombre de la variable elegida, usado como etiqueta del eje Y en los gráficos.
  etiqueta_stat <- reactive({
    req(input$stat_variable)
    input$stat_variable
  })
  
  # Arma el data.frame final a analizar, según la variable, el nivel
  # (réplica/punto) y los grupos elegidos. Valida en cada paso que existan
  # los datos necesarios y que haya al menos 2 grupos para poder comparar.
  datos_stat <- reactive({
    req(input$stat_variable)
    info <- variables_graficos[[input$stat_variable]]
    
    if (!is.null(info$resumen) && !is.null(info$puntos)) {
      # La variable está disponible en ambos formatos: se elige según
      # el nivel seleccionado por el usuario.
      if (nivel_stat() == "replica") {
        validate(need(!is.null(resultados()),
                      "Para analizar esta variable por réplica primero debés procesar 'Resumen por muestra'."))
        datos <- preparar_datos_resumen(resultados(), info$resumen)
      } else {
        validate(need(!is.null(wb_puntos()),
                      "Para analizar todos los puntos primero debés procesar 'Datos punto a punto'."))
        datos <- preparar_datos_puntos(wb_puntos(), info$puntos, nivel = "punto")
      }
    } else if (!is.null(info$resumen)) {
      # La variable solo existe en el resumen por muestra.
      validate(need(!is.null(resultados()),
                    "Esta variable requiere procesar 'Resumen por muestra'."))
      datos <- preparar_datos_resumen(resultados(), info$resumen)
    } else {
      # La variable solo existe en los datos punto a punto.
      validate(need(!is.null(wb_puntos()),
                    "Esta variable requiere procesar 'Datos punto a punto'."))
      datos <- preparar_datos_puntos(wb_puntos(), info$puntos, nivel = nivel_stat())
    }
    
    validate(need(!is.null(datos), "No hay datos para analizar esta variable."))
    validate(need(!is.null(input$stat_grupos) && length(input$stat_grupos) > 0,
                  "Seleccioná al menos un grupo."))
    
    # Se queda solo con los grupos tildados por el usuario.
    datos <- datos[datos$patron %in% input$stat_grupos, ]
    datos$patron <- droplevels(datos$patron)
    
    validate(need(nrow(datos) > 0, "No hay datos para los grupos seleccionados."))
    validate(need(nlevels(datos$patron) >= 2, "Seleccioná al menos 2 grupos para poder comparar."))
    
    datos
  })
  
  # Chequeo de supuestos: normalidad por grupo y homogeneidad de varianzas
  # entre grupos, usados luego para recomendar el test más apropiado.
  normalidad_stat <- reactive({
    req(datos_stat())
    test_normalidad(datos_stat())
  })
  
  homogeneidad_stat <- reactive({
    req(datos_stat())
    test_homogeneidad(datos_stat())
  })
  
  recomendacion_stat <- reactive({
    req(datos_stat(), normalidad_stat(), homogeneidad_stat())
    n_grupos <- nlevels(datos_stat()$patron)
    recomendar_test(normalidad_stat(), homogeneidad_stat(), n_grupos)
  })
  
  output$tabla_normalidad <- renderTable({
    req(normalidad_stat())
    df <- normalidad_stat()
    data.frame(
      Grupo = as.character(df$patron),
      n = df$n,
      Estadistico = round(df$estadistico, 3),
      `p-valor` = signif(df$p_valor, 4),
      `¿Normal?` = ifelse(is.na(df$normal), "s/d (n insuficiente)",
                          ifelse(df$normal, "Sí", "No")),
      check.names = FALSE
    )
  }, rownames = FALSE, striped = TRUE, bordered = TRUE, spacing = "s", digits = 4)
  
  output$texto_homogeneidad <- renderText({
    req(homogeneidad_stat())
    h <- homogeneidad_stat()
    if (is.na(h$p_valor)) return("Homogeneidad de varianzas: no se pudo calcular.")
    paste0("Homogeneidad de varianzas (Levene): F = ", round(h$estadistico, 3),
           ", p = ", signif(h$p_valor, 4), " -> ",
           if (h$homogenea) "varianzas homogéneas" else "varianzas distintas")
  })
  
  output$texto_recomendacion <- renderText({
    req(recomendacion_stat())
    paste0("Test recomendado: ", recomendacion_stat())
  })
  
  # Selector de test: las opciones cambian según la cantidad de grupos
  # (2 grupos -> tests de comparación simple; 3+ -> ANOVA/Kruskal-Wallis/
  # ANOVA de dos factores). Se preselecciona el test recomendado según los
  # supuestos, si está entre las opciones disponibles.
  output$ui_stat_test_elegido <- renderUI({
    req(datos_stat())
    n_grupos <- nlevels(datos_stat()$patron)
    opciones <- if (n_grupos == 2) {
      c("t-test (Student)", "t-test de Welch", "Mann-Whitney (Wilcoxon)")
    } else {
      c("ANOVA de una vía", "Welch ANOVA", "Kruskal-Wallis", "ANOVA de dos factores")
    }
    recomendado <- tryCatch(recomendacion_stat(), error = function(e) opciones[1])
    selectInput("stat_test_elegido", "Test a realizar", choices = opciones,
                selected = if (recomendado %in% opciones) recomendado else opciones[1])
  })
  
  # Modo de comparación: solo aplica a los tests de >2 grupos con post-hoc de
  # a pares (no a los tests de 2 grupos ni al ANOVA de dos factores)
  output$ui_stat_modo_comparacion <- renderUI({
    req(datos_stat(), input$stat_test_elegido)
    n_grupos <- nlevels(datos_stat()$patron)
    if (n_grupos <= 2) return(NULL)
    if (!(input$stat_test_elegido %in% c("ANOVA de una vía", "Welch ANOVA", "Kruskal-Wallis"))) return(NULL)
    radioButtons("stat_modo_comparacion", "¿Qué comparar?",
                 choices = c("Todos contra todos" = "todos",
                             "Todos contra un grupo de referencia" = "control"),
                 selected = "todos")
  })
  
  # Si se eligió comparar contra un grupo de referencia, se pide cuál es.
  output$ui_stat_grupo_referencia <- renderUI({
    req(datos_stat())
    if (is.null(input$stat_modo_comparacion) || input$stat_modo_comparacion != "control") return(NULL)
    patrones <- levels(datos_stat()$patron)
    selectInput("stat_grupo_referencia", "Grupo de referencia", choices = patrones, selected = patrones[1])
  })
  
  resultado_test <- reactiveVal(NULL)
  
  # Ejecuta el test elegido al presionar el botón. Valida el grupo de
  # referencia (si corresponde) y arma la lista de "factores" con nombres
  # y niveles cuando el test es ANOVA de dos factores.
  observeEvent(input$ejecutar_test_btn, {
    req(datos_stat(), input$stat_test_elegido)
    
    modo <- if (is.null(input$stat_modo_comparacion)) "todos" else input$stat_modo_comparacion
    referencia <- if (modo == "control") input$stat_grupo_referencia else NULL
    
    if (modo == "control" && (is.null(referencia) || !nzchar(referencia))) {
      showNotification("Elegí un grupo de referencia.", type = "error")
      return()
    }
    
    factores <- NULL
    if (input$stat_test_elegido == "ANOVA de dos factores") {
      niveles_f1 <- trimws(strsplit(input$factor1_niveles, ",")[[1]])
      niveles_f2 <- trimws(strsplit(input$factor2_niveles, ",")[[1]])
      niveles_f1 <- niveles_f1[niveles_f1 != ""]
      niveles_f2 <- niveles_f2[niveles_f2 != ""]
      
      if (length(niveles_f1) < 2 || length(niveles_f2) < 2) {
        showNotification("Ingresá al menos 2 niveles para cada factor.", type = "error")
        return()
      }
      
      factores <- list(
        nombre_f1 = if (nzchar(input$factor1_nombre)) input$factor1_nombre else "Factor 1",
        nombre_f2 = if (nzchar(input$factor2_nombre)) input$factor2_nombre else "Factor 2",
        niveles_f1 = niveles_f1,
        niveles_f2 = niveles_f2
      )
    }
    
    res <- ejecutar_test(datos_stat(), input$stat_test_elegido, factores = factores, modo = modo, referencia = referencia)
    resultado_test(res)
  })
  
  # Si cambia la variable, el nivel, los grupos o el modo/referencia de
  # comparación, el resultado del test anterior queda obsoleto.
  observeEvent(list(input$stat_variable, input$stat_nivel, input$stat_grupos,
                    input$stat_modo_comparacion, input$stat_grupo_referencia), {
    resultado_test(NULL)
  }, ignoreInit = TRUE)
  
  # Arma dinámicamente el bloque de resultados según el tipo de test:
  # - Si hubo error, se muestra el mensaje de error.
  # - Si es ANOVA de dos factores, se muestra la tabla de efectos y,
  #   opcionalmente, un desplegable con la asignación de patrones a factores.
  # - Para el resto de los tests, se muestra estadístico, p-valor y si la
  #   diferencia es significativa.
  # En todos los casos se agrega un desplegable con el resumen de los datos
  # usados, y la tabla de comparaciones de a pares si hay post-hoc.
  output$resultado_test_ui <- renderUI({
    req(resultado_test())
    r <- resultado_test()
    
    if (!is.null(r$error)) {
      return(tags$p(style = "color:red;", r$error))
    }
    
    elementos <- list()
    
    if (!is.null(r$aviso)) {
      elementos <- c(elementos, list(tags$p(style = "color:#a66a00;", r$aviso)))
    }
    
    if (!is.null(r$tabla_factores)) {
      elementos <- c(elementos, list(
        h4("Resultado: ANOVA de dos factores"),
        tableOutput("tabla_efectos")
      ))
      if (!is.null(r$verificacion_factores)) {
        elementos <- c(elementos, list(
          tags$details(
            style = "margin-top: 10px; margin-bottom: 10px;",
            tags$summary(style = "cursor: pointer; color: #337ab7; font-weight: bold;",
              "▸ Cliqueá acá para ver la asignación de patrones a cada factor"),
            tags$div(style = "margin-top: 8px;", tableOutput("tabla_verificacion"))
          )
        ))
      }
    } else {
      elementos <- c(elementos, list(
        h4(paste("Resultado:", r$nombre)),
        tags$p(tags$b("Estadístico: "), round(r$estadistico, 3)),
        tags$p(tags$b("p-valor: "), signif(r$p_valor, 5)),
        tags$p(tags$b("¿Diferencia significativa? (p < 0.05): "),
               if (!is.na(r$p_valor) && r$p_valor < 0.05) "Sí" else "No")
      ))
    }
    
    elementos <- c(elementos, list(
      tags$details(
        style = "margin-top: 10px; margin-bottom: 10px;",
        tags$summary(style = "cursor: pointer; color: #337ab7; font-weight: bold;",
                     "▸ Ver resumen de los datos utilizados"),
        tags$div(style = "margin-top: 8px;", tableOutput("tabla_resumen_datos"))
      )
    ))
    
    if (!is.null(r$pares) && nrow(r$pares) > 1) {
      elementos <- c(elementos, list(
        h4("Comparaciones de a pares (post-hoc)"),
        tableOutput("tabla_pares")
      ))
    }
    
    tagList(elementos)
  })
  
  output$tabla_verificacion <- renderTable({
    req(resultado_test())
    r <- resultado_test()
    req(r$verificacion_factores)
    r$verificacion_factores
  })
  
  output$tabla_resumen_datos <- renderTable({
    req(resultado_test(), datos_stat())
    r <- resultado_test()
    req(is.null(r$error))
    stats <- resumen_estadistico(datos_stat())
    tabla_resumen_para_test(stats, r$nombre)
  }, rownames = FALSE, striped = TRUE, bordered = TRUE, spacing = "s", digits = 4)
  
  output$tabla_efectos <- renderTable({
    req(resultado_test())
    r <- resultado_test()
    req(r$tabla_factores)
    r$tabla_factores
  }, rownames = FALSE, striped = TRUE, bordered = TRUE, spacing = "s", digits = 4)
  
  output$tabla_pares <- renderTable({
    req(resultado_test())
    r <- resultado_test()
    req(r$pares)
    data.frame(
      Grupo1 = r$pares$grupo1,
      Grupo2 = r$pares$grupo2,
      `p ajustado` = signif(r$pares$p_ajustado, 4),
      Significancia = r$pares$etiqueta,
      check.names = FALSE
    )
  }, rownames = FALSE, striped = TRUE, bordered = TRUE, spacing = "s", digits = 4)
  
  # Botón "Graficar resultados": solo aparece si hay un test ya ejecutado
  # y sin errores.
  output$ui_btn_graficar <- renderUI({
    if (is.null(resultado_test()) || !is.null(resultado_test()$error)) return(NULL)
    actionButton("btn_graficar", "Graficar resultados")
  })
  
  # Paquete de datos que necesita la pestaña "Gráficos": los datos crudos
  # usados en el test, el resumen estadístico, la etiqueta del eje Y y las
  # comparaciones de a pares (si las hay), para poder marcar significancia.
  resultado_para_grafico <- reactiveVal(NULL)
  
  observeEvent(input$btn_graficar, {
    req(datos_stat(), resultado_test())
    resultado_para_grafico(list(
      datos = datos_stat(),
      stats = resumen_estadistico(datos_stat()),
      titulo_y = etiqueta_stat(),
      pares = resultado_test()$pares
    ))
    updateTabsetPanel(session, "tabs_principal", selected = "Gráficos")
  })
  
  # -- Pestaña Gráficos -------------------------------------------------------
  
  output$plot_grafico <- renderPlot({
    validate(need(!is.null(resultado_para_grafico()),
                  "Primero elegí una variable, ejecutá un test en la pestaña 'Estadística' y apretá 'Graficar resultados'."))
    r <- resultado_para_grafico()
    
    # Las comparaciones significativas solo se pasan al gráfico si el
    # usuario tildó "Mostrar significancia".
    pares <- if (isTRUE(input$grafico_mostrar_sig)) r$pares else NULL
    grafico_grupos(
      datos = r$datos,
      stats = r$stats,
      titulo_y = r$titulo_y,
      tipo = input$tipo_grafico,
      mostrar_puntos = input$grafico_mostrar_puntos,
      pares = pares
    )
  })
  
  # Descarga del gráfico como PNG (siempre usa el gráfico de barras con
  # error, independientemente del tipo elegido para la vista en pantalla).
  output$descargar_grafico <- downloadHandler(
    filename = function() {
      req(resultado_para_grafico())
      nombre_var <- gsub("[^A-Za-z0-9]+", "_", resultado_para_grafico()$titulo_y)
      paste0("Grafico_", nombre_var, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(resultado_para_grafico())
      r <- resultado_para_grafico()
      pares <- if (isTRUE(input$grafico_mostrar_sig)) r$pares else NULL
      p <- grafico_barras_error(r$datos, r$stats, titulo_y = r$titulo_y,
                                mostrar_puntos = input$grafico_mostrar_puntos, pares = pares)
      ggsave(file, plot = p, width = 7, height = 5, dpi = 300)
    }
  )
}

shinyApp(ui,server)