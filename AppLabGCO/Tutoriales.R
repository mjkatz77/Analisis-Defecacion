# ---------- Pestaña tutorial de la App ----------

# En este código se configuran los tutoriales para las distintas pestañas 

tutorial_pages <- list(
  list(
    title = "Seleccionar carpeta",
    text  = shiny::tagList(
      shiny::p("Seleccioná la carpeta que contiene los CSV exportados de ImageJ/Fiji con el formato 'NOMBRE.tif-results-azul'."),
      shiny::tags$ul(
        shiny::tags$li(shiny::tags$b("A:"), " seleccionar el directorio donde esté guardada la carpeta."),
        shiny::tags$li(shiny::tags$b("B:"), " seleccionar la carpeta deseada."),
        shiny::tags$li(shiny::tags$b("C:"), " se muestra el contenido dentro de la carpeta seleccionada.")
      ),
    ),
    image = "imagenes/Config1_SelCarpeta.png"
  ),
  list(
    title = "Grupo control y grupos experimentales",
    text  = "El programa reconoce los grupos según el nombre de los CSVs, verifica que estos sean correctos. Seleccioná un grupo control y los grupos experimentales a comparar.",
    image = "imagenes/Config2_Grupos.png"
  ),
  list(
    title = "Área ROI",
    text  = "Podés ingresar un valor manual de área si todas las imagenes comparten el mismo ROI, o seleccionar un archivo .txt con las áreas calculadas por imagen.",
    image = "imagenes/Config3_ROI.png"
  ),
  list(
    title = "Procesar datos",
    text  = "Una vez seleccionados los parámetros, presioná 'Procesar datos'. Esto genera un resumen y habilita el botón 'Descargar datos'.",
    image = "imagenes/Config4_Procesar.png"
  ),
  list(
    title = "Descargar datos",
    text  = shiny::tagList(
      shiny::p("En esta pestaña seleccioná los datos que desees descargar. Entre las opciones se encuentran:"),
      shiny::tags$ul(
        shiny::tags$li(shiny::tags$b("Tipo de archivo:"), " si desea los datos por réplica, punto a punto, o ambos."),
        shiny::tags$li(shiny::tags$b("Muestras a incluir:"), " Selección de grupos entre los que fueron procesados."),
        shiny::tags$li(shiny::tags$b("Parámetros a incluir:"), " según el tipo elegido, podés seleccionar qué variables descargar (cantidad, densidad, etc.).")
      ),
      shiny::p("Tenés 3 formas de descargar:"),
      shiny::tags$ul(
        shiny::tags$li(shiny::tags$b("Copiar al portapapeles:"), " para pegar directo (copy-paste)."),
        shiny::tags$li(shiny::tags$b("Descargar Excel:"), " genera una tabla con los datos correspondientes."),
        shiny::tags$li(shiny::tags$b("Descargar GraphPad:"), " genera una tabla lista para pegar en tu plantilla.")
      )
    ),
    image = "imagenes/Config5_Descargar.png"
  )
)

tutorial_page_ui <- function(page) {
  shiny::tagList(
    shiny::h3(page$title),
    if (!is.null(page$image)) {
      shiny::tags$img(src = page$image, style = "max-width:100%; border:1px solid #ddd; border-radius:6px; margin-bottom:10px;")
    },
    page$text
  )
}

tutorial_modal_ui <- function(pages, current_page) {
  n <- length(pages)
  shiny::modalDialog(
    tutorial_page_ui(pages[[current_page]]),
    title = NULL, easyClose = TRUE, size = "l",
    footer = shiny::tagList(
      shiny::div(style = "display:flex; justify-content:space-between; width:100%; align-items:center;",
                 shiny::span(paste0(current_page, " / ", n), style = "color:#888;"),
                 shiny::div(
                   if (current_page > 1) shiny::actionButton("tutorial_prev", "\u2190 Anterior"),
                   if (current_page < n) shiny::actionButton("tutorial_next", "Siguiente \u2192"),
                   shiny::modalButton("Cerrar")
                 )
      )
    )
  )
}

tutorial_server <- function(input, output, session) {
  tutorial_page <- shiny::reactiveVal(1)
  shiny::observeEvent(input$help_btn, {
    tutorial_page(1)
    shiny::showModal(tutorial_modal_ui(tutorial_pages, tutorial_page()))
  })
  shiny::observeEvent(input$tutorial_next, {
    tutorial_page(tutorial_page() + 1)
    shiny::showModal(tutorial_modal_ui(tutorial_pages, tutorial_page()))
  })
  shiny::observeEvent(input$tutorial_prev, {
    tutorial_page(tutorial_page() - 1)
    shiny::showModal(tutorial_modal_ui(tutorial_pages, tutorial_page()))
  })
}




###### Tutorial pestaña de estadística
# Seleccionar variable a estudiar
# Seleccionar grupos a comparar
# Seleccionar test a realizar
 ##Nota: según el resultado de los supuestos, el programa recomienda un test u otro, sin embargo el usuario puede 
 ##      seleccionar el que desee
# Seleccionar como desea hacer la comparación (contra todos o contra referencia)
# Presionar "Ejecutar test"
# Mostrará el resultado del test, indicando significancia para cada caso