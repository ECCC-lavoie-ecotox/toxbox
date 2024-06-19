#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  r <- reactiveValues(
    activeTable = "species"
  )

  observeEvent(input$`table-nav-sites`, {
    r$activeTable = "sites"
  })

  mod_table_view_server("selected-tab", r)
}
