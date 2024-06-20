#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  r <- reactiveValues(
    active_table = "species"
  )

  mod_table_view_server("table_viewer", r)
  mod_table_nav_server("table_nav", r)
}
