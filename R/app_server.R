#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  r <- reactiveValues(
    page = "species"
  )

  mod_table_view_server("table_viewer", r)
  mod_nav_tables_server("table_nav", r)
  mod_search_server("search")

}
