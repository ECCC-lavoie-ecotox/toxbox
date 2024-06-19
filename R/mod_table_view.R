#' table_view UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_table_view_ui <- function(id){
  ns <- NS(id)
  bslib::card(
    bslib::card_header(
      "View and edit table",
      actionButton("Add entry", "Add", class="btn btn-sm", style = "align-self:flex-end;")
    ),
    bslib::card_body(
      reactable::reactableOutput(ns("table"))
    )
  )
}
#' table_view Server Functions
#'
#' @noRd 
mod_table_view_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observe({
      dataTable <- get_tbl(con, r$activeTable)
      output$table <- reactable::renderReactable({
        reactable::reactable(dataTable)
      })
    })
  })
}
    
## To be copied in the UI
# mod_table_view_ui("table_view_1")
    
## To be copied in the server
# mod_table_view_server("table_view_1")
