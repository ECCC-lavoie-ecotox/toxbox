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
      h5(
        textOutput(ns("selected_table")),
        class="m-0",
        style="font-weight:bold;"
      ),
      actionButton(
          "Add entry", "Add", 
          icon=icon("plus"), 
          class="btn btn-success btn-sm"
        ), 
      class = "d-flex justify-content-between align-items-center"
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
      dataTable <- get_tbl(con, r$active_table)
      output$table <- reactable::renderReactable({
        reactable::reactable(
          dataTable,
          filterable = TRUE,
          searchable = TRUE,
          resizable = TRUE
        )
      })
    })

    observeEvent(r$active_table, {
      output$selected_table <- renderText({
          paste(stringr::str_to_title(r$active_table), "table")
      })
    })
  })
}
