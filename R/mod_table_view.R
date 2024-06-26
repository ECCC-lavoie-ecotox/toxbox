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
  uiOutput(ns("table_view"))
}
#' table_view Server Functions
#'
#' @noRd 
mod_table_view_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$table_view <- renderUI({
      if(req(r$page) %in% unlist(get_golem_config("tables"))) {
        tagList(
          bslib::card(
              bslib::card_header(
                textOutput(ns("selected_table")),
                actionButton(
                    "Add entry", "Add", 
                    icon=icon("plus"), 
                    class="btn btn-success"
                  ), 
                class = "bg-dark d-flex justify-content-between align-items-center"
              ),
              bslib::card_body(
                reactable::reactableOutput(ns("table"))
              )
            )
        )
      }
    })

    observe({
      if (req(r$page) %in% unlist(get_golem_config("tables"))) {
        data_table <- get_tbl(con, r$page)
        output$table <- reactable::renderReactable({
          reactable::reactable(
            data_table,
            filterable = TRUE,
            resizable = TRUE,
            defaultPageSize = 100
          )
        })
      }
    })

    observeEvent(r$page, {
      if (req(r$page) %in% unlist(get_golem_config("tables"))) {
        output$selected_table <- renderText({
          paste(stringr::str_to_title(r$page), "table")
        })
      }
    })
  })
}
