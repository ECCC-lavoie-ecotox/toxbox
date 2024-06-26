#' tables_nav UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_nav_tables_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::accordion(
      open = TRUE,
      bslib::accordion_panel(
        "Reference tables", icon = bsicons::bs_icon("database-fill"),
        lapply(get_golem_config("tables")$reference_tables, \(b){
          actionButton(ns(paste0("table-nav-",b)),
            class = "btn btn-outline-primary m-1",
            label = stringr::str_to_title(b)
          )
        })
      ),
      bslib::accordion_panel(
        "Field tables", icon = bsicons::bs_icon("feather"),
        lapply(get_golem_config("tables")$field_tables, \(b){
          actionButton(ns(paste0("table-nav-",b)),
            class = "btn btn-outline-primary m-1",
            label = stringr::str_to_title(b)
          )
        })
      ),
      bslib::accordion_panel(
        "Laboratory tables", icon = bsicons::bs_icon("menu-app"),
        lapply(get_golem_config("tables")$lab_tables, \(b){
          actionButton(ns(paste0("table-nav-",b)),
            class = "btn btn-outline-primary m-1",
            label = stringr::str_to_title(b)
          )
        })
      )
    )
  )
}
    
#' tables_nav Server Functions
#'
#' @noRd 
mod_nav_tables_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    tables <- unlist(get_golem_config("tables"))
    lapply(tables, \(t) {
      id <- paste0("table-nav-", t)
      observeEvent(input[[id]], {
        r$page <- t
      })
    })
  })
}
