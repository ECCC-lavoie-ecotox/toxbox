#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
    # Leave this function for adding external resources
    golem_add_external_resources()

    bslib::page_sidebar(
      title="Contaminant's database editor",
      window_title = "contaminR",
      theme = bslib::bs_theme(
          "bslib-dashboard-design" = "false",
          bootswatch = "minty",
          version = 5
      ),
      sidebar = bslib::sidebar(
        position = "left",
        actionButton("dashboard",
          class = "btn btn-outline-primary btn-sm",
          label = "Dashboard",
          icon = icon("gauge")
        ),
        actionButton("documentation",
          class = "btn btn-outline-primary btn-sm",
          label = "Documentation",
          icon = icon("book")
        ),
        bslib::accordion(
          bslib::accordion_panel(
            "Reference tables", icon = bsicons::bs_icon("database-fill"),
            lapply(get_golem_config("tables")$reference_tables, \(b){
              actionButton(paste0("table-nav-",b),
                class = "btn btn-outline-primary btn-sm m-1",
                label = stringr::str_to_title(b)
              )
            })
          ),
          bslib::accordion_panel(
            "Field", icon = bsicons::bs_icon("feather"),
            lapply(get_golem_config("tables")$field_tables, \(b){
              actionButton(paste0("table-nav-",b),
                class = "btn btn-outline-primary btn-sm m-1",
                label = stringr::str_to_title(b)
              )
            })
          ),
          bslib::accordion_panel(
            "Laboratory", icon = bsicons::bs_icon("menu-app"),
            lapply(get_golem_config("tables")$lab_tables, \(b){
              actionButton(paste0("table-nav-",b),
                class = "btn btn-outline-primary btn-sm m-1",
                label = stringr::str_to_title(b)
              )
            })
          )
        )
      ),
      mod_table_view_ui("selected-tab")
    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "toxbox"
    )
  )
}
