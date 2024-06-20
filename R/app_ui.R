#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
    # Leave this function for adding external resources
    golem_add_external_resources()
    shinyjs::useShinyjs()
    bslib::page_sidebar(
      window_title = "contaminR",
      theme = bslib::bs_theme(
          bootswatch = "zephyr",
          base_font = bslib::font_google("Poppins"),
          version = 5
      ),
      div(class = "col-auto d-none d-lg-block",
        img(class="img-fluid text-left d-inline", src="www/logo.png",height="100", width="100"),
        h3("ContaminR", class="d-inline", style="font-weight:bold;"),
        p("Database editor", class ="text-muted d-inline m-3")
      ),
      sidebar = bslib::sidebar(
        position = "left",
        actionButton("dashboard",
          class = "btn btn-success",
          label = "Dashboard",
          icon = icon("gauge")
        ),
        actionButton("documentation",
          class = "btn btn-success",
          label = "Documentation",
          icon = icon("book")
        ),
        mod_table_nav_ui("table_nav")
      ),
      mod_table_view_ui("table_viewer")
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
