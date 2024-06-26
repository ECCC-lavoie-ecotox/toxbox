#' search UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_search_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_fillable(
      bslib::layout_column_wrap(
        fill = FALSE,
        style = htmltools::css(grid_template_columns = "3fr 3fr 2fr"),
        bslib::card(
          bslib::card_header("Spatial criterias", class = "bg-dark"),
          bslib::card_body(
            mapedit::selectModUI(ns("map")),
            class = "p-0"
          )
        ),
        bslib::card(
          bslib::card_header("Search criterias", class = "bg-dark"),
          bslib::card_body(
            div(class = "container",
              div(
                class = "row",
                selectInput("species", "Species", choices = as.character(get_tbl(con, "species")$vernacular_en), multiple = TRUE, width = "33%"),
                selectInput("family_compound", "Family compound", choices = as.character(get_tbl(con, "analyte")$family), multiple = TRUE, width = "33%"),
                selectInput("compound", "Compound", choices = unique(as.character(get_tbl(con, "analyte")$name)), multiple = TRUE, width = "33%")
              ),
              div(
                class = "row",
                selectInput("tissue", "Sample tissue", choices = unique(as.character(get_tbl(con, "field_sample")$tissue)), multiple = TRUE, width = "50%"),
                selectInput("age", "Sample age", choices = unique(as.character(get_tbl(con, "field_sample")$age)), multiple = TRUE, width = "50%")
              ),
              div(
                class = "row",
                sliderInput("sampling_range", "Sampling period", min = min(as.Date(get_tbl(con, "field_sample")$collection_date), na.rm = TRUE), max = max(as.Date(get_tbl(con, "field_sample")$collection_date), na.rm = TRUE), value = c(min(as.Date(get_tbl(con, "field_sample")$collection_date), na.rm = TRUE), max(as.Date(get_tbl(con, "field_sample")$collection_date), na.rm = TRUE)), width = "100%")
              )
            )
          )
        ),
        bslib::card(
          bslib::card_header("Export", class = "bg-dark"),
          bslib::card_body(
            div(
              class = "container",
              div(class="row", 
                radioButtons(
                  "keep_na", 
                  "Keep unknown / NA values for corresponding filters?", 
                  c("yes" = "Yes", "no" = "No"),
                  inline = TRUE
                )
              ),
              div(class="row", 
                radioButtons(
                  "simplified", 
                  "Merge tables (simplify export version)", 
                  c("yes" = "Yes", "no" = "No"),
                  inline = TRUE
                )
              ),
              div(class="row", 
                selectInput("tables", "Include tables in export", choices = unname(unlist(get_golem_config("tables"))), multiple = TRUE, width = "100%")
              ),
              div(class="row", 
                div(class = "btn-group",
                  actionButton(ns("reset-filter"),
                    class = "btn btn-dark m-1",
                    icon = icon("arrows-rotate"), 
                    label = "Reset filters"),
                  downloadButton(ns("download"), "Export in xlsx", class = "btn btn-primary m-1")
                )
              )
            )
          )
        )
      ),
      bslib::layout_columns(
        bslib::card(
          bslib::card_header("Data", class = "bg-dark"),
          bslib::card_body(
            reactable::reactableOutput(ns("test"))
          )
        )      
      )
    )
  )
}
    
#' search Server Functions
#'
#' @noRd
mod_search_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sites_sf <- get_tbl(con, "sites") |>
      dplyr::filter(!is.na(lat) & !is.na(lon))

    map <- leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::addCircleMarkers(data = sites_sf, lng=~lon , lat=~lat, radius=3, color="#198754", opacity = 0.8)

    edits <- callModule(
        mapedit::editMod,
        leafmap = map,
        id = "map"
    )

    output$data <- reactable::renderReactable({
      reactable::reactable(
        get_tbl(con, "lab_measurement"),
        resizable = TRUE
      )
    })
  })
}
