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
          full_screen = TRUE,
          bslib::card_header("Spatial criterias", 
            class = "d-flex justify-content-between align-items-center bg-dark",
            div(class="btn-group",
              actionButton(ns("reset_filter_spatial"),
                class = "btn btn-success btn-sm m-1",
                icon = icon("arrows-rotate"),
                label = "Reset filters"
              ),
              actionButton(ns("apply_filter_spatial"),
                class = "btn btn-primary btn-sm m-1",
                label = "Apply"
              )
            )
          ),
          bslib::card_body(
            mapedit::selectModUI(ns("map")),
            class = "p-0"
          )
        ),
        bslib::card(
          bslib::card_header(
            "Search criterias", 
            class = "d-flex justify-content-between align-items-center bg-dark",
            div(class="btn-group",
              actionButton(ns("reset_filter_fields"),
                class = "btn btn-success btn-sm m-1",
                icon = icon("arrows-rotate"),
                label = "Reset filters"
              ),
              actionButton(ns("apply_filter_fields"),
                class = "btn btn-primary btn-sm m-1",
                label = "Apply"
              )
            )
          ),
          bslib::card_body(
            div(
              class = "container",
              div(
                class = "row",
                selectInput(ns("species"), "Species", choices = as.character(get_tbl(con, "species")$vernacular_en), multiple = TRUE, width = "50%"),
                selectInput(ns("sites"), "Sites", choices = as.character(get_tbl(con, "sites")$id_site), multiple = TRUE, width = "50%"),
              ),
              div(
                class = "row",
                radioButtons(
                  ns("search_by"),
                  "Search by",
                  c("Family" = "family", "Compound" = "compound"),
                  selected = "family",
                  inline = TRUE,
                  width = "30%"
                ),
                uiOutput(ns("compound"))
              ),
              div(
                class = "row",
                selectInput(ns("tissue"), "Tissue sample", choices = unique(as.character(get_tbl(con, "field_sample")$tissue)), multiple = TRUE, width = "50%"),
                selectInput(ns("age"), "Age", choices = unique(as.character(get_tbl(con, "field_sample")$age)), multiple = TRUE, width = "50%")
              ),
              div(
                class = "row",
                sliderInput(ns("sampling_range"), "Sampling period", min = min(as.Date(get_tbl(con, "field_sample")$collection_date), na.rm = TRUE), max = max(as.Date(get_tbl(con, "field_sample")$collection_date), na.rm = TRUE), value = c(min(as.Date(get_tbl(con, "field_sample")$collection_date), na.rm = TRUE), max(as.Date(get_tbl(con, "field_sample")$collection_date), na.rm = TRUE)), width = "100%")
              )
            )
          )
        ),
        bslib::card(
          bslib::card_header("Options", class = "bg-dark"),
          bslib::card_body(
            div(
              class = "container",
              div(
                class = "row",
                selectInput(ns("tables"), "Include tables in export", choices = unname(unlist(get_golem_config("tables"))), multiple = TRUE, width = "100%")
              ),
              div(
                class = "row",
                div(class = "col-md-8",
                  "Merge tables?"
                ),
                div(class = "col-md-4",
                  shinyWidgets::materialSwitch(
                    inputId = ns("simplify"),
                    status = "primary",
                    value = FALSE
                  )
                )
              ),
              div(
                class = "row",
                div(class = "col-md-8",
                  "Keep unknown / NA values?"
                ),
                div(class = "col-md-4",
                  shinyWidgets::materialSwitch(
                    inputId = "keep_na",
                    status = "primary",
                    value = FALSE
                  )
                )
              )
            )
          )
        )),
        bslib::card(
          min_height = "600px",
          bslib::card_header("Data",
            downloadButton(ns("download"), "Export in xlsx", class = "btn btn-success btn-sm m-1"),
            class = "bg-dark d-flex justify-content-between align-items-center"
          ),
          bslib::card_body(
            reactable::reactableOutput(ns("data")) |> shinycssloaders::withSpinner()
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

    tables <- get_golem_config("tables") |>
      unlist() |>
      unname()
    
    init_datasets <- function(){  
        lapply(tables, \(x) dplyr::tbl(con, x) |> dplyr::collect()) |>
          setNames(tables)
    }

    db <- reactiveValues(data = init_datasets(), simplified = NULL)

    sites_sf <- get_tbl(con, "sites") |>
      dplyr::filter(!is.na(lat) & !is.na(lon)) |>
      sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(4326), remove = FALSE)

    edits <- callModule(
      mapedit::editMod,
      leafmap = leaflet::leaflet() |>
        leaflet::addTiles(group = "OpenStreetMap") |>
        leaflet::addCircleMarkers(data = sites_sf, lng = ~lon, lat = ~lat, radius = 3, color = "#66003b", opacity = 0.8, fill = TRUE) |>
        leaflet.extras::addDrawToolbar(
          polylineOptions = FALSE,
          circleMarkerOptions = FALSE,
          markerOptions = FALSE,
          editOptions = leaflet.extras::editToolbarOptions(edit = TRUE, remove = TRUE)
        ),
      id = "map",
      record = FALSE,
      sf = TRUE
    )

    observeEvent(input$search_by, {
      if (input$search_by == "family") {
        output$compound <- renderUI({
          selectInput(ns("family_compound"), "Family compounds", choices = as.character(db$data$analyte$family), multiple = TRUE, width = "70%")
        })
      } else {
        output$compound <- renderUI({
          selectInput(ns("compound"), "Specific compounds", choices = unique(as.character(db$data$analyte$name)), multiple = TRUE, width = "70%")
        })
      }
    })

    observe({
      nRes <- nrow(db$data$lab_measurement)
      if ( nRes == 0 ) {
        showNotification(
          glue::glue("Found { nRes } measurements with these criterias."),
          type = "warning"
        )
      } else {
        showNotification(
          glue::glue("Found { nRes } measurements with these criterias."),
          type = "message"
        )
      }
    })
      
    observeEvent(input$apply_filter_fields, {
      gen_query <- dplyr::tbl(
        con, dplyr::sql("
            SELECT * FROM lab_measurement
              LEFT JOIN analyte AS anal USING (id_analyte)
              LEFT JOIN lab_sample USING (id_lab_sample)
              LEFT JOIN lab_field_sample USING (id_lab_sample)
              LEFT JOIN field_sample USING (id_field_sample)
              LEFT JOIN report USING (id_report)
              LEFT JOIN project USING (id_project)
              LEFT JOIN species USING (id_species)")
      )

      # Set criterias
      ccond <- list(
        vernacular_en = input$species,
        id_site = input$sites,
        tissue = input$tissue,
        age = input$age,
        family = input$family_compound,
        name = input$compound
      ) |>
        purrr::discard(is.null) |>
        purrr::imap(
          function(slice, name) {
            colname <- rlang::sym(name)
            rlang::quo({{ colname }} %in% slice)
          }
        ) |>
        unname()

      # Execute textual criterias
      tmp_db <- gen_query |>
        dplyr::filter(!!!ccond) |>
        dplyr::collect()

      # Execute date range criterias
      if (length(input$sampling_range) == 2) {
        tmp_db <- tmp_db |>
          dplyr::mutate(collection_date = as.Date(collection_date)) |>
          dplyr::filter(collection_date >= input$sampling_range[1] & collection_date <= input$sampling_range[2])
      }

      # Split large db to original
      db$data <- purrr::map(tables, \(t){
          cn <- get_tbl_info(con, t)$name
          dplyr::select(tmp_db, any_of(cn)) |> dplyr::distinct()
      }) |> setNames(tables)
    
    })

    output$data <- reactable::renderReactable({
      reactable::reactable(
        db$data$lab_measurement,
        resizable = TRUE
      )
    })
  })
}
