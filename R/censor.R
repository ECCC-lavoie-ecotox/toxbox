#' Detect censored data
#'
#' @description For each column specified, censored data are detected by the presence of the symbol [<] . This function returned a data.frame with two columns by
#'
#' @param template a template. Use "?" to list available template.
#' @param data data passed to [whisker::whisker.render()].
#' @param ... further arguments passed to [rmarkdown::render()].
#'
#' @return The return value, if any, from executing the function.
#' @examples
#' \dontrun{
#' custom_render("report1",
#'   data = list(title = "templex"),
#'   envir = list(geom = 1)
#' )
#' }
uncensored <- function(df = NULL, cols = c(NULL), keep_cens = TRUE) {

  if (!is.data.frame(df)) cli::cli_abort("df must be a dataframe")
  if (!all(cols %in% names(df))) cli::cli_abort("Some specified columns")

  if (keep_cens) {
    df <- df |> dplyr::mutate(
      tidyselect::all_of(cols) |> dplyr::across(detect_cens, .names = "{.col}_censored")
    )
  }

  df <- df |> dplyr::mutate(
    tidyselect::all_of(cols) |> dplyr::across(remove_cens)
  )

  return(df)
}

detect_cens <- function(x) {
  x |> stringr::str_detect("<")
}

remove_cens <- function(x) {
  x |>
    stringr::str_trim() |>
    stringr::str_replace(pattern = "<", replacement = "") |>
    as.numeric()
}
