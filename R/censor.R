#' Manage censored data
#'
#' @description For each column specified, censored data are detected by the presence of the symbol [<] . This function removes all censored data in the specified columns. keep_cens=TRUE assigns the logicical test result (presence of absence of symbol <) to a new column, to keep track of the original censored data. 
#'
#' @param df a data.frame. 
#' @param cols a vector of character.
#' @param keep_cens a boolean. If TRUE, assigns the logicical test result (presence of absence of symbol <) to a new column
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
  if (!all(cols %in% names(df))) cli::cli_abort("Some columns are not present in the data.frame")

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
