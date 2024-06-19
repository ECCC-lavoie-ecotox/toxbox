#' Start function
#'
#' @export

fct_start <- function() {
    con <<- init_con()
    onStop(clean_up_app)
}

clean_up_app <- function() {
    cli::cli_alert_info("Application stopped -- cleaning up")
    DBI::dbDisconnect(con)
}
