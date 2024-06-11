#' Init sqlite connexion
#' 
#' @examples
#' \dontrun{
#'  init_con()
#' }
#' 
#' @export
init_con <- function(){
    db <- file.path(db_path(), db_name())
    stopifnot(db |> file.exists())
    DBI::dbConnect(RSQLite::SQLite(), db)
}
