#' search_tbl
#'
#' @param con connexion object returned by DBI::dbConnect()
#' @param tbl a character name of the table
#' @param ... table fields with search value
#' @param nfetch data fetch size (default = 100)
#' 
#' @return
#' A data.frame with query results
#' 
#' @examples
#' \dontrun{
#'  search_tbl(con, "species", genus = "%Poo%")
#' }
#' @export
#' 
search_tbl <- function(con = NULL, tbl = NULL, ..., nfetch = 100){

    fields <- list(...)
    check_fields_exist(con, tbl, names(fields))

    search_criterias <- purrr::map2(names(fields), fields, \(n, p){
        if(length(p) > 1L){
            glue::glue_sql("{`n`} IN ({ p* })", .con = con)
        } else {
            glue::glue_sql("{`n`} LIKE { p }", .con = con)
        }
    }) |> glue::glue_sql_collapse(" OR ")

    query <- glue::glue_sql("SELECT * FROM { tbl } WHERE { search_criterias };", .con = con)
    res <- DBI::dbSendQuery(con, query)

    entries <- list()
    while (!DBI::dbHasCompleted(res)) {
        entries[[length(entries)+1]] <- DBI::dbFetch(res, nfetch)
    }
    entries <- dplyr::bind_rows(entries)

    cli::cli_alert_info("Found { nrow(entries) } entries")

    on.exit(DBI::dbClearResult(res))

    return(entries)
}

