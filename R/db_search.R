#' search_tbl
#'
#' @param tbl a character name of the table
#' @param ... table fields with search value
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
search_tbl <- function(con = NULL, tbl = NULL,...){

    fields <- list(...)
    tbl_fields <- DBI::dbListFields(con, tbl)

    # Safety assertions
    stopifnot(!any(names(fields) |> is.null()))
    stopifnot(all(names(fields) %in% tbl_fields))

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
        entries[[length(entries)+1]] <- DBI::dbFetch(res, 100)
    }
    entries <- dplyr::bind_rows(entries)

    cli::cli_alert_info("Found { nrow(entries) } entries")

    on.exit(DBI::dbClearResult(res))

    return(entries)
}

