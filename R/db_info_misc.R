#' Check if pkeys presents
#' 
#' @param tbl a character name of the table
#' @param fields a vector of column names in the specified table
#' 
#' @return
#' Logical test result.
#' 
#' @examples
#' \dontrun{
#'  check_pkeys_fields("species", fields = c("species_id"))
#' }
#' @export
#' 
check_pkeys_fields <- function(tbl, fields){
    pkeys <- get_tbl_fields_pkey(tbl)
    
    if(!all(pkeys %in% names(fields))){
        missing_pkeys <- pkeys[which(!pkeys %in% fields)] |>
            glue::glue_collapse(", ", last = "and")
        cli::cli_abort("Primary key(s) { missing_pkeys } is/are missing")
    }
}

#' Check if not null fields present
#' 
#' @param tbl a character name of the table
#' @param fields a vector of column names in the specified table
#' 
#' @return
#' Logical test result.
#' 
#' @examples
#' \dontrun{
#'  check_notnull_fields("species", fields = c("species_id"))
#' }
#' 
#' @export
check_notnull_fields <- function(tbl, fields){
    notnulls <- get_tbl_fields_notnull(tbl)
    
    if(!all(notnulls %in% fields)){
        missing_fields <- notnulls[which(!notnulls %in% fields)] |>
            glue::glue_collapse(", ", last = "and")
        cli::cli_abort("{ missing_fields } cannot be null(s)")
    }
}

#' Get table fields properties (fkey, pkey, unique constraints etc.)
#' 
#' @param tbl a character name of the table
#' 
#' @return
#' data.frame with 6 columns:
#' - cid: field position
#' - name: field name
#' - type: field type
#' - notnull: is not null constraint apply to this field?
#' - dflt_value: field default value
#' - pk: is this field a primary key?
#' 
#' @examples
#' \dontrun{
#'  get_tbl_info("species")
#' }
#' @export
get_tbl_info <- function(tbl) {
    con <- init_con()
    res <- DBI::dbGetQuery(con, glue::glue("SELECT * FROM pragma_table_info('{ tbl }');"))
    on.exit(DBI::dbDisconnect(con))
    return(res)
}

#' Get table primary key fields
#' 
#' @param tbl a character name of the table
#' 
#' @return
#' Primary key fields as a character vector.
#' 
#' @examples
#' \dontrun{
#'  get_tbl_fields_pkey("species")
#' }
#' @export
get_tbl_fields_pkey <- function(tbl){
    get_tbl_info(tbl) |>
        dplyr::filter(rlang::.data$pk == 1) |>
        dplyr::pull(rlang::.data$name)
}

#' Get table not null fields
#' 
#' @param tbl a character name of the table
#' 
#' @return
#' Not null fields as a character vector.
#' 
#' @examples
#' \dontrun{
#'  get_tbl_fields_notnull("species")
#' }
#' @export
get_tbl_fields_notnull <- function(tbl){
    get_tbl_info(tbl) |>
        dplyr::filter(rlang::.data$notnull == 1) |>
        dplyr::pull(rlang::.data$name)
}

