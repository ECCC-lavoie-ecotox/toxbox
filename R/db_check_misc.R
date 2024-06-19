#' Check if fields present in table
#' 
#' @param con connexion object returned by DBI::dbConnect()
#' @param tbl a character name of the table
#' @param fields a vector of column names in the specified table
#' 
#' @examples
#' \dontrun{
#'      check_fields_exist(con, "species", species_id = "test")
#' }
#' @export
#' 
check_fields_exist <- function(con, tbl, fields){
    columns <- get_tbl_info(con, tbl = tbl) |> dplyr::pull(name)
    if(!all(fields %in% columns)){
        unknown_fields <- fields[which(!fields %in% columns)] |>
            glue::glue_collapse(", ", last = " and ")
        cli::cli_abort("Fields { unknown_fields } is/are not present in table { tbl }")       
    }
}

#' Check if pkeys presents
#' 
#' @param con connexion object returned by DBI::dbConnect()
#' @param tbl a character name of the table
#' @param fields a vector of column names in the specified table
#' 
#' @examples
#' \dontrun{
#'  check_fields_pkeys(con, "species", fields = c("species_id"))
#' }
#' @export
#' 
check_fields_pkeys <- function(con, tbl, fields){
    pkeys <- get_tbl_pkeys(con, tbl = tbl)
    
    if(!all(pkeys %in% fields)){
        missing_pkeys <- pkeys[which(!pkeys %in% fields)] |>
            glue::glue_collapse(", ", last = "and")
        cli::cli_abort("Primary key(s) { missing_pkeys } is/are missing")
    }
}

#' Check if not null fields present
#' 
#' @param con connexion object returned by DBI::dbConnect()
#' @param tbl a character name of the table
#' @param fields a vector of column names in the specified table
#' 
#' @examples
#' \dontrun{
#'  check_fields_notnulls(con, "species", fields = c("species_id"))
#' }
#' 
#' @export
check_fields_notnulls <- function(con, tbl, fields){
    notnulls <- get_tbl_notnulls(con, tbl = tbl)
    
    if(!all(notnulls %in% fields)){
        missing_fields <- notnulls[which(!notnulls %in% fields)] |>
            glue::glue_collapse(", ", last = "and")
        cli::cli_abort("{ missing_fields } cannot be null(s)")
    }
}

#' Get table fields properties (fkey, pkey, unique constraints etc.)
#' 
#' @param con connexion object returned by DBI::dbConnect()
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
#'      get_tbl_info("species")
#' }
#' @export
get_tbl_info <- function(con = NULL, tbl = NULL) {
    q <- glue::glue_sql("SELECT * FROM pragma_table_info({tbl});", .con = con)
    res <- DBI::dbGetQuery(con, q)
    return(res)
}

#' Get table primary key fields
#' 
#' @param con connexion object returned by DBI::dbConnect()
#' @param tbl a character name of the table
#' 
#' @return
#' Primary key fields as a character vector.
#' 
#' @examples
#' \dontrun{
#'      get_tbl_pkeys("species")
#' }
#' @export
get_tbl_pkeys <- function(con, tbl){
    get_tbl_info(con, tbl = tbl) |>
        dplyr::filter(pk == 1) |>
        dplyr::pull(name)
}

#' Get table not null fields
#' 
#' @param con connexion object returned by DBI::dbConnect()
#' @param tbl a character name of the table
#' 
#' @return
#' Not null fields as a character vector.
#' 
#' @examples
#' \dontrun{
#'  get_tbl_notnulls("species")
#' }
#' @export
get_tbl_notnulls <- function(con, tbl){
    get_tbl_info(con, tbl = tbl) |>
        dplyr::filter(notnull == 1) |>
        dplyr::pull(name)
}

