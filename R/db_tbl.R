#' Add entry within specified table
#'
#' @param con connexion object returned by DBI::dbConnect()
#' @param tbl a character name of the table
#' @param ... a vector of column names in the specified table
#'
#' @export
#' 
add_entry_tbl <- function(con = NULL, tbl = NULL, ...){
    fields <- list(...)

    check_fields_exist(con, tbl, names(fields))
    check_fields_pkeys(con, tbl, names(fields))
    check_fields_notnulls(con, tbl, names(fields))

    ddl <- glue::glue_sql("INSERT INTO { tbl } ({ names(fields)* }) VALUES ({ fields* });", .con = con)
    res <- DBI::dbSendStatement(con, ddl)
    
    on.exit(DBI::dbClearResult(res))
}

#' @describeIn add_entry_tbl Modify entry within specified table.
#' @export
#' 
modify_entry_tbl <- function(con = NULL, tbl = NULL, ...){
    fields <- list(...)

    check_fields_exist(con, tbl, names(fields))
    check_fields_pkeys(con, tbl, names(fields))

    pkeys_tbl <- get_tbl_pkeys(con, tbl)
    target_row <- do.call("search_tbl", list(con = con, tbl = tbl) |> append(fields[pkeys_tbl]))

    if(nrow(target_row) > 1L){
        cli::cli_abort("Error: More than one row found with { fields[pkeys_tbl] }")
    } else {
        
        pkeys_values <- fields[which(names(fields) %in% pkeys_tbl)]
        update_values <- fields[-which(names(fields) %in% pkeys_tbl)]

        update_entries <- purrr::map(names(update_values), \(n){
            glue::glue("{n} = ${n}")
        }) |> glue::glue_sql_collapse(",")

        criterias <- purrr::map(names(pkeys_values), \(n){
            glue::glue("{n} = ${n}")
        }) |> glue::glue_sql_collapse(" AND ")

        ddl <- glue::glue_sql("
            UPDATE { tbl }
            SET { update_entries }
            WHERE { criterias };
        ", .con = con)

        res <- DBI::dbSendStatement(con, ddl)
        DBI::dbBind(res, fields)
        
        on.exit(DBI::dbClearResult(res))
    }
}

#' @describeIn add_entry_tbl Delete entry within specified table.
#' @export
#' 
delete_entry_tbl <- function(con = NULL, tbl = NULL, ...){
    fields <- list(...)

    check_fields_exist(con, tbl, names(fields))
    check_fields_pkeys(con, tbl, names(fields))

    pkeys_tbl <- get_tbl_pkeys(con, tbl)
    target_row <- do.call("search_tbl", list(con =  con, tbl = tbl) |> append(fields[pkeys_tbl]))

    if(nrow(target_row) > 1L){
        cli::cli_abort("Error: More than one row found with { fields[pkeys_tbl] }")
    } else {
        criterias <- purrr::map(names(fields[pkeys_tbl]), \(n){
            glue::glue("{n} = ${n}")
        }) |> glue::glue_sql_collapse(" AND ")

        ddl <- glue::glue_sql("
            DELETE 
            FROM { tbl }
            WHERE { criterias };
        ", .con = con)

        res <- DBI::dbSendStatement(con, ddl)
        DBI::dbBind(res, fields)
        
        on.exit(DBI::dbClearResult(res))
    }
}

get_tbl <- function(con = NULL, tbl = NULL) {
    DBI::dbReadTable(con, tbl)
}
