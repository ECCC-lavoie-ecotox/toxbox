#' DB getters
#'
#' Retrieve specific information from our admin database.
#'
#' @param tbl a character name of the table
#' @param ... a vector of column names in the specified table
#'
#' 
#' @describeIn get_column_elements Get species name list.
#' @export
#' 
add_entry_tbl <- function(tbl, ...){
    fields <- list(...)

    check_pkeys_fields(tbl, fields)
    check_notnull_fields(tbl, fields)

    columns <- glue::glue_collapse(names(fields), ", ")
    values <- glue::glue_collapse(fields, ", ")

    ddl <- glue::glue("INSERT INTO {tbl} ({columns}) VALUES ({glue::single_quote(values)});")
    con <- init_con()
    res <- DBI::dbSendStatement(con, ddl)

    if(DBI::dbHasCompleted(res)){
        cli::cli_alert_info("{ DBI::dbGetRowsAffected(res) } row inserted in {tbl}")
    }
    
    on.exit(DBI::dbClearResult(res))
    on.exit(DBI::dbDisconnect(con), add=TRUE, after = TRUE)
}

modify_entry_tbl <- function(tbl, ...){
    fields <- list(...)

    check_pkeys_fields(tbl, fields)

    pkeys_tbl <- get_tbl_fields_pkey(tbl)
    target_row <- do.call("search_tbl", list(tbl = tbl) |> append(fields[pkeys_tbl]))

    if(nrow(target_row) > 1L){
        cli::cli_abort("Error: More than one row found with { fields[pkeys_tbl] }")
    } else {
        
        pkeys_values <- fields[which(names(fields) %in% pkeys_tbl)]
        update_values <- fields[-which(names(fields) %in% pkeys_tbl)]

        update_entries <- purrr::map(names(update_values), \(n){
            glue::glue("{n} = ${n}")
        }) |> glue::glue_collapse(",")

        criterias <- purrr::map(names(pkeys_values), \(n){
            glue::glue("{n} = ${n}")
        }) |> glue::glue_collapse(" AND ")

        ddl <- glue::glue("
            UPDATE {tbl}
            SET { update_entries }
            WHERE { criterias };
        ")

        con <- init_con()
        res <- DBI::dbSendStatement(con, ddl)
        DBI::dbBind(res, fields)

        if(DBI::dbHasCompleted(res)){
            cli::cli_alert_info("Entry with { criterias } updated in { tbl }")
        }
        
        on.exit(DBI::dbClearResult(res))
        on.exit(DBI::dbDisconnect(con), add=TRUE, after = TRUE)
    }
}

delete_entry_tbl <- function(tbl, ...){
    fields <- list(...)

    check_pkeys_fields(tbl, fields)
    pkeys_tbl <- get_tbl_fields_pkey(tbl)
    target_row <- do.call("search_tbl", list(tbl = tbl) |> append(fields[pkeys_tbl]))

    if(nrow(target_row) > 1L){
        cli::cli_abort("Error: More than one row found with { fields[pkeys_tbl] }")
    } else {
        criterias <- purrr::map(names(fields[pkeys_tbl]), \(n){
            glue::glue("{n} = ${n}")
        }) |> glue::glue_collapse(" AND ")

        ddl <- glue::glue("
            DELETE 
            FROM {tbl}
            WHERE { criterias };
        ")

        con <- init_con()
        res <- DBI::dbSendStatement(con, ddl)
        DBI::dbBind(res, fields)

        if(DBI::dbHasCompleted(res)){
            cli::cli_alert_info("Entry with { criterias } deleted in { tbl }")
        }
        
        on.exit(DBI::dbClearResult(res))
        on.exit(DBI::dbDisconnect(con), add=TRUE, after = TRUE)
    }
}

get_tbl <- function(tbl) {
    con <- init_con()
    DBI::dbReadTable(con, tbl)
    on.exit(DBI::dbDisconnect(con))
}
