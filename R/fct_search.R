#' Get full database to cascade filter
#'
#' @export

get_db <- function(con = NULL) {
    dplyr::tbl(
        con, dplyr::sql("
        SELECT * FROM lab_measurement
            LEFT JOIN analyte AS anal USING (id_analyte)
            LEFT JOIN lab_sample USING (id_lab_sample)
            LEFT JOIN lab_field_sample USING (id_lab_sample)
            LEFT JOIN field_sample USING (id_field_sample)
            LEFT JOIN report USING (id_report)
            LEFT JOIN project USING (id_project)
            LEFT JOIN species USING (id_species)")
    )
}

#' Get full database cached
#' @export
get_db_cached <- memoise::memoise(get_db)
