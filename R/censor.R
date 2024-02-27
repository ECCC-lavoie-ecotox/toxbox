#' Detect censored data
#'
#' @description For each column specified, censored data are detected by the presence of the symbol [<] . This function returned a data.frame with two columns by 
#'
#' @param template a template. Use "?" to list available template.
#' @param data data passed to [whisker::whisker.render()].
#' @param ... further arguments passed to [rmarkdown::render()].
#'
#' @return The return value, if any, from executing the function.
#' @examples
#' \dontrun{
#' custom_render("report1",
#'     data = list(title = "templex"),
#'     envir = list(geom = 1)
#' )
#' }

censor <- function(df = NULL, cols = c(NULL), keep_cens = TRUE) {
    
    cli::cli_alert_abort(is.data.frame(df))
    cli::cli_alert_abort(all(cols %in% names(df)))

    cols <- colnames(data) #create a vector of col names for future naming ease
    
    for (i in t:ncol(data)) { #t = number of the col we start at 
        #(skip the sample info, start at first compound)
        
        #if "<" is found, put TRUE in the new col because it is a non-detect, 
        #if not, put FALSE (= uncensored value)
        data$new <- if_else(grepl( "<", data[, (i)]), TRUE, FALSE)
        #paste the compound name to the col head followed by "cen"
        names(data)[names(data) == 'new'] <- paste0(cols[[i]], "cen") 
        #if "<" is found, remove it for easy numeric conversion
        data[, (i)] <- str_replace(data[, (i)], "<", "") 
        #convert to numeric
        data[, (i)] <- as.numeric(as.character(data[, (i)]))
    }
    return(df)
}


detect_censored_data <- function(x){ x |> stringr::str_detect("<") }

get_censored_data <- function(x) {
    x |> 
        stringr::trim() |>
        stringr::str_replace(pattern = "<", replacement = "") |>
        as.numeric()
}