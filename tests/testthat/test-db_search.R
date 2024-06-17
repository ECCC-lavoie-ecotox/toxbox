context("Search entries in table")

test_that("search_tbl() success", {

    con <- test_db(mockData = TRUE)
    
    testthat::expect_equal(
        search_tbl(
            con = con, 
            tbl = "species", 
            species_id = c("TSN", "TSN2")
        )$species_id, c("TSN", "TSN2")
    )

    testthat::expect_equal(
        search_tbl(
            con = con, 
            tbl = "species", 
            species_id = "%TS%"
        )$species_id, c("TSN", "TSN2", "TSN3")
    )

    testthat::expect_equal(
        search_tbl(
            con = con, 
            tbl = "species", 
            genus = "Alces",
            genus = "Castor"
        )$species_id, c("TSN2", "TSN3")
    )

    withr::deferred_run()
})
