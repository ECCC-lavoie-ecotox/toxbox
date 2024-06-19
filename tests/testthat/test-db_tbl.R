context("Add, delete, update entries in table")

test_that("insert_entry() success", {

    con <- test_db()
    
    insert_entry(
        con = con, 
        tbl = "species", 
        species_id = "TSN", 
        genus = "Lupus",
        species = "Lupus lupus"
    )

    rs <- DBI::dbGetQuery(con,"SELECT * FROM species;")
    testthat::expect_true(nrow(rs) == 1L)
    testthat::expect_true(rs$species_id == "TSN")
    testthat::expect_true(rs$genus == "Lupus")
    testthat::expect_true(rs$species == "Lupus lupus")

    withr::deferred_run()
})

test_that("delete_entry() success", {
    con <- test_db(mockData = TRUE)

    delete_entry(
        con = con, 
        tbl = "species",
        species_id = "TSN"
    )
    
    rs <- DBI::dbGetQuery(con,"SELECT * FROM species WHERE species_id = 'TSN';")
    testthat::expect_true(nrow(rs) == 0L)

    withr::deferred_run()
})

test_that("modify_entry() success", {
    con <- test_db(mockData = TRUE)

    modify_entry(
        con = con, 
        tbl = "species",
        species_id = "TSN",
        genus = "Hydra" 
    )
    
    new_genus <- DBI::dbGetQuery(con,"SELECT genus FROM species WHERE species_id = 'TSN';")$genus
    testthat::expect_true(new_genus == "Hydra")

    withr::deferred_run()
})

