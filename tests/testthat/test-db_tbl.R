context("Add, delete, update entries in table")

test_db <- function(mockData = FALSE, env = parent.frame()) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

    DBI::dbExecute(con, "DROP TABLE IF EXISTS 'species';")
    DBI::dbExecute(con, "CREATE TABLE 'species' (
            species_id TEXT, 
            genus TEXT, 
            species TEXT NOT NULL,
            PRIMARY KEY (species_id)
        );")

    if(mockData){
        DBI::dbExecute(con, "INSERT INTO 'species' (species_id, genus, species) VALUES ('TSN', 'Lupus', 'Lupus lupus');")
    }

    withr::defer(DBI::dbDisconnect(con), env)
    return(con)
}

test_that("add_entry_tbl() success", {

        con <- test_db()
        add_entry_tbl(
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


test_that("add_entry_tbl() with missing pkeys", {
        con <- test_db()

        testthat::expect_error(
            add_entry_tbl(
                con = con, 
                tbl = "species", 
                genus = "Lupus",
                species = "Lupus lupus"
        ), "Primary key(s) species_id is/are missing", fixed = TRUE)
        
        withr::deferred_run()
})

test_that("add_entry_tbl() with not null contraint ", {
        con <- test_db()

        fields = list(species_id = "TSN", 
                genus = "Lupus",
                species = NA)

        testthat::expect_error(
            add_entry_tbl(
                con = con, 
                tbl = "species", 
                species_id = "TSN", 
                genus = "Lupus"
        ), "species cannot be null(s)", fixed = TRUE)
        
        withr::deferred_run()
})
