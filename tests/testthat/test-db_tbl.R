context("Add, delete, update entries in table")

test_db <- tempfile()
con <- DBI::dbConnect(RSQLite::SQLite(), test_db)

DBI::dbExecute(con, "
    CREATE TABLE 'species' (
        species_id TEXT, 
        genus TEXT, 
        species TEXT NOT NULL, 
        taxa TEXT,
        PRIMARY KEY (species_id)
    );")

DBI::dbDisconnect()

test_that("add_entry_tbl()", {
    # Classic
    testthat::expect_success(
        add_entry_tbl(
            con = DBI::dbConnect(RSQLite::SQLite(), test_db), 
            tbl = "species", 
            species_id = "TSN", 
            species = "Lupus lupus"
        )
    )
    # Without pkey
    # add_entry_tbl(con = DBI::dbConnect(RSQLite::SQLite(), test_db), "species", species = "Lupus lupus")
    # # With null values for not null fields
    # add_entry_tbl(con, "species", species_id = "TSN")
})
