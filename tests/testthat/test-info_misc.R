context("Get table informations")

test_that("get_tbl_info() success", {
    con <- test_db()
    df <- get_tbl_info(con, "species")
    testthat::expect_identical(df, tbl_info_test)
    withr::deferred_run()
})

test_that("get_tbl_fields_pkey() success", {
    con <- test_db()
    mockery::stub(get_tbl_fields_pkey, "get_tbl_info", tbl_info_test)
    pkey <- get_tbl_fields_pkey(con, "species")
    testthat::expect_true(pkey == "species_id")
    withr::deferred_run()    
})

test_that("get_tbl_fields_notnull() success", {
    con <- test_db()
    mockery::stub(get_tbl_fields_notnull, "get_tbl_info", tbl_info_test)
    pkey <- get_tbl_fields_notnull(con, "species")
    testthat::expect_true(pkey == "species")
    withr::deferred_run()     
})

test_that("check_notnull_fields() error", {
    con <- test_db()
    mockery::stub(check_notnull_fields, "get_tbl_fields_notnull", "species")  
    testthat::expect_error(
        check_notnull_fields(con, "species", "test"), 
        "species cannot be null(s)", fixed = TRUE
    )
    withr::deferred_run()     
})

test_that("check_pkeys_fields() error", {
    con <- test_db()
    mockery::stub(check_pkeys_fields, "get_tbl_fields_pkey", "species_id")    
    testthat::expect_error(
        check_pkeys_fields(con, "species", "test"), 
        "Primary key(s) species_id is/are missing", fixed = TRUE
    )
    withr::deferred_run()     
})

