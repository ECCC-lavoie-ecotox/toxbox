context("Get table informations")

test_that("get_tbl_info() success", {
    con <- test_db()
    df <- get_tbl_info(con, "species")
    testthat::expect_identical(df, tbl_info_test)
    withr::deferred_run()
})

test_that("get_tbl_pkeys() success", {
    con <- test_db()
    mockery::stub(get_tbl_pkeys, "get_tbl_info", tbl_info_test)
    pkey <- get_tbl_pkeys(con, "species")
    testthat::expect_true(pkey == "species_id")
    withr::deferred_run()    
})

test_that("get_tbl_notnulls() success", {
    con <- test_db()
    mockery::stub(get_tbl_notnulls, "get_tbl_info", tbl_info_test)
    pkey <- get_tbl_notnulls(con, "species")
    testthat::expect_true(pkey == "species")
    withr::deferred_run()     
})

test_that("check_fields_exist() error", {
    con <- test_db()
    mockery::stub(check_fields_exist, "get_tbl_info", tbl_info_test)
    testthat::expect_error(
        check_fields_exist(con, "species", "test"), 
        "Fields test is/are not present in table species", fixed = TRUE
    )
    withr::deferred_run()     
})

test_that("check_fields_notnulls() error", {
    con <- test_db()
    mockery::stub(check_fields_notnulls, "get_tbl_notnulls", "species")  
    testthat::expect_error(
        check_fields_notnulls(con, "species", "test"), 
        "species cannot be null(s)", fixed = TRUE
    )
    withr::deferred_run()     
})

test_that("check_fields_pkeys() error", {
    con <- test_db()
    mockery::stub(check_fields_pkeys, "get_tbl_pkeys", "species_id")    
    testthat::expect_error(
        check_fields_pkeys(con, "species", "test"), 
        "Primary key(s) species_id is/are missing", fixed = TRUE
    )
    withr::deferred_run()     
})

