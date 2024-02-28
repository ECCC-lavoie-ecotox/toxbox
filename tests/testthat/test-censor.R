context("Censored data")

test_df <- structure(
    list(
        pcb_87 = c("<0.0009", "7.8425083",
            "7.3865055", "6.5096138", "5.0000000",
            "4.0000000", "<0.0009", "6.9134932", 
            "5.8384615", "4.99076"
        ), pcb_92 = c("7.5738032",
            "7.8096829", "8.7030200", "7.6721659",
            "<0.0001", "8.9999999", "9.2408142",
            "7.4146705", "6.9902564", "7.6210256"
    )), row.names = c(NA, -10L), class = c("data.frame"))

test_that("Check if data censored test results are presents", {
    res <- uncensored(test_df, cols = c("pcb_87", "pcb_92"))
    expect_equal(dim(res), c(10,4))
})

test_that("Check detect_cens() function", {
    expect_equal(detect_cens(test_df$pcb_87[1]), TRUE)
})

test_that("Check remove_cens() function", {
    expect_equal(remove_cens(test_df$pcb_87[1]), 0.0009)
    expect_equal(is.numeric(remove_cens(test_df$pcb_87[1])), TRUE)
})