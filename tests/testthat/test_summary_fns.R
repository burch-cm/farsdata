library(testthat)
library(farsdata)
context('Summarizing FARS files')

test_that("FARS Summary data loads correctly", {
    expect_is(fars_summarize_years(2015), "tbl_df")
    expect_is(fars_summarize_years("2015"), "tbl_df")
    expect_match(names(fars_summarize_years(2015))[1], "MONTH")
})
