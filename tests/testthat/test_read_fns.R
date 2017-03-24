library(testthat)
library(farsdata)
context('Loading FARS files')

test_that("Correct filename is created from year", {
    expect_match(make_filename(2015), "accident_2015.csv.bz2")
    expect_match(make_filename("2015"), "accident_2015.csv.bz2")
})

test_that("FARS object is the correct class", {
    expect_is(fars_read_years(2015), "list")
    expect_is(fars_read_years("2015"), "list")
})
