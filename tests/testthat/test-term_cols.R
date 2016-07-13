context("Checking term_cols")

test_that("term_cols is a tibble.",{

    expect_true(methods::is(term_cols(markers), 'tbl_df'))

})

test_that("term_cols gets correct number of columns.",{

    expect_true(ncol(term_cols(markers)) == 4)

})

