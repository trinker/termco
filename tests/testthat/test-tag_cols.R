context("Checking tag_cols")

test_that("tag_cols is a tibble.",{

    expect_true(methods::is(tag_cols(markers), 'tbl_df'))

})

test_that("tag_cols gets correct number of columns.",{

    expect_true(ncol(tag_cols(markers)) == 4)

})

