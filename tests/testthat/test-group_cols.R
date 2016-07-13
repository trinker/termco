context("Checking group_cols")


test_that("group_cols is a tibble.",{

    expect_true(methods::is(group_cols(markers), 'tbl_df'))

})

test_that("group_cols gets correct number of columns.",{

    expect_true(ncol(group_cols(markers)) == 2)

})

