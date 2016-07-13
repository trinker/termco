context("Checking as_dtm")

test_that("as_dtm makes a DocumentTermMatrix",{

    expect_true(methods::is(as_dtm(markers), "DocumentTermMatrix"))

})

