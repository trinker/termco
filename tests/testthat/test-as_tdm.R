context("Checking as_tdm")

test_that("as_tdm makes a TermDocumentMatrix",{

    expect_true(methods::is(as_tdm(markers), "TermDocumentMatrix"))
})

