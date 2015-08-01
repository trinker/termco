context("Checking paste2")

test_that("paste2 pastes list of vectors",{

    x <- rep(list(state.abb[1:8],  month.abb[1:8]) , 5)
    expect_equal(paste2(x), c("AL.Jan.AL.Jan.AL.Jan.AL.Jan.AL.Jan", "AK.Feb.AK.Feb.AK.Feb.AK.Feb.AK.Feb",
            "AZ.Mar.AZ.Mar.AZ.Mar.AZ.Mar.AZ.Mar", "AR.Apr.AR.Apr.AR.Apr.AR.Apr.AR.Apr",
            "CA.May.CA.May.CA.May.CA.May.CA.May", "CO.Jun.CO.Jun.CO.Jun.CO.Jun.CO.Jun",
            "CT.Jul.CT.Jul.CT.Jul.CT.Jul.CT.Jul", "DE.Aug.DE.Aug.DE.Aug.DE.Aug.DE.Aug"
        )
    )

})


test_that("paste2 pastes dataframe columns",{

    x2 <- data.frame(matrix(c(NA, 1:7, "  f  "), ncol = 3))
    expect_equal(paste2(x2), c(NA, "1.4.7", "2.5.f"))

})

test_that("paste2 pastes dataframe columns without na handle",{

    x2 <- data.frame(matrix(c(NA, 1:7, "  f  "), ncol = 3))
    expect_equal(paste2(x2, handle.na = FALSE),
        c("NA.3.6", "1.4.7", "2.5.f")
    )

})

test_that("paste2 pastes dataframe columns without trimming",{

    x2 <- data.frame(matrix(c(NA, 1:7, "  f  "), ncol = 3))
    expect_equal(paste2(x2, trim = FALSE),
        c(NA, "1.4.7", "2.5.  f  ")
    )

})
