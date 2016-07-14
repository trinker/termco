context("Checking classify")

test_that("classify produces more results for higher n regardless of ties.method",{

    data(presidential_debates_2012)

    discoure_markers <- list(
        response_cries = c("\\boh", "\\bah", "\\baha", "\\bouch", "yuk"),
        back_channels = c("uh[- ]huh", "uhuh", "yeah"),
        summons = "hey",
        justification = "because"
    )


    x <- with(presidential_debates_2012, term_count(dialogue, TRUE, discoure_markers))


    y <- table(unlist(classify(x)))
    z <- table(unlist(classify(x, n=2)))

    expect_true(sum(y) < sum(z))

    a <- table(unlist(classify(x, ties.method = "random")))
    b <- table(unlist(classify(x, n=2, ties.method = "random")))

    expect_true(sum(a) < sum(b))


})

test_that("classify produces same n results regardless of ties.method",{

    data(presidential_debates_2012)

    discoure_markers <- list(
        response_cries = c("\\boh", "\\bah", "\\baha", "\\bouch", "yuk"),
        back_channels = c("uh[- ]huh", "uhuh", "yeah"),
        summons = "hey",
        justification = "because"
    )


    x <- with(presidential_debates_2012, term_count(dialogue, TRUE, discoure_markers))


    y <- table(unlist(classify(x)))
    z <- table(unlist(classify(x, n=2)))

    a <- table(unlist(classify(x, ties.method = "random")))
    b <- table(unlist(classify(x, n=2, ties.method = "random")))

    expect_true(sum(y) == sum(a))

    expect_true(sum(z) == sum(b))
})
