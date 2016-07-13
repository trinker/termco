context("Checking term_count")

library(dplyr)
data(presidential_debates_2012)

test_that("term_count produces expected output when no grouping variable is supplied",{

    discoure_markers <- list(
        response_cries = c("oh", "ah", "aha", "ouch", "yuk"),
        back_channels = c("uh[- ]huh", "uhuh", "yeah"),
        summons = "hey",
        justification = "because"
    )

    markers1 <- with(presidential_debates_2012,
        term_count(dialogue, , discoure_markers)
    )

    expect_true(nrow(markers1) == 1)
    expect_true(ncol(markers1) == 6)
    expect_true(is(markers1, "tbl_df"))
    expect_true(is(markers1, "term_count"))
    expect_true(all(colnames(markers1) %in% c("all", "n.words", "response_cries",
        "back_channels", "summons", "justification")))
})

test_that("term_count produces expected output when one grouping variable is supplied",{

    discoure_markers <- list(
        response_cries = c("oh", "ah", "aha", "ouch", "yuk"),
        back_channels = c("uh[- ]huh", "uhuh", "yeah"),
        summons = "hey",
        justification = "because"
    )

    markers2 <- with(presidential_debates_2012,
        term_count(dialogue, list(person), discoure_markers)
    )

    expect_true(nrow(markers2) == 6)
    expect_true(ncol(markers2) == 6)
    expect_true(is(markers2, "tbl_df"))
    expect_true(is(markers2, "term_count"))
    expect_true(all(colnames(markers2) %in% c("person", "n.words", "response_cries",
        "back_channels", "summons", "justification")))

})

test_that("term_count produces expected output when two grouping variable is supplied",{

    discoure_markers <- list(
        response_cries = c("oh", "ah", "aha", "ouch", "yuk"),
        back_channels = c("uh[- ]huh", "uhuh", "yeah"),
        summons = "hey",
        justification = "because"
    )

    markers3 <- with(presidential_debates_2012,
        term_count(dialogue, list(person, time), discoure_markers)
    )

    expect_true(nrow(markers3) == 10)
    expect_true(ncol(markers3) == 7)
    expect_true(is(markers3, "tbl_df"))
    expect_true(is(markers3, "term_count"))
    expect_true(all(colnames(markers3) %in% c("person", "time", "n.words", "response_cries",
        "back_channels", "summons", "justification")))

})

test_that("term_count prints when not pretty",{

    discoure_markers <- list(
        response_cries = c("oh", "ah", "aha", "ouch", "yuk"),
        back_channels = c("uh[- ]huh", "uhuh", "yeah"),
        summons = "hey",
        justification = "because"
    )

    markers3 <- with(presidential_debates_2012,
        term_count(dialogue, list(person, time), discoure_markers)
    )


    output <- capture.output(print(markers3, pretty = FALSE))

    expect_true(
        length(output) > 12 &
            grepl("Coverage: 100%", output[1]) &
            grepl("10\\s+SCHIEFFER\\s+time\\s+3\\s+1445\\s+0\\s+0\\s+2\\s+6", utils::tail(output, 1))
    )

})

test_that("term_count plots a ggplot object",{

    discoure_markers <- list(
        response_cries = c("oh", "ah", "aha", "ouch", "yuk"),
        back_channels = c("uh[- ]huh", "uhuh", "yeah"),
        summons = "hey",
        justification = "because"
    )

    markers3 <- with(presidential_debates_2012,
        term_count(dialogue, list(person, time), discoure_markers)
    )

    expect_true(is(plot(markers3), "ggplot"))
    expect_true(is.null(plot(markers)[["labels"]][["label"]]))

    expect_true(is(plot(markers3, labels=TRUE), "ggplot"))
    expect_false(is.null(plot(markers, labels=TRUE)[["labels"]][["label"]]))


})


