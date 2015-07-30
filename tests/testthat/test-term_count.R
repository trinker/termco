context("Checking term_count")

library(dplyr)
data(pres_debates2012)

test_that("term_count produces expected output when no grouping variable is supplied",{

    discoure_markers <- list(
        response_cries = c("oh", "ah", "aha", "ouch", "yuk"),
        back_channels = c("uh[- ]huh", "uhuh", "yeah"),
        summons = "hey",
        justification = "because"
    )

    markers1 <- with(pres_debates2012,
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

    markers2 <- with(pres_debates2012,
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

    markers3 <- with(pres_debates2012,
        term_count(dialogue, list(person, time), discoure_markers)
    )

    expect_true(nrow(markers3) == 10)
    expect_true(ncol(markers3) == 7)
    expect_true(is(markers3, "tbl_df"))
    expect_true(is(markers3, "term_count"))
    expect_true(all(colnames(markers3) %in% c("person", "time", "n.words", "response_cries",
        "back_channels", "summons", "justification")))

})

test_that("term_count prints pretty",{

    discoure_markers <- list(
        response_cries = c("oh", "ah", "aha", "ouch", "yuk"),
        back_channels = c("uh[- ]huh", "uhuh", "yeah"),
        summons = "hey",
        justification = "because"
    )

    markers3 <- with(pres_debates2012,
        term_count(dialogue, list(person, time), discoure_markers)
    )

    expect_true(all.equal(capture.output(print(markers3, pretty = FALSE)),
    c("Coverage: 100% ", "Source: local data frame [10 x 7]", "", "      person   time n.words response_cries back_channels summons justification",
    "1      OBAMA time 1    3599              4             0      43            26",
    "2      OBAMA time 2    7477              2             0      42            29",
    "3      OBAMA time 3    7243              4             1      58            33",
    "4     ROMNEY time 1    4085              1             0      27             8",
    "5     ROMNEY time 2    7536              6             3      49            20",
    "6     ROMNEY time 3    8303              8             0      84            19",
    "7    CROWLEY time 2    1672              2             0       4            12",
    "8     LEHRER time 1     765              6             3       0             0",
    "9   QUESTION time 2     583              2             0       0             2",
    "10 SCHIEFFER time 3    1445              0             0       2             6"
    )))

})

test_that("term_count plots a ggplot object",{

    discoure_markers <- list(
        response_cries = c("oh", "ah", "aha", "ouch", "yuk"),
        back_channels = c("uh[- ]huh", "uhuh", "yeah"),
        summons = "hey",
        justification = "because"
    )

    markers3 <- with(pres_debates2012,
        term_count(dialogue, list(person, time), discoure_markers)
    )

    expect_true(is(plot(markers3), "ggplot"))
    expect_true(is.null(plot(markers)[["labels"]][["label"]]))

    expect_true(is(plot(markers3, labels=TRUE), "ggplot"))
    expect_false(is.null(plot(markers, labels=TRUE)[["labels"]][["label"]]))


})


