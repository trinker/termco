context("Checking rename_tags")

test_that("rename_tags updates column and attribute names",{

    data(presidential_debates_2012)

    discoure_markers <- list(
        response_cries = c("\\boh", "\\bah", "\\baha", "\\bouch", "yuk"),
        back_channels = c("uh[- ]huh", "uhuh", "yeah"),
        summons = "hey",
        justification = "because"
    )

    (markers <- with(presidential_debates_2012,
        term_count(dialogue, list(person, time), discoure_markers)
    ))

    new <- c('people', 'bcs', 's')
    old <-  c('person', 'back_channels', 'summons')

    x <- rename_tags(markers, old = old, new = new)

    expect_true(all(new %in% colnames(x)))
    expect_true(!any(old %in% colnames(x)))

    atts <- c(attributes(x)[['term.vars']], attributes(x)[['group.vars']])

    expect_true(all(new %in% atts))
    expect_true(!any(old %in% atts))

})

