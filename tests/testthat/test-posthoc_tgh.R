context("posthoc_tgh")

# [!!!] A more reliable unit test is needed
test_that("multiplication works", {
    data(ChickWeight)
    rez <- posthoc_tgh(y = ChickWeight$weight,
                       x = hickWeight$Diet)
    expect_is(unclass(rez), "list")
})
