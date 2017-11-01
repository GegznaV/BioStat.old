context("posthocTGH")

# [!!!]
# A more reliable unit test is needed
test_that("multiplication works", {
    data(ChickWeight)
    rez <- posthocTGH(y = ChickWeight$weight, x = ChickWeight$Diet)
    expect_is(unclass(rez), "list")
})
