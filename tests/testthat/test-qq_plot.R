context("Function `qq_plot()`")

test_that("`qq_plot()` works", {
    data("iris", package = "datasets")
    gg1 <- qq_plot(Sepal.Length ~ Species, data = iris)
    gg2 <- qq_plot("Sepal.Length", groups = "Species", data = iris)

    # expect_silent(plot(gg1))
    # expect_silent(print(gg1))
    expect_silent(gg1)
    expect_silent(gg2)

    expect_is(gg1, c("gg","ggplot"))
    expect_is(gg2, c("gg","ggplot"))
})