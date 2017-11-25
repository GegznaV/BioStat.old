context("posthoc_anova")

# [!!!] A more reliable unit test is needed
test_that("`posthoc_anova`` works", {
    data(ChickWeight)
    rez <- posthoc_anova(y = ChickWeight$weight,
                       x = ChickWeight$Diet)
    expect_is(unclass(rez), "list")
})

test_that("`posthoc_anova_games_howell` works", {
    data(ChickWeight)
    rez1 <- posthoc_anova_games_howell(y = ChickWeight$weight,
                       x = ChickWeight$Diet)
    expect_is(unclass(rez1), "list")

    rez2 <- posthoc_anova_games_howell(weight ~ Diet, data = ChickWeight)
    expect_is(unclass(rez2), "list")
})

test_that("`posthoc_anova_tukey` works", {
    data(ChickWeight)
    rez1 <- posthoc_anova_tukey(y = ChickWeight$weight,
                                x = ChickWeight$Diet)
    expect_is(unclass(rez1), "list")

    rez2 <- posthoc_anova_tukey(weight ~ Diet, data = ChickWeight)
    expect_is(unclass(rez2), "list")
})
