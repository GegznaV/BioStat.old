context("standardized_coef")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`standardized_coef` works", {
    data(USJudgeRatings)
    us <- USJudgeRatings
    lm1 <- lm(CONT ~ INTG + DMNR + log(DILG), data = us)
    rez <- standardized_coef(lm1)

    expect_is(rez, "lm_beta")
    expect_is(rez, "numeric")
    expect_equal(names(rez), c("INTG", "DMNR", "log(DILG)"))
    expect_equivalent(unclass(round(rez, 3)), c(-0.293, -0.316, 0.533))
})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`print.lm_beta` works", {
    data(USJudgeRatings)
    us <- USJudgeRatings
    lm1 <- lm(CONT ~ INTG + DMNR + log(DILG), data = us)
    rez <- standardized_coef(lm1)

    expect_output(print(rez), "INTG")
    expect_output(print(rez), "DMNR")
    expect_output(print(rez), "log\\(DILG\\)")
    expect_output(print(rez), "-0.293")
    expect_output(print(rez), "-0.316")
    expect_output(print(rez), "0.533")
})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
