
context("Operator `%++%`")

test_that("`%++%` works", {
    expect_equal("a" %++% "b", "ab")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
context("Function `qq_line_coeffs`")

test_that("`qq_line_coeffs()` works", {
    set.seed(254)
    rez <- qq_line_coeffs(rnorm(50))
    expect_true(is.vector(rez))
    expect_equal(names(rez), c("intercept", "slope"))

    # Approximate rezult
    expect_equal(round(rez, 7),
                 c(intercept = 0.1078016, slope = 0.8185576))
})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
context("Function `prettify_p_value`")

test_that("`prettify_p_value()` works with numbers", {
    expect_equal(prettify_p_value(0.005),    "<0.01 ")
    expect_equal(prettify_p_value(0.0005),   "<0.001")
    expect_equal(prettify_p_value(0.052147), " 0.05 ")
})
test_that("`prettify_p_value()` works with characters", {
    expect_equal(prettify_p_value("0.005"),    "<0.01 ")
    expect_equal(prettify_p_value("0.0005"),   "<0.001")
    expect_equal(prettify_p_value("0.052147"), " 0.05 ")
})
test_that("`prettify_p_value()` works with factors", {
    expect_equal(prettify_p_value(factor("0.005")),    "<0.01 ")
    expect_equal(prettify_p_value(factor("0.0005")),   "<0.001")
    expect_equal(prettify_p_value(factor("0.052147")), " 0.05 ")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
context("Function `prettify_p_value`")

test_that("`prettify_p_value()` works with numbers", {
    expect_equal(prettify_p_value(0.005),    "<0.01 ")
    expect_equal(prettify_p_value(0.0005),   "<0.001")
    expect_equal(prettify_p_value(0.052147), " 0.05 ")

    data("CO2")
    data <- test_normality(uptake ~ Type, data = CO2)
    rez  <- prettify_p_column(data)

    classes_before <- purrr::map_chr(data, ~class(.))
    classes_after  <- purrr::map_chr(rez,  ~class(.))

    # Classes of other columns than "p.value" must not change
    expect_true(all(classes_before[-3] ==  classes_after[-3]))

    # Class of colmn "p.value" changes to "character"
    expect_match(classes_before["p.value"], "numeric")
    expect_match(classes_after["p.value"], "character")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
context("Function `SIGNIF`")

test_that("`SIGNIF()` works", {
    expect_equal(SIGNIF(0.005),    0.005)
    expect_is(SIGNIF(0.005),   "numeric")
    expect_is(SIGNIF("0.005"), "numeric")
})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
context("Function `adjust_vector_length`")

test_that("`adjust_vector_length()` works", {
    expect_length(adjust_vector_length(2, CO2),    length(CO2))
    expect_length(adjust_vector_length(1:5, CO2),    length(CO2))
    expect_error(adjust_vector_length(2:3, CO2))

})