context("make_cld")

test_that("`make_cld.pairwise.htest` works", {
    obj1 <- pairwise.wilcox.test(chickwts$weight,
                                 chickwts$feed,
                                 exact = FALSE)
    expect_equivalent(as.character(make_cld(obj1)$cld),
                      c("a", "b", "bc", "ac", "c", "a"))
})

test_that("`make_cld.PMCMR` works", {
    expect_warning(
    obj2 <- PMCMR::posthoc.kruskal.conover.test(weight ~ feed,
                                                data = chickwts)
    )

    expect_equivalent(as.character(make_cld(obj2)$cld),
                      c("a", "b", "bc", "ac", "c", "a"))
})

test_that("`make_cld.posthocTGH` works", {
    # Temporary test
    obj3 <- posthoc_anova(chickwts$weight, chickwts$feed)
    class(obj3) <- "posthocTGH"
    expect_equivalent(as.character(make_cld(obj3)$cld),
                      c("a", "ab", "bcd", "bc", "d", "cd"))
})

test_that("`make_cld.posthoc_anova` works", {
    obj9 <- posthoc_anova(chickwts$weight, chickwts$feed)
    expect_equivalent(as.character(make_cld(obj9)$cld),
                      c("a", "ab", "bcd", "bc", "d", "cd"))
})

test_that("`make_cld.matrix` (symetric) works", {
    m <- c(1.00, 0.22, 0.05, 0.00,
           0.22, 1.00, 0.17, 0.01,
           0.05, 0.17, 1.00, 0.22,
           0.00, 0.01, 0.22, 1.00)
    obj7 <- matrix(m, nrow = 4)
    rownames(obj7) <- colnames(obj7) <- c("P", "O", "I", "U")

    expect_equivalent(as.character(make_cld(obj7)$cld),
                      c("a", "a", "ab", "b"))
})
