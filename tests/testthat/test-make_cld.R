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
    obj3 <- posthoc_tgh(chickwts$weight, chickwts$feed)
    expect_equivalent(as.character(make_cld(obj3)$cld),
                      c("a", "ab", "bcd", "bc", "d", "cd"))
})
