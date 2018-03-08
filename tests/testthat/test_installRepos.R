context("Pointer to repositories")

repos <- installRepos()

test_that("installRepos returns all repos", {
    allOS <- c("BioCsoft", "CRAN", "BioCann", "BioCexp")
    expect_true(all(allOS %in% names(repos)))
})

test_that("installRepos does not return any NA repos", {
    expect_true(!any(is.na(repos)))
})

test_that("installRepos returns expected order", {
    expect_identical("BioCsoft", names(repos)[[1]])
})
