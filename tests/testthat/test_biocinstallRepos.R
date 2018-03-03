context("Pointer to repositories")

repos <- biocinstallRepos()

test_that("biocinstallRepos returns all repos", {
    allOS <- c("BioCsoft", "CRAN", "BioCann", "BioCexp")
    expect_true(all(allOS %in% names(repos)))
})

test_that("biocinstallRepos does not return any NA repos", {
    expect_true(!any(is.na(repos)))
})

test_that("biocinstallRepos returns expected order", {
    expect_identical("BioCsoft", names(repos)[[1]])
})
