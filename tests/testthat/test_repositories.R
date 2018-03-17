context("Pointer to repositories")

test_that("repositories() returns all repos", {
    allOS <- c("BioCsoft", "CRAN", "BioCann", "BioCexp")
    expect_true(all(allOS %in% names(repositories())))
})

test_that("repositories() does not return any NA repos", {
    expect_true(!any(is.na(repositories())))
})

test_that("repositories() returns expected order", {
    expect_identical("BioCsoft", names(repositories())[[1]])
})
