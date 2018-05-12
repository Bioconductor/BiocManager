context("Pointer to repositories")

test_that("repositories() returns all repos", {
    allOS <- c("BioCsoft", "CRAN", "BioCann", "BioCexp", "BioCworkflows")
    expect_true(all(allOS %in% names(repositories())))
})

test_that("repositories() does not return any NA repos", {
    expect_true(!anyNA(repositories()))
})

test_that("repositories() returns expected order", {
    expect_identical("BioCsoft", names(repositories())[[1]])
})

test_that("'site_repository=' inserted correctly", {
    site_repository <- "file:///tmp"
    repos <- repositories(site_repository)
    expect_identical(c(site_repository = site_repository), repos[1])
})

test_that("repositories() rejects invalid versions", {
    expect_error(
        repositories(version="2.0"),
        "Bioconductor version 2.0 requires R version 2.5"
    )
    ## other validations tested in test_version.R
})

test_that("repositories() returns sentinel links", {
    if ("BiocVersion" %in% rownames(installed.packages()))
        skip("Removal of BiocVersion required")
    repos <- repositories()
    bioc <- repos[startsWith(names(repos), "BioC")]
    expect_true(all(grepl("0.0", bioc, fixed = TRUE)))
})
