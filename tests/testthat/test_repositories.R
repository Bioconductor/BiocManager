context("repositories()")

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
        "Bioconductor version '2.0' requires R version '2.5'.*"
    )
    ## other validations tested in test_version.R
})

test_that("repositories(version = 'devel') works", {
    expect_equal(
        repositories(version = .version_bioc("devel")),
        repositories(version = "devel")
    )
})

test_that("repositories helper replaces correct URL", {
    repos <- c(
        CRAN1 = "https://cloud.r-project.org/",
        CRAN2 = "@CRAN@",
        CRAN3 = "https://mran.microsoft.com/snapshot/2017-05-01"
    )
    withr::with_options(list(repos = repos), {
        expect_equal(
            .repositories_base(),
            c(CRAN1 = "https://cloud.r-project.org/",
              CRAN2 = "https://cran.rstudio.com",
              CRAN3 = "https://cran.rstudio.com")
        )
    })
})
