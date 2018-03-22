context("Filter URLs, CRAN and GitHub packages")

test_that("Helpers filter the right packages", {
    .rRepos <- Bioconductor:::.rRepos
    .githubRepos <- Bioconductor:::.githubRepos

    r <- "foo"
    http <- c("http://foo.bar/baz", "https://foo.bar/baz")
    github <- c("foo/bar", "foo/bar@baz")
    all <- c(r, http, github)

    expect_identical(c(r, http), .rRepos(all))
    expect_identical(github, .githubRepos(all))
})

test_that("Versions are checked in install", {
    expect_error(install(version = "0.1"))
    expect_error(install(1:3))
    expect_error(install(NA))
    expect_error(install(c("BioStrings", "S4Vectors", NA)))
    expect_error(install(site_repository = c("string1", "string2")))
    expect_error(install(TRUE))
    expect_error(install(ask = "No"))
    expect_error(install(ask = c("No", "Yes", NA)))
    expect_error(install(version = c("3.7", "3.6")))
    expect_error(install(version = "3.4.2"))
})
