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
