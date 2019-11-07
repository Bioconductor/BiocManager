context("repositories()")

test_that("repositories() returns all repos", {
    .skip_if_misconfigured()
    skip_if_offline()
    allOS <- c("BioCsoft", "CRAN", "BioCann", "BioCexp", "BioCworkflows")
    expect_true(all(allOS %in% names(repositories())))
})

test_that("repositories() does not return any NA repos", {
    .skip_if_misconfigured()
    skip_if_offline()
    expect_true(!anyNA(repositories()))
})

test_that("repositories() returns expected order", {
    .skip_if_misconfigured()
    skip_if_offline()
    expect_identical("BioCsoft", names(repositories())[[1]])
})

test_that("'site_repository=' inserted correctly", {
    .skip_if_misconfigured()
    skip_if_offline()
    site_repository <- "file:///tmp"
    repos <- repositories(site_repository)
    expect_identical(c(site_repository = site_repository), repos[1])
})

test_that("repositories() rejects invalid versions", {
    skip_if_offline()
    expect_error(
        repositories(version="2.0"),
        "Bioconductor version '2.0' requires R version '2.5'.*"
    )
    ## other validations tested in test_version.R
})

test_that("repositories(version = 'devel') works", {
    .skip_if_misconfigured()
    skip_if_offline()
    if (version() == .version_bioc("devel")) {
        expect_equal(
            repositories(version = .version_bioc("devel")),
            repositories(version = "devel")
        )
    }
})

test_that("repositories helper replaces correct URL", {
    default_repos <- c(CRAN = "https://cran.rstudio.com")

    ## https://github.com/Bioconductor/BiocManager/issues/17
    repos <- "http://cran.cnr.Berkeley.edu"
    withr::with_options(list(repos = repos), {
        expect_equal(.repositories_base(), repos)
    })

    ## works as advertised
    repos <- c(CRAN = "@CRAN@")         # provide default repo
    withr::with_options(list(repos = repos), {
        expect_equal(.repositories_base(), default_repos)
    })

    repos <-                            # update CRAN-named repo with mran URL
        c(CRAN = "https://mran.microsoft.com/snapshot/2017-05-01")
    withr::with_options(list(repos = repos), {
        expect_equal(.repositories_base(), default_repos)
    })

    repos <-                            # DO NOT update non-CRAN mran
        c(FOO = "https://mran.microsoft.com/snapshot/2017-05-01")
    withr::with_options(list(repos = repos), {
        expect_equal(.repositories_base(), repos)
    })

    repos <-                            # DO NOT update non-mran CRAN
        c(CRAN = "http://cran.cnr.Berkeley.edu")
    withr::with_options(list(repos = repos), {
        expect_equal(.repositories_base(), repos)
    })

    ## edge cases?
    repos <- character()                # no repositories
    withr::with_options(list(repos = repos), {
        expect_equal(.repositories_base(), repos)
    })

    repos <- "@CRAN@"                   # unnamed
    withr::with_options(list(repos = repos), {
        expect_equal(.repositories_base(), unname(default_repos))
    })
})
