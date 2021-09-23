context("repositories()")

test_that("repositories() returns all repos", {
    .skip_if_misconfigured()
    skip_if_offline()
    allOS <- c("BioCsoft", "CRAN", "BioCann", "BioCexp", "BioCworkflows")
    if (version() >= "3.12")
        allOS <- c(allOS, "BioCbooks")
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
    .skip_if_misconfigured()
    default_repos <- c(CRAN = "https://cloud.r-project.org")

    ## https://github.com/Bioconductor/BiocManager/issues/17
    repos <- "http://cran.cnr.Berkeley.edu"
    withr::with_options(list(repos = repos), {
        expect_equal(.repositories_base(), repos)
    })

    ## works as advertised
    repos <- c(CRAN = "@CRAN@")
    withr::with_options(list(repos = repos), {
        expect_equal(.repositories_base(), default_repos)
    })

    ## DO NOT update CRAN repo
    repos <- c(CRAN = "https://mran.microsoft.com/snapshot/2017-05-01")
    withr::with_options(list(
               repos = repos
           ), {
               expect_message(.repositories_base())
               expect_equal(.repositories_base(), repos)
               expect_message(repositories(), "'getOption\\(\"repos\"\\)'")
           })

    ## ...unless BiocManager.check_repositories == TRUE
    withr::with_options(list(
               repos = repos,
               BiocManager.check_repositories = FALSE
           ), {
               expect_equal(.repositories_base(), repos)
           })

    ## DO NOT update other repositories...
    withr::with_options(list(
               repos = c(BioCsoft = "foo.bar")
           ), {
               expect_message(.repositories_base())
           })

    ## ...unless BiocManager.check_repositories == FALSE
    repos <- c(BioCsoft = "foo.bar")
    withr::with_options(list(           # other renameing
               repos = repos,
               BiocManager.check_repositories = FALSE
           ), {
               expect_equal(.repositories_base(), repos)
           })

    ## edge cases?
    repos <- character()                # no repositories
    withr::with_options(list(repos = repos), {
        expect_equal(.repositories_base(), repos)
    })

    repos <- "@CRAN@"                   # unnamed
    withr::with_options(list(
               repos = repos
           ), {
               expect_equal(.repositories_base(), unname(default_repos))
           })
})

test_that("'.repositories_filter()' works", {
    .skip_if_misconfigured()
    skip_on_cran()
    skip_if_offline("bioconductor.org")
    repos0 <- BiocManager::repositories()
    expect_equal(.repositories_filter(repos0), repos0)
    repos  <- c(repos0, "https://bioconductor.org")
    expect_equal(.repositories_filter(repos), repos0)
})

test_that("'.repositories_base()' respects BiocManager.snapshot", {
    withr::with_options(
               list(BiocManager.snapshot = "FOO"),
               expect_error(.repositories_base(), "BiocManager.snapshot")
           )
    withr::with_options(
               list(BiocManager.snapshot = c("RSPM", "CRAN")),
               expect_error(.repositories_base(), "BiocManager.snapshot")
           )
})

test_that("'.repositories_container_binaries' & '.repositories_bioc' works", {
    skip_if_offline()
    bin_url <- "https://storage.googleapis.com/bioconductor_docker"
    ver <- "3.13"
    expected_url <- paste(bin_url, "packages", ver, "bioc", sep = "/")
    expected_url <- setNames(expected_url, "BioCbinaries")
    withr::with_envvar(
        list(
            "BIOCONDUCTOR_CONTAINER_BINARY_URL" = bin_url,
            "BIOCONDUCTOR_DOCKER_VERSION" = ver
        ),
        expect_identical(
            .repositories_container_binaries(
                version = ver,
                binary_base_url = bin_url
            ),
            expected_url
        )
    )
    withr::with_envvar(
        list("BIOCONDUCTOR_CONTAINER_BINARY_URL" = bin_url),
        expect_true(
          !"BioCbinaries" %in% names(.repositories_bioc(ver))
        )
    )
    withr::with_envvar(
        list("BIOCONDUCTOR_CONTAINER_BINARY_URL" = bin_url),
        expect_identical(
            .repositories_container_binaries(
                version = ver,
                binary_base_url = bin_url
            ),
            NULL
        )
    )
})
