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

test_that("repositories() returns BioCsoft in repositories vector", {
    .skip_if_misconfigured()
    skip_if_offline()
    expect_true("BioCsoft" %in% names(repositories()))
})

test_that("'site_repository=' inserted correctly", {
    .skip_if_misconfigured()
    skip_if_offline()
    site_repository <- "file:///tmp"
    repos <- repositories(site_repository)
    expect_identical(c(site_repository = site_repository), repos[1])
})

test_that("site_repository is used when env var or option available", {
    .skip_if_misconfigured()
    expected_repository <- "https://example.com/bioc"
    withr::with_envvar(
        c(BIOCMANAGER_SITE_REPOSITORY = expected_repository),
        {
            actual_repository <- .repositories_site_repository()
            expect_equal(actual_repository, expected_repository)
        }
    )
    withr::with_envvar(
        c(BIOCMANAGER_SITE_REPOSITORY = ""),
        {
            actual_repository <- .repositories_site_repository()
            expect_equal(actual_repository, character(0L))
        }
    )
    withr::with_envvar(
        c(BIOCMANAGER_SITE_REPOSITORY = NULL),
        {
            actual_repository <- .repositories_site_repository()
            expect_equal(actual_repository, character(0L))
        }
    )
    withr::with_options(
        list(BiocManager.site_repository = expected_repository),
        {
            actual_value <- .repositories_site_repository()
            expect_equal(actual_value, expected_repository)
        }
    )
    withr::with_options(
        list(BiocManager.site_repository = ""),
        {
            actual_value <- .repositories_site_repository()
            expect_equal(actual_value, character(0L))
        }
    )
    withr::with_options(
        list(BiocManager.site_repository = NULL),
        {
            actual_value <- .repositories_site_repository()
            expect_equal(actual_value, character(0L))
        }
    )
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
    withr::with_options(
        list(repos = repos),
        expect_equal(.repositories_base(), repos)
    )

    ## works as advertised
    repos <- c(CRAN = "@CRAN@")
    withr::with_options(
        list(repos = repos),
        expect_equal(.repositories_base(), default_repos)
    )

    ## DO NOT update CRAN repo
    repos <- c(CRAN = "https://packagemanager.posit.co/")
    withr::with_options(
        list(repos = repos),
        {
            expect_message(.repositories_base())
            expect_equal(.repositories_base(), repos)
            expect_message(repositories(), "'getOption\\(\"repos\"\\)'")
        }
    )

    ## ...unless BiocManager.check_repositories == TRUE
    withr::with_options(
        list(repos = repos, BiocManager.check_repositories = FALSE),
        expect_equal(.repositories_base(), repos)
    )

    ## DO NOT update other repositories...
    withr::with_options(
        list(repos = c(BioCsoft = "foo.bar")),
        expect_message(.repositories_base())
    )

    ## ...unless BiocManager.check_repositories == FALSE
    repos <- c(BioCsoft = "foo.bar")
    # other renaming
    withr::with_options(
        list(repos = repos, BiocManager.check_repositories = FALSE),
        expect_equal(.repositories_base(), repos)
    )

    ## edge cases?
    repos <- character()                # no repositories
    withr::with_options(
        list(repos = repos),
        expect_equal(.repositories_base(), repos)
    )

    repos <- "@CRAN@"                   # unnamed
    withr::with_options(
        list(repos = repos),
        expect_equal(.repositories_base(), unname(default_repos))
    )
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

test_that("'containerRepository' uses 'type' argument", {
    skip_if_offline()
    bin_url <- "https://bioconductor.org/packages/%s/container-binaries/%s"
    ver <- "3.14"
    bin_url <- sprintf(bin_url, ver, "bioconductor_docker")
    expected_url <- setNames(bin_url, "BioCcontainers")
    withr::with_envvar(
        c(
            "BIOCONDUCTOR_DOCKER_VERSION" = ver,
            "BIOCONDUCTOR_USE_CONTAINER_REPOSITORY" = TRUE
        ),
        expect_identical(
            containerRepository(version = ver, type = "source"),
            character(0L)
        )
    )
    withr::with_envvar(
        c(
            "BIOCONDUCTOR_DOCKER_VERSION" = ver,
            "BIOCONDUCTOR_USE_CONTAINER_REPOSITORY" = TRUE
        ),
        expect_identical(
            containerRepository(version = ver, type = "both"),
            expected_url
        )
    )
    withr::with_envvar(
        c(
            "BIOCONDUCTOR_DOCKER_VERSION" = ver,
            "BIOCONDUCTOR_USE_CONTAINER_REPOSITORY" = TRUE
        ),
        expect_identical(
            containerRepository(version = ver, type = "binary"),
            expected_url
        )
    )
})

test_that("'containerRepository' & '.repositories_bioc' works", {
    skip_if_offline()
    bin_url <- "https://bioconductor.org/packages/%s/container-binaries/%s"
    ver <- "3.14"
    bin_url <- sprintf(bin_url, ver, "bioconductor_docker")
    expected_url <- setNames(bin_url, "BioCcontainers")
    # When no BIOCONDUCTOR_DOCKER_VERSION is set, no 'BioCcontainers' repo
    withr::with_envvar(
        c(
            "BIOCONDUCTOR_USE_CONTAINER_REPOSITORY" = TRUE,
            "BIOCONDUCTOR_DOCKER_VERSION" = ""
        ),
        expect_true(
            !"BioCcontainers" %in% names(.repositories_bioc(ver))
        )
    )
    # When no BIOCONDUCTOR_DOCKER_VERSION is set, expect empty string
    withr::with_envvar(
        c(
            "BIOCONDUCTOR_USE_CONTAINER_REPOSITORY" = TRUE,
            "BIOCONDUCTOR_DOCKER_VERSION" = ""
        ),
        expect_identical(
            containerRepository(version = ver),
            character(0L)
        )
    )
    # When BIOCONDUCTOR_DOCKER_VERSION is set and
    # BIOCONDUCTOR_USE_CONTAINER_REPOSITORY is true, expect binaries url
    withr::with_envvar(
        c(
            "BIOCONDUCTOR_USE_CONTAINER_REPOSITORY" = TRUE,
            "BIOCONDUCTOR_DOCKER_VERSION" = ver
        ),
        expect_identical(
            containerRepository(version = ver),
            expected_url
        )
    )
    # When no BIOCONDUCTOR_DOCKER_VERSION is set, expect empty string
    withr::with_envvar(
        c(
            "BIOCONDUCTOR_USE_CONTAINER_REPOSITORY" = FALSE,
            "BIOCONDUCTOR_DOCKER_VERSION" = ver
        ),
        expect_identical(
            containerRepository(version = ver),
            character(0L)
        )
    )
})
