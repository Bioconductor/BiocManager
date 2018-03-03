context("Selecting appropriate version of Bioconductor")

test_that("useDevel works", {
    if (!Bioconductor:::IS_DOWNGRADEABLE)
        expect_error(useDevel(FALSE), silent=TRUE)
    if (!Bioconductor:::IS_UPGRADEABLE) {
        expect_error(useDevel(), silent=TRUE)
        opts <- options(warn=2); on.exit(options(opts))
        expect_error(install("BiocUpgrade"))
    }
})

test_that("ContribUrl exists", {
    skip("Bioconductor package not yet in repository")
    fun <- Bioconductor:::.getContribUrl

    vers <- Bioconductor:::BIOC_VERSION
    expect_true(grepl(vers, fun(vers)))
    if (Bioconductor:::IS_UPGRADEABLE) {
        vers <- Bioconductor:::UPGRADE_VERSION
        expect_true(grepl(vers, fun(vers)))
    }
    if (Bioconductor:::IS_DOWNGRADEABLE) {
        vers <- Bioconductor:::DOWNGRADE_VERSION
        expect_true(grepl(vers, fun(vers)))
    }
})
