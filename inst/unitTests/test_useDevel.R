test_useDevel <- function()
{
    if (!Bioconductor:::IS_DOWNGRADEABLE)
        checkException(useDevel(FALSE), silent=TRUE)
    if (!Bioconductor:::IS_UPGRADEABLE) {
        checkException(useDevel(), silent=TRUE)
        opts <- options(warn=2); on.exit(options(opts))
        checkException(biocLite("BiocUpgrade"))
    }
}

test_getContribUrl_exist <- function()
{
    fun <- Bioconductor:::.getContribUrl

    vers <- Bioconductor:::BIOC_VERSION
    checkTrue(grepl(vers, fun(vers)))
    if (Bioconductor:::IS_UPGRADEABLE) {
        vers <- Bioconductor:::UPGRADE_VERSION
        checkTrue(grepl(vers, fun(vers)))
    }
    if (Bioconductor:::IS_DOWNGRADEABLE) {
        vers <- Bioconductor:::DOWNGRADE_VERSION
        checkTrue(grepl(vers, fun(vers)))
    }
}
