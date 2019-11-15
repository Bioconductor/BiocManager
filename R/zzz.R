.onAttach <-
    function(libname, pkgname)
{
    version <- version()
    valid <- .version_validity(version)
    if (isTRUE(valid)) {
        fmt <- paste0(
            "Bioconductor version %s (BiocManager %s), ",
            "?BiocManager::install for help"
        )
        .packageStartupMessage(fmt, version, packageVersion("BiocManager"))
    }

    recommend <- .version_recommend(version)
    isTRUE(recommend) || .packageStartupMessage(recommend)
}
