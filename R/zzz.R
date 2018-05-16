.onAttach <-
    function(libname, pkgname)
{
    if (!interactive())
        return()

    version <- version()

    valid <- .version_validity(version)
    if (!isTRUE(valid))
        .stop(valid)

    fmt <- paste0(
        "Bioconductor version %s (BiocManager %s), ",
        "?BiocManager::install for help"
    )
    .message(fmt, version(), packageVersion("BiocManager"))

    recommend <- .version_recommend(version)
    if (!isTRUE(recommend))
        .message(recommend)
}
