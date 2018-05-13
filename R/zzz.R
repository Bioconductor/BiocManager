.onAttach <-
    function(libname, pkgname)
{
    if (!interactive())
        return()

    version <- version()

    valid <- .version_validity(version)
    if (!isTRUE(valid))
        .stop(valid)

    .message(
        "Bioconductor version %s (BiocManager %s), ?install for help",
        version(), packageVersion("BiocManager")
    )

    recommend <- .version_recommend(version)
    if (!isTRUE(recommend))
        .message(recommend)
}
