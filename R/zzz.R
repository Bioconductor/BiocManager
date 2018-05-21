.onAttach <-
    function(libname, pkgname)
{
    if (!interactive())
        return()

    version <- version()
    if (identical(version, .VERSION_SENTINEL)) {
        .warning(.VERSION_UNKNOWN)
        return()
    }

    valid <- .version_validity(version)
    if (identical(valid, .VERSION_MAP_UNABLE_TO_VALIDATE)) {
        .warning(valid)
        return()
    }
    isTRUE(valid) || .stop(valid)

    fmt <- paste0(
        "Bioconductor version %s (BiocManager %s), ",
        "?BiocManager::install for help"
    )
    .message(fmt, version, packageVersion("BiocManager"))

    txt <- .version_is_not_future(version)
    if (!isTRUE(txt))
        .message(txt)

    recommend <- .version_recommend(version)
    if (!isTRUE(recommend))
        .message(recommend)
}
