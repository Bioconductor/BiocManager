.onLoad <-
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
    isTRUE(valid) || .warning(valid)

    fmt <- paste0(
        "Bioconductor version %s (BiocManager %s), ",
        "?BiocManager::install for help"
    )
    .message(fmt, version, packageVersion("BiocManager"))

    txt <- .version_is_not_future(version)
    isTRUE(txt) || identical(txt, .VERSION_MAP_UNABLE_TO_VALIDATE) ||
        .message(txt)

    recommend <- .version_recommend(version)
    isTRUE(recommend) || .message(recommend)
}
