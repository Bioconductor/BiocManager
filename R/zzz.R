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
    isTRUE(valid) || ifelse(.is_CRAN_check(), .message(valid), .stop(valid))

    fmt <- paste0(
        "Bioconductor version %s (BiocManager %s), ",
        "?BiocManager::install for help"
    )
    .packageStartupMessage(fmt, version, packageVersion("BiocManager"))

    txt <- .version_is_not_future(version)
    isTRUE(txt) || identical(txt, .VERSION_MAP_UNABLE_TO_VALIDATE) ||
        .packageStartupMessage(txt)

    recommend <- .version_recommend(version)
    isTRUE(recommend) || .packageStartupMessage(recommend)
}
