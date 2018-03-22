.onAttach <-
    function(libname, pkgname)
{
    .message("Bioconductor version %s (package version %s), ?install for help",
             version(), packageVersion("Bioconductor"))

    opt <- getOption(
        "BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS",
        Sys.getenv("BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS", TRUE)
    )
    if (isTRUE(opt))
        return()

    ver_result <- .version_diagnosis()
    if (!isTRUE(ver_result))
        .message(ver_result)
}
