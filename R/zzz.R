.onAttach <-
    function(libname, pkgname)
{
    .message("Bioconductor version %s (BiocManager version %s), ?install for help",
             version(), packageVersion("BiocManager"))

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
