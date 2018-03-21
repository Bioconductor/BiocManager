.onAttach <-
    function(libname, pkgname)
{
    .message("Bioconductor version %s (package version %s), ?install for help",
             version(), packageVersion("Bioconductor"))
    ver_result <- .version_diagnosis()
    if (!isTRUE(ver_result))
        ver_result
}
