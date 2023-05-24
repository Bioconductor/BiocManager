.onAttach <-
    function(libname, pkgname)
{
    version <- version()

    validity <- .version_validity(version, check_future = TRUE)
    isTRUE(validity) || .packageStartupMessage(validity)

    if (interactive() && isTRUE(validity))
        .packageStartupMessage(.version_string(version))

    recommend <- .version_recommend(version)
    isTRUE(recommend) || .packageStartupMessage(recommend)
}
