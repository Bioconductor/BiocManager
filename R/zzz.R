.onAttach <-
    function(libname, pkgname)
{
    version <- version()

    validity <- .version_validity(version)
    isTRUE(validity) || .packageStartupMessage(validity)

    if (isTRUE(validity) && interactive())
        .packageStartupMessage(.version_string(version, digest = FALSE))

    recommend <- .version_recommend(version)
    isTRUE(recommend) || .packageStartupMessage(recommend)
}
