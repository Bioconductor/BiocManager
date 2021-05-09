.onAttach <-
    function(libame, pkgname)
{
    version <- version()

    validity <- .version_validity(version)
    isTRUE(validity) || .packageStartupMessage(validity)

    if (interactive() && isTRUE(validity))
        .packageStartupMessage(.version_string(version))

    recommend <- .version_recommend(version)
    isTRUE(recommend) || .packageStartupMessage(recommend)
}
