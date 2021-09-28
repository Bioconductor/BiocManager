.onAttach <-
    function(libname, pkgname)
{
    version <- version()

    validity <- .version_validity(version)
    isTRUE(validity) || .packageStartupMessage(validity)

    if (interactive() && isTRUE(validity))
        .packageStartupMessage(.version_string(version))

    recommend <- .version_recommend(version)
    isTRUE(recommend) || .packageStartupMessage(recommend)
}

.onLoad <- function(libname, pkgname) {

    bin_url <- getOption(
        "BiocManager.container_binary_repos",
        "https://bioconductor.org/packages"
    )
    options("BiocManager.container_binary_repos" = bin_url)

}
