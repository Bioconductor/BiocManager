.base_repositories <-
    function()
{
    repos <- getOption("repos")
    ## Microsoft R Open is shipped with getOption("repos")[["CRAN"]]
    ## pointing to a *snapshot* of CRAN (e.g.
    ## https://mran.microsoft.com/snapshot/2017-05-01), and not to a
    ## CRAN mirror that is current. For the current release and devel
    ## BioC versions, repositories() needs to point to a CRAN mirror
    ## that is current so install() and valid() behave the same for
    ## all BioC users, whether they use mainstream R or Microsoft R
    ## Open.  However, since old versions of BioC are frozen, it would
    ## probably make sense to point to a *snapshot* of CRAN instead of
    ## a CRAN mirror that is current.
    cran <- repos[["CRAN"]]
    snapshot_pattern <- "/snapshot/20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
    if (cran == "@CRAN@" || grepl(snapshot_pattern, cran))
        repos[["CRAN"]] <- "https://cran.rstudio.com"

    repos
}

#' @importFrom stats setNames
.bioc_repositories <-
    function(version)
{
    mirror <- getOption("BioC_mirror", "https://bioconductor.org")
    paths <- c(
        BioCsoft = "bioc", BioCann = "data/annotation",
        BioCexp = "data/experiment", BioCworkflows = "workflows"
    )
    bioc_repos <- paste(mirror, "packages", version, paths, sep="/")
    setNames(bioc_repos, names(paths))
}

#' Display current Bioconductor and CRAN repositories.
#'
#' Displays the URLs of the repositories used by
#' \code{\link{install}()} to install Bioconductor and CRAN packages.
#'
#' @param site_repository (Optional) \code{character(1)} representing
#'     an additional repository (e.g., a URL to an organization's
#'     internally maintained repository) in which to look for packages
#'     to install. This repository will be prepended to the default
#'     repositories returned by the function.
#' @param version (Optional) \code{character(1)} or
#'     \code{package_version} indicating the Bioconductor version
#'     (e.g., "3.8") for which repositories are required.
#' @return Named \code{character()} of repositories.
#' @seealso
#'
#' \code{\link{install}()} Installs or updates Bioconductor, CRAN, and
#'   github packages.
#'
#' \code{\link{chooseBioCmirror}()} choose an alternative Bioconductor
#'   mirror; not usually necessary.
#'
#' \code{\link{chooseCRANmirror}()} choose an alternative CRAN mirror;
#'   not usually necessary.
#'
#' \code{\link{setRepositories}()} Select additional repositories for
#'   searching.
#'
#' @keywords environment
#'
#' @examples
#' repositories()
#' repositories(version="3.6")
#' pkgs <- available.packages(repos=repositories()["BioCsoft"])
#' nrow(pkgs)          # how many Bioconductor packages available to you?
#'
#' @export repositories
repositories <-
    function(site_repository = character(), version = BiocManager::version())
{
    stopifnot(
        length(site_repository) <= 1L,
        is.character(site_repository), !anyNA(site_repository)
    )
    version <- .version_validate(version)

    base_repos <- .base_repositories()
    bioc_repos <- .bioc_repositories(version)

    repos <- c(site_repository = site_repository, bioc_repos, base_repos)
    repos[!duplicated(names(repos))]
}
