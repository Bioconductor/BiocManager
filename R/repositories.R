#'
#' Display current Bioconductor and CRAN repositories.
#'
#' Displays the URLs of the repositories used by \code{\link{install}} to
#' install Bioconductor and CRAN packages.
#'
#' @param site_repository (Optional) \code{character(1)} representing
#'     an additional repository (e.g., a URL to an organization's
#'     internally maintained repository) in which to look for packages
#'     to install. This repository will be prepended to the default
#'     repositories returned by the function.
#' @param version (Optional) \code{character(1)} or
#'     \code{package_version} indicating the Bioconductor version
#'     (e.g., \dQuote{3.1}) for which repositories are required.
#' @return Named \code{character()} of repositories.
#' @seealso
#'
#' \code{\link{install}} Installs/updates Bioconductor/CRAN packages.
#'
#' \code{\link{install.packages}} installs the packages themselves.
#'
#' \code{\link{chooseBioCmirror}} lets you choose from a list of all public
#'   Bioconductor mirror URLs.
#'
#' \code{\link{chooseCRANmirror}} lets you choose from a list of all public
#'   CRAN mirror URLs.
#'
#' @keywords environment
#'
#' @examples
#' repositories()
#' repositories(version="3.6")
#' pkgs <- available.packages(repos=repositories()["BioCsoft"])
#'
#' ## Choose CRAN mirrors
#' \dontrun{
#'   chooseCRANmirror()
#' }
#'
#' @export repositories
repositories <-
    function(site_repository = character(), version = Bioconductor::version())
{
    version <- package_version(version)
    stopifnot(
        length(site_repository) <= 1L,
        is.character(site_repository), !any(is.na(site_repository))
    )

    biocVersion <- package_version(version)

    old.repos <- getOption("repos")
    on.exit(options(repos = old.repos))

    ## Starting at some point in R-2.14, Omegahat is included in
    ## the list of available repositories, on windows only.
    ## Thus repository lists differ between OS and
    ## it's better not to rely on the repository list numbers.

    setRepositories(ind=1:20) # in case more repos are added
    repos <- getOption("repos")

    biocMirror <- getOption("BioC_mirror", "https://bioconductor.org")
    biocPaths <- c(
        BioCsoft="bioc", BioCann="data/annotation", BioCexp="data/experiment"
    )
    if (version >= "3.7")
        biocPaths[["BioCworkflows"]] <- "workflows"

    biocRepos <- paste(biocMirror, "packages", biocVersion, biocPaths, sep="/")
    repos[names(biocPaths)] <- biocRepos

    keepNames <- names(repos)[repos %in% old.repos]
    keepRepos <- unique(c(names(biocPaths), "CRAN", keepNames))

    repos <- repos[keepRepos]

    CRAN_repo <- repos[["CRAN"]]
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
    snapshot_pattern <- "/snapshot/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
    if (CRAN_repo == "@CRAN@" || grepl(snapshot_pattern, CRAN_repo))
        repos[["CRAN"]] <- "https://cran.rstudio.com"

    c(site_repository = site_repository, repos)
}
