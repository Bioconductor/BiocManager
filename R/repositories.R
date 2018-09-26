.repositories_base <-
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
    name_is_CRAN <- names(repos) == "CRAN"
    if (length(name_is_CRAN) == 0L)     # NULL names
        name_is_CRAN <- logical(length(repos))
    snapshot_pattern <- "/snapshot/20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
    rename <- name_is_CRAN & grepl(snapshot_pattern, repos)

    ## update "@CRAN@" to default
    rename <- rename | (repos == "@CRAN@")

    repos[rename] <- "https://cran.rstudio.com"
    repos
}

#' @importFrom stats setNames
.repositories_bioc <-
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

.repositories <-
    function(site_repository, version)
{
    base <- .repositories_base()
    bioc <- .repositories_bioc(version)

    repos <- c(site_repository = site_repository, bioc, base)
    repos[!duplicated(names(repos))]
}

#' Display current Bioconductor and CRAN repositories.
#'
#' `repositories()` reports the URLs from which to install
#' _Bioconductor_ and CRAN packages. It is used by
#' `BiocManager::install()` and other functions.
#'
#' @param site_repository (Optional) `character(1)` representing an
#'     additional repository (e.g., a URL to an organization's
#'     internally maintained repository) in which to look for packages
#'     to install. This repository will be prepended to the default
#'     repositories returned by the function.
#' @param version (Optional) `character(1)` or `package_version`
#'     indicating the _Bioconductor_ version (e.g., "3.8") for which
#'     repositories are required.
#' @return Named `character()` of repositories.
#' @seealso
#'
#' `BiocManager::\link{install}()` Installs or updates Bioconductor,
#'  CRAN, and GitHub packages.
#'
#' `\link{chooseBioCmirror}()` choose an alternative Bioconductor
#' mirror; not usually necessary.
#'
#' `\link{chooseCRANmirror}()` choose an alternative CRAN mirror; not
#' usually necessary.
#'
#' `\link{setRepositories}()` Select additional repositories for
#' searching.
#'
#' @keywords environment
#'
#' @examples
#' BiocManager::repositories()
#' \dontrun{
#' BiocManager::repositories(version="3.8")
#' }
#'
#' @md
#' @export repositories
repositories <-
    function(site_repository = character(), version = BiocManager::version())
{
    stopifnot(
        length(site_repository) <= 1L,
        is.character(site_repository), !anyNA(site_repository)
    )
    version <- .version_validate(version)
    .repositories(site_repository, version)
}
