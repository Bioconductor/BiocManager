.repositories_check_repos <-
    function(repos)
{
    conflict <-
        names(repos) %in% c(names(.repositories_bioc(version())), "CRAN")
    conflict <- conflict & repos != "@CRAN@"
    conflicts <- repos[conflict]

    ## FIXME: allow for MRAN repositories from appropriate dates for
    ## BiocManager::version()
    ##
    ## pattern <-
    ##     "snapshot/(20[[:digit:]]{2}-[[:digit:]]{2}-[[:digit:]]{2})/*$"
    ## is_snapshot <- grepl(pattern, repos)
    ## if (any(is_snapshot)) {
    ##     ...
    ## }

    if (length(conflicts)) {
        txt <- paste(
            "'getOption(\"repos\")' replaces Bioconductor standard ",
            "repositories, see '?repositories' for details"
        )
        fmt <- paste0(
            .msg(txt, exdent = 0),
            "\n\nreplacement repositories:",
            "\n    %s\n"
        )
        repos_string <- paste0(
            names(conflicts), ": ", unname(conflicts),
            collapse = "\n    "
        )

        FUN <- ifelse(
            getOption("BiocManager.check_repositories", TRUE),
            .stop, .message
        )
        FUN(fmt, repos_string, call. = FALSE, wrap. = FALSE)
    }

    repos
}

.repositories_base <-
    function()
{
    repos <- getOption("repos")
    repos <- .repositories_check_repos(repos)
    rename <- repos == "@CRAN@"
    repos[rename] <- "https://cran.rstudio.com"
    repos
}

#' @importFrom stats setNames
.repositories_bioc <-
    function(version)
{
    mirror <- getOption("BioC_mirror", "https://bioconductor.org")
    paths <- c(
        BioCsoft = "bioc",
        BioCann = "data/annotation",
        BioCexp = "data/experiment",
        BioCworkflows = "workflows",
        BioCbooks= "books"
    )
    bioc_repos <- paste(mirror, "packages", version, paths, sep="/")
    setNames(bioc_repos, names(paths))
}

.repositories_filter <-
    function(repos)
{
    ext <- c(".rds", ".gz", "")
    pkg_files <- paste0("/PACKAGES", ext)
    online <- logical(length(repos))
    for (pkg_file in pkg_files) {
        if (all(online))
            next
        urls <- paste0(contrib.url(repos[!online]), pkg_file)
        online[!online] <- vapply(urls, .url_exists, logical(1))
    }
    repos[online]
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
#'
#' @details
#'
#' _Bioconductor_ has a 'release' and a 'devel' semi-annual release
#' cycle. Packages within a release have been tested against each
#' other and the current version of packages on CRAN. _Bioconductor_
#' best practice is to use packages from the same release, and from
#' the appropriate CRAN repository. `repositories()` returns the
#' appropriate package repositories for your version of
#' _Bioconductor_.
#'
#' It may be desireable to specify different default repositories,
#' especially CRAN, for intentionally out-of-date _Bioconductor_
#' releases (e.g., to support reproducible research). Use the approach
#' provided by base _R_ to specify alternative repositories, e.g.,
#' `options(repos = c(CRAN =
#' "https://mran.microsoft.com/snapshot/2020-02-08"))`. This is
#' supported, but generates a warning because specification of an
#' inappropriate CRAN repository (one providing packages not
#' consistent with the dates of the _Bioconductor_ release) results in
#' use of CRAN packages not consistent with _Bioconductor_ best
#' practices.
#'
#' If alternative default repositories are known to provide
#' appropriate version of CRAN or _Bioconductor_ packages, the warning
#' may be silenced (displayed as a message) with
#' `options(BiocManager.check_repositories = FALSE)`. A message is
#' still printed, to serve as a reminder when debugging problems
#' related to incompatible package installation.
#'
#' The intended use of `site_repository =` is to enable installation of
#' packages not available in the default repositories, e.g., packages
#' internal to an organization and not yet publicly available. A
#' secondary use might provide alternative versions (e.g., compiled
#' binaries) of packages available in the default repositories. Note
#' that _R_'s standard rules of package selection apply, so the most
#' recent version of candidate packages is selected independent of the
#' location of the repository in the vector returned by `repositories()`.
#'
#' For greater flexiblity in installing packages while still adhering
#' as much as possible to _Bioconductor_ best practices, use
#' `repositories()` as a basis for constructing the `repos =` argument
#' to `install.packages()` and related functions.
#'
#' @return Named `character()` of repositories.
#'
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
