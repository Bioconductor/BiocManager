BINARY_BASE_URL <- "https://bioconductor.org/packages/%s/container-binaries/%s"

.repositories_check_repos_envopt <-
    function()
{
    opt <- Sys.getenv("BIOCMANAGER_CHECK_REPOSITORIES", TRUE)
    opt <- getOption("BiocManager.check_repositories", opt)
    isTRUE(as.logical(opt))
}

.repositories_site_repository <-
    function(site_repository = character())
{
    stopifnot(
        length(site_repository) == 0L || .is_scalar_character(site_repository)
    )
    if (!length(site_repository) || !nzchar(site_repository)) {
        site_repository <- Sys.getenv("BIOCMANAGER_SITE_REPOSITORY", "")
        site_repository <-
            getOption("BiocManager.site_repository", site_repository)

        if (!nzchar(site_repository))
            site_repository <- character()
    }
    site_repository
}

.repositories_check_repos <-
    function(repos)
{
    conflict <-
        names(repos) %in% c(names(.repositories_bioc(version())), "CRAN")
    conflict <- conflict & repos != "@CRAN@"
    conflicts <- repos[conflict]

    if (length(conflicts)) {
        txt <- paste(
            "'getOption(\"repos\")' replaces Bioconductor standard ",
            "repositories, see ",
            "'help(\"repositories\", package = \"BiocManager\")' for details."
        )
        fmt <- paste0(
            .msg(txt, exdent = 0),
            "\nReplacement repositories:",
            "\n    %s\n"
        )
        repos_string <- paste0(
            names(conflicts), ": ", unname(conflicts),
            collapse = "\n    "
        )
        if (.repositories_check_repos_envopt())
            .message(
                fmt, repos_string,
                call. = FALSE, wrap. = FALSE, appendLF = FALSE
            )
    }

    repos
}

.repositories_base <-
    function()
{
    repos <- getOption("repos")
    repos <- .repositories_check_repos(repos)
    rename <- repos == "@CRAN@"
    repos[rename] <- "https://cloud.r-project.org"
    repos
}

#' @importFrom stats setNames
.repositories_bioc <-
    function(version, ..., type = NULL)
{
    mirror <- getOption("BioC_mirror", "https://bioconductor.org")
    paths <- c(
        BioCsoft = "bioc",
        BioCann = "data/annotation",
        BioCexp = "data/experiment",
        BioCworkflows = "workflows",
        BioCbooks = if (version() >= "3.12") "books" else character()
    )
    bioc_repos <- paste(mirror, "packages", version, paths, sep="/")
    c(
        containerRepository(version = version, type = type),
        setNames(bioc_repos, names(paths))
    )
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
    function(site_repository, version, ...)
{
    base <- .repositories_base()
    bioc <- .repositories_bioc(version, ...)

    repos <- c(site_repository = site_repository, bioc, base)
    repos[!duplicated(names(repos))]
}

#' @title Display current Bioconductor and CRAN repositories.
#'
#' @aliases BiocManager.check_repositories
#'
#' @description `repositories()` reports the URLs from which to
#'     install _Bioconductor_ and CRAN packages. It is used by
#'     `BiocManager::install()` and other functions.
#'
#' @param site_repository (Optional) `character(1)` representing an
#'     additional repository (e.g., a URL to an organization's
#'     internally maintained repository) in which to look for packages
#'     to install. This repository will be prepended to the default
#'     repositories returned by the function.
#'
#' @param version (Optional) `character(1)` or `package_version`
#'     indicating the _Bioconductor_ version (e.g., "3.8") for which
#'     repositories are required.
#'
#' @param ... Additional parameters passed to lower level functions, not
#'   used.
#'
#' @param type (Optional) `character(1)` indicating the type of package
#'   repository to retrieve (default: "both"). Setting `type` to "source" will
#'   disable any Bioconductor binary packages specifically built for the
#'   containers.
#'
#' @details
#'
#' `repositories()` returns the appropriate software package
#' repositories for your version of _Bioconductor_.
#'
#' _Bioconductor_ has a 'release' and a 'devel' semi-annual release
#' cycle. Packages within a release have been tested against each
#' other and the current version of packages on CRAN. _Bioconductor_
#' best practice is to use packages from the same release, and from
#' the appropriate CRAN repository.
#'
#' To install binary packages on containerized versions of Bioconductor,
#' a default binary package location URL is set as a package constant,
#' see `BiocManager:::BINARY_BASE_URL`. Binary package installations
#' are enabled by default for Bioconductor Docker containers. Anyone
#' wishing to opt out of the binary package installation can set either the
#' variable or the option, \env{BIOCONDUCTOR_USE_CONTAINER_REPOSITORY}, to
#' `FALSE`. Note that the availability of Bioconductor package binaries is
#' experimental and binary installations are intended to be used with
#' `bioconductor/bioconductor_docker` images where such installations
#' correspond to specific versions of Linux / Ubuntu.
#'
#' If alternative default repositories are known to provide appropriate
#' versions of CRAN or _Bioconductor_ packages, the message may be silenced
#' by setting either the option or the variable to `FALSE`, i.e.,
#' `options(BiocManager.check_repositories = FALSE)` or
#' \env{BIOCMANAGER_CHECK_REPOSITORIES=FALSE}. Alternative default
#' repositories are not guaranteed to work without issues related to
#' incompatible package installations and are used at the user's own risk.
#'
#' The intended use of `site_repository =` is to enable installation of
#' packages not available in the default repositories, e.g., packages
#' internal to an organization and not yet publicly available. A
#' secondary use might provide alternative versions (e.g., compiled
#' binaries) of packages available in the default repositories. Note
#' that _R_'s standard rules of package selection apply, so the most
#' recent version of candidate packages is selected independent of the
#' location of the repository in the vector returned by `repositories()`.
#' To set a more permenanent `site_repository`, one can use either the
#' \env{BIOCMANAGER_SITE_REPOSITORY} environment variable or the
#' `options(BiocManager.site_repository = ...)` option.
#'
#' For greater flexiblity in installing packages while still adhering
#' as much as possible to _Bioconductor_ best practices, use
#' `repositories()` as a basis for constructing the `repos =` argument
#' to `install.packages()` and related functions.
#'
#' @return `repositories()`: named `character()` of repositories.
#'
#' @seealso
#'
#' \code{BiocManager::\link{install}()} Installs or updates Bioconductor,
#'  CRAN, and GitHub packages.
#'
#' \code{\link{chooseBioCmirror}()} choose an alternative Bioconductor
#' mirror; not usually necessary.
#'
#' \code{\link{chooseCRANmirror}()} choose an alternative CRAN mirror; not
#' usually necessary.
#'
#' \code{\link{setRepositories}()} Select additional repositories for
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
repositories <- function(
    site_repository = character(),
    version = BiocManager::version(),
    ...,
    type = "both"
) {
    site_repository <- .repositories_site_repository(site_repository)

    stopifnot(
        length(site_repository) <= 1L,
        is.character(site_repository), !anyNA(site_repository)
    )
    version <- .version_validate(version)
    .repositories(site_repository, version, ..., type = type)
}

## is the docker container configured correctly?
.repository_container_version_test <-
    function(bioconductor_version, container_version)
{
    bioconductor_version <- package_version(bioconductor_version)
    docker_version <- package_version(container_version)
    (bioconductor_version$major == docker_version$major) &&
        (bioconductor_version$minor == docker_version$minor)
}

## are we running on a docker container?
.repository_container_version <-
    function()
{
    container_version <- Sys.getenv("BIOCONDUCTOR_DOCKER_VERSION")
    if (nzchar(container_version)) {
        platform <- Sys.getenv("BIOCONDUCTOR_NAME", "bioconductor_docker")
    } else {
        platform <- Sys.getenv("TERRA_R_PLATFORM")
        container_version <- Sys.getenv("TERRA_R_PLATFORM_BINARY_VERSION")
    }
    # platform and container_version are zero character vectors
    # when not running on a container
    list(platform = platform, container_version = container_version)
}

.repositories_use_container_repo <-
    function()
{
    opt <- Sys.getenv("BIOCONDUCTOR_USE_CONTAINER_REPOSITORY", TRUE)
    opt <- getOption("BIOCONDUCTOR_USE_CONTAINER_REPOSITORY", opt)
    isTRUE(as.logical(opt))
}

#' @rdname repositories
#'
#' @aliases BINARY_BASE_URL
#'
#' @description `containerRepository()` reports the location of the repository
#'     of binary packages for fast installation within containerized versions
#'     of Bioconductor, if available.
#'
#' @details
#'
#' The unexported URL to the base repository is available with
#' `BiocManager:::BINARY_BASE_URL`.
#'
#' \env{BIOCONDUCTOR_USE_CONTAINER_REPOSITORY} is an environment
#' variable or global `options()` which, when set to `FALSE`, avoids
#' the fast installation of binary packages within containerized
#' versions of Bioconductor.
#'
#' @return `containerRepository()`: character(1) location of binary repository,
#'     if available, or character(0) if not.
#'
#' @examples
#' containerRepository() # character(0) if not within a Bioconductor container
#'
#' @importFrom utils contrib.url
#'
#' @md
#' @export
containerRepository <-
    function(
        version = BiocManager::version(), type = "binary"
    )
{
    if (identical(type, "source"))
        return(character())
    platform_docker <- .repository_container_version()
    container_version <- platform_docker$container_version
    platform <- platform_docker$platform

    ## are we running on a known container?
    if (!nzchar(container_version))
        return(character())

    ## do the versions of BiocManager::version() and the container match?
    versions_match <- .repository_container_version_test(
        version, container_version
    )
    if (!versions_match)
        return(character())

    if (!.repositories_use_container_repo())
        return(character())

    ## does the binary repository exist?
    binary_repos0 <- sprintf(BINARY_BASE_URL, version, platform)
    packages <- paste0(contrib.url(binary_repos0), "/PACKAGES.gz")
    url <- url(packages)
    tryCatch({
        suppressWarnings(open(url, "rb"))
        close(url)
        setNames(binary_repos0, "BioCcontainers")
    }, error = function(...) {
        close(url)
        character()
    })
}
