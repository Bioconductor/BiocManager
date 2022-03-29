BINARY_BASE_URL <- "https://bioconductor.org/packages/%s/container-binaries/%s"

.repositories_check_repos <-
    function(repos, useContainerRepository)
{
    conflict <-
        names(repos) %in% c(
            names(.repositories_bioc(version(), useContainerRepository)),
            "CRAN"
        )
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
        if (getOption("BiocManager.check_repositories", TRUE))
            .message(fmt, repos_string, call. = FALSE, wrap. = FALSE)
    }

    repos
}

.repositories_rspm <-
    function(cran)
{
    rspm_version <- .version_field("RSPM")
    if (is.na(rspm_version)) {
        cran
    } else {
        rspm_version <- format(as.Date(rspm_version, "%m/%d/%Y"), "%Y-%m-%d")
        paste0("https://packagemanager.rstudio.com/cran/", rspm_version)
    }
}

.repositories_mran <-
    function(cran)
{
    mran_version <- .version_field("MRAN")
    if (is.na(mran_version)) {
        cran
    } else {
        mran_version <- format(as.Date(mran_version, "%m/%d/%Y"), "%Y-%m-%d")
        paste0("https://mran.microsoft.com/snapshot/", mran_version)
    }
}

.repositories_base <-
    function(useContainerRepository)
{
    repos <- getOption("repos")
    repos <- .repositories_check_repos(repos, useContainerRepository)
    rename <- repos == "@CRAN@"
    if (any(rename)) {
        ## default <- if (version() > "3.11") "MRAN" else "CRAN"
        opt <- getOption("BiocManager.snapshot", "CRAN")
        valid <- c("CRAN", "MRAN", "RSPM")
        if (length(opt) != 1L || !opt %in% valid)
            .stop(
                "'getOption(\"BiocManager.snapshot\")' must be one of %s",
                paste0("'", valid, "'", collapse = " ")
            )
        cran <- "https://cloud.r-project.org"
        repos[rename] <- switch(
            opt,
            RSPM = .repositories_rspm(cran),
            MRAN = .repositories_mran(cran),
            CRAN = cran,
            .stop("unknown option 'BiocManager.snapshot = \"%s\"'", opt)
        )
    }

    repos
}

#' @importFrom stats setNames
.repositories_bioc <-
    function(version, useContainerRepository)
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
        containerRepository(useContainerRepository = useContainerRepository),
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
    function(site_repository, version, useContainerRepository)
{
    base <- .repositories_base(useContainerRepository)
    bioc <- .repositories_bioc(version, useContainerRepository)

    repos <- c(site_repository = site_repository, bioc, base)
    repos[!duplicated(names(repos))]
}

#' @title Display current Bioconductor and CRAN repositories.
#'
#' @aliases BiocManager.snapshot BiocManager.check_repositories
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
#' CRAN packages for out-of-date _Bioconductor_ installations can be
#' installed from historical 'snapshots' consistent with the last date
#' the Bioconductor version was current.  This behavior can be specified with
#' `BiocManager.snapshot` For example, _Bioconductor_ version 3.11 was current
#' until October 28, 2020; CRAN packages are therefore installed from
#' a snapshot created on 2020-10-28. By default, the snapshots are
#' from 'MRAN', the [Microsoft R Archive Network][MRAN]. Use
#' `options(BiocManager.snapshot = "RSPM")` to instead use the
#' [RStudio Package Manager][RSPM], or `options(BiocManager.snapshot =
#' "CRAN")` to use the current CRAN repository (i.e., disabling the
#' snapshot feature).
#'
#' [MRAN]: https://mran.microsoft.com/timemachine
#' [RSPM]: https://packagemanager.rstudio.com/client/#/repos/2/overview
#'
#' It may be desirable to specify different default repositories,
#' especially CRAN, for intentionally out-of-date _Bioconductor_
#' releases (e.g., to support reproducible research). Use the approach
#' provided by base _R_ to specify alternative repositories, e.g.,
#' `options(repos = c(CRAN =
#' "https://mran.microsoft.com/snapshot/2020-02-08"))`. This is
#' supported, but generates an error because specification of an
#' inappropriate CRAN repository (one providing packages not
#' consistent with the dates of the _Bioconductor_ release) results in
#' use of CRAN packages not consistent with _Bioconductor_ best
#' practices.
#'
#' To install binary packages on containerized versions of Bioconductor,
#' a default binary package location URL is set as a package constant,
#' see `BiocManager:::BINARY_BASE_URL`. Binary package installations
#' are enabled by default for Bioconductor Docker containers. Anyone
#' wishing to opt out of the binary package installation can set either the
#' variable or the option to a `""` value. Note that the availability
#' of Bioconductor package binaries is still experimental and that binary
#' installations are intended to be used with
#' `Bioconductor/bioconductor_docker` images where such installations
#' correspond to specific versions of Linux / Ubuntu.
#'
#' If alternative default repositories are known to provide
#' appropriate versions of CRAN or _Bioconductor_ packages, the warning
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
#' @return `repositories()`: named `character()` of repositories.
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
    function(
        site_repository = character(),
        version = BiocManager::version(),
        binary_base_url = BINARY_BASE_URL,
        useContainerRepository = TRUE
    )
{
    stopifnot(
        length(site_repository) <= 1L,
        is.character(site_repository), !anyNA(site_repository)
    )
    version <- .version_validate(version)
    .repositories(
        site_repository,
        version,
        useContainerRepository
    )
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
        platform <- "bioconductor_docker"
    } else {
        platform <- Sys.getenv("TERRA_R_PLATFORM")
        container_version <- Sys.getenv("TERRA_R_PLATFORM_BINARY_VERSION")
    }
    # platform and container_version are zero character vectors
    # when not running on a container
    list(platform = platform, container_version = container_version)
}

#' @rdname repositories
#'
#' @aliases BINARY_BASE_URL
#'
#' @description `containerRepository()` reports the location of the repository
#'     of binary packages for fast installation within containerized versions
#'     of Bioconductor, if available.
#'
#' @details The unexported URL to the base repository is available
#'     with `BiocManager:::BINARY_BASE_URL`.
#'
#' @param binary_base_url `character(1)` host and base path for binary
#'     package 'CRAN-style' repository; not usually required by the
#'     end-user.
#'
#' @param useContainerRepository `logical(1)` whether to allow fast binary
#'   package installations within Bioconductor containers (by default, `TRUE`);
#'   otherwise ignored. To opt-out of binary package installations within a
#'   container, set `useContainerRepository` to `FALSE`.
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
        version = BiocManager::version(),
        binary_base_url = BINARY_BASE_URL,
        useContainerRepository = TRUE
    )
{
    stopifnot(
        ## 'version' validated in '.repository_container_version_test()'
        .is_scalar_character(binary_base_url),
        .is_scalar_logical(useContainerRepository)
    )

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

    if (!useContainerRepository)
        return(character())

    ## does the binary repository exist?
    binary_repos0 <- sprintf(binary_base_url, version, platform)
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
