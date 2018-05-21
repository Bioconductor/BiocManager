.package_filter_masked <-
    function(pkgs)
{
    path0 <- normalizePath(pkgs[, "LibPath"], winslash="/")
    path1 <- normalizePath(.libPaths(), winslash="/")
    idx <- order(match(path0, path1))
    dup <- duplicated(pkgs[idx,"Package"])[order(idx)]
    pkgs[!dup,, drop=FALSE]
}

.package_filter_unwriteable <-
    function(pkgs, instlib=NULL)
{
    if (!nrow(pkgs)) return(pkgs)

    libs <- if (is.null(instlib)) pkgs[,"LibPath"] else instlib

    ulibs <- unique(libs)
    status <- dir.exists(ulibs)

    if (.Platform$OS.type == "windows") {
        status[status] <- vapply(ulibs[status], function(lib) {
            ## from tools::install.R: file.access() unreliable on
            ## Windows
            fn <- file.path(lib, paste0("_test_dir", Sys.getpid()))
            unlink(fn, recursive = TRUE) # precaution
            res <- try(dir.create(fn, showWarnings = FALSE))
            if (inherits(res, "try-error") || !res) {
                FALSE
            } else {
                unlink(fn, recursive = TRUE)
                TRUE
            }
        }, logical(1))
    } else
        status[status] <- file.access(ulibs[status], 2L) == 0

    status <- status[match(libs, ulibs)]
    if (!all(status))
        .message(
            "installation path not writeable, unable to update packages: %s",
            paste(pkgs[!status, "Package"], collapse=", ")
        )

    pkgs[status,, drop=FALSE]
}

.install_filter_r_repos <-
    function(pkgs, invert = FALSE)
{
    grep("^(https?://.*|[^/]+)$", pkgs, invert = invert, value=TRUE)
}

.install_filter_github_repos <-
    function(pkgs)
{
    pkgs <- .install_filter_r_repos(pkgs, invert = TRUE)
    grep("^[^/]+/.+", pkgs, value=TRUE)
}

.install_github_load_remotes <-
    function(pkgs, lib.loc = NULL)
{
    if (!"remotes" %in% rownames(installed.packages(lib.loc))) {
        if (is.null(lib.loc))
            lib.loc <- .libPaths()
        stop(
            "package 'remotes' not installed in library path(s)",
            "\n    ", paste(lib.loc, collapse="\n    "),
            "\ninstall with 'install(\"remotes\")'",
            call.=FALSE
        )
    }

    tryCatch({
        loadNamespace("remotes", lib.loc)
    }, error=function(e) {
        stop(
            "'loadNamespace(\"remotes\")' failed:",
            "\n    ", conditionMessage(e)
        )
    })

    TRUE
}

.install_repos <-
    function(pkgs, lib, repos, ...)
{
    doing <- .install_filter_r_repos(pkgs)
    if (length(doing)) {
        pkgNames <- paste(.sQuote(doing), collapse=", ")
        .message("Installing package(s) %s", pkgNames)
        install.packages(pkgs = doing, lib = lib, repos = repos, ...)
    }
    setdiff(pkgs, doing)
}

.install_github <-
    function(pkgs, lib, lib.loc, repos = repos, ...)
{
    doing <- .install_filter_github_repos(pkgs)

    oopts <- options(repos = repos)     # required by remotes::
    on.exit(options(oopts))
    if (length(doing)) {
        pkgNames <- paste(.sQuote(doing), collapse=", ")
        .message("Installing github package(s) %s", pkgNames)
        .install_github_load_remotes(pkgs, lib.loc = lib.loc)
        for (repo in doing)
            remotes::install_github(repo, lib = lib, ...)
    }
    setdiff(pkgs, doing)
}

.install_validate_dots <-
    function(..., repos)
{
    if (!missing(repos))
        .stop("'repos' argument to 'install()' not allowed")

    TRUE
}

.install_ask_up_or_down_grade <-
    function(version, cmp)
{
    action <- if (cmp < 0) "Downgrade" else "Upgrade"
    txt <- sprintf("%s Bioconductor to version '%s'? [y/n]: ", action, version)
    .getAnswer(txt, allowed = c("y", "Y", "n", "N")) == "y"
}

.install <-
    function(pkgs, repos, lib.loc=NULL, lib=.libPaths()[1], ...)
{
    requireNamespace("utils", quietly=TRUE) ||
        .stop("failed to load package 'utils'")

    todo <- .install_repos(pkgs, lib = lib, repos = repos, ...)
    todo <- .install_github(
        todo, lib = lib, lib.loc = lib.loc, repos = repos, ...
    )

    if (length(todo))
        .warning(
            "packages not installed (unknown repository)",
            "\n    ", paste(.sQuote(todo), collapse = ", ")
        )

    setdiff(pkgs, todo)
}

.install_update <-
    function(repos, ask, lib.loc = NULL, instlib = NULL, checkBuilt = TRUE, ...)
{
    old_pkgs <- old.packages(lib.loc, repos, checkBuilt = checkBuilt)
    if (is.null(old_pkgs))
        return()

    old_pkgs <- .package_filter_masked(old_pkgs)
    old_pkgs <- .package_filter_unwriteable(old_pkgs, instlib)

    if (!nrow(old_pkgs))
        return()

    pkgs <- paste(old_pkgs[,"Package"], collapse="', '")
    .message("Update old packages: '%s'", pkgs)
    if (ask) {
        answer <- .getAnswer(
            "Update all / some / none? [a/s/n]: ",
            allowed = c("a", "A", "s", "S", "n", "N")
        )

        if (answer == "n")
            return()

        ask <- answer == "s"
    }

    update.packages(
        lib.loc, repos, oldPkgs = old_pkgs, ask = ask, instlib = instlib
    )
}

.install_updated_version <-
    function(update, repos, ...)
{
    valid <- valid()

    pkgs <- c(rownames(valid$too_new), rownames(valid$out_of_date))
    if (is.null(pkgs) || !update)
        return(pkgs)

    answer <- .getAnswer(
        sprintf(
            "reinstall %d packages for Bioconductor version %s? [y/n]: ",
            length(pkgs), version()
        ),
        allowed = c("y", "Y", "n", "N")
    )
    if (answer == "y")
        .install(pkgs, repos, ...)

    pkgs
}

#' @name install
#' @aliases BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS
#' @md
#'
#' @title Install or update Bioconductor, CRAN, and GitHub packages
#'
#' @description The `BiocManager::install()` function installs or
#'     updates _Bioconductor_ and CRAN packages in a _Bioconductor_
#'     release. Upgrading to a new _Bioconductor_ release may require
#'     additional steps; see \url{https://bioconductor.org/install}.
#'
#' @details
#'
#' Installation of _Bioconductor_ and CRAN packages use R's standard
#' functions for library management -- `install.packages()`,
#' `available.packages()`, `update.packages()`. Installation of GitHub
#' packages uses the `remotes::install_github()`.
#'
#' When installing CRAN or _Bioconductor_ packages, typical arguments
#' include: `lib.loc`, passed to `\link{old.packages}()` and used to
#' determine the library location of installed packages to be updated;
#' and `lib`, passed to `\link{install.packages}()` to determine the
#' library location where `pkgs` are to be installed.
#'
#' When installing GitHub packages, `...` is passed to the
#' \pkg{remotes} package functions `\link[remotes]{install_github}()`
#' and `remotes:::install()`. A typical use is to build vignettes, via
#' `dependencies=TRUE, build_vignettes=TRUE`.
#'
#' \env{BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS} is an environment
#' variable or global `options()` which, when set to `FALSE`, avoids
#' the R and _Bioconductor_ version checks that are done by querying
#' an online configuration file. Setting
#' \env{BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS} to `FALSE` can speed
#' package loading when internet access is slow or non-existent, but
#' may result in out-of-date information about the current release and
#' development versions of _Bioconductor_.
#'
#' @param pkgs `character()` vector of package names to install or
#'     update.  A missing value updates installed packages according
#'     to `update =` and `ask =`. Package names containing a '/' are
#'     treated as GitHub repositories and installed using
#'     `remotes::install_github()`.
#' @param ... Additional arguments used by `install.packages()`.
#' @param site_repository (Optional) `character(1)` vector
#'     representing an additional repository in which to look for
#'     packages to install. This repository will be prepended to the
#'     default repositories (which you can see with
#'     `BiocManager::\link{repositories}()`).
#' @param update `logical(1)`. When `FALSE`, `BiocManager::install()`
#'     does not attempt to update old packages. When `TRUE`, update
#'     old packages according to `ask`.
#' @param ask `logical(1)` indicating whether to prompt user before
#'     installed packages are updated.  If TRUE, user can choose
#'     whether to update all outdated packages without further
#'     prompting, to pick packages to update, or to cancel updating
#'     (in a non-interactive session, no packages will be updated
#'     unless `ask = FALSE`).
#' @param version `character(1)` _Bioconductor_ version to install,
#'     e.g., `version = "3.8"`. The special symbol `version = "devel"`
#'     installs the current 'development' version.
#'
#' @return `BiocManager::install()` returns the `pkgs` argument, invisibly.
#' @seealso
#'
#' `BiocManager::\link{repositories}()` returns the _Bioconductor_ and
#' CRAN repositories used by `install()`.
#'
#' `\link{install.packages}()` installs the packages themselves.
#'
#' `\link{update.packages}()` updates all installed packages.
#'
#' `\link{chooseBioCmirror}()` allows choice of a mirror from all
#' public _Bioconductor_ mirrors.
#'
#' `\link{chooseCRANmirror}()` allows choice of a mirror from all
#' public CRAN mirrors.
#'
#' @keywords environment
#' @examples
#'
#' \dontrun{
#' ## update previously installed packages
#' BiocManager::install()
#'
#' ## install Bioconductor packages, and prompt to update all
#' ## installed packages
#' BiocManager::install(c("GenomicRanges", "edgeR"))
#'
#' ## install a CRAN and Bioconductor packages:
#' BiocManager::install(c("survival", "SummarizedExperiment"))
#'
#' ## install a package from source:
#' BiocManager::install("IRanges", type="source")
#' }
#'
#' @export
install <-
    function(pkgs = character(), ..., site_repository = character(),
        update = TRUE, ask = TRUE, version = BiocManager::version())
{
    stopifnot(
        is.character(pkgs), !anyNA(pkgs),
        .install_validate_dots(...),
        length(site_repository) <= 1L,
        is.character(site_repository), !any(is.na(site_repository)),
        is.logical(update), length(update) == 1L, !is.na(update),
        is.logical(ask), length(ask) == 1L, !is.na(ask),
        length(version) == 1L || identical(version, .VERSION_SENTINEL)
    )
    version <- .version_validate(version)

    if (!"BiocVersion" %in% rownames(installed.packages())) {
        pkgs <- unique(c("BiocVersion", pkgs))
    }

    cmp <- .version_compare(version, version())
    if (cmp != 0L) {
        .install_ask_up_or_down_grade(version, cmp) ||
            .stop("Bioconductor version not changed")
        pkgs <- unique(c("BiocVersion", pkgs))
    }

    .message(
        "Bioconductor version %s (BiocManager %s), %s",
        version, packageVersion("BiocManager"),
        sub(" version", "", R.version.string)
    )

    repos <- repositories(site_repository, version = version)
    pkgs <- .install(pkgs, repos = repos, ...)
    if (update && cmp == 0L) {
        .install_update(repos, ask, ...)
    } else if (cmp != 0L) {
        .install_updated_version(update, repos, ...)
    }

    invisible(pkgs)
}
