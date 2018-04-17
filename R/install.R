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
            paste(pkgs[!status, "Package"], collapse=", "))

    pkgs[status,, drop=FALSE]
}


.rRepos <- function(pkgs, invert = FALSE)
    grep("^(https?://.*|[^/]+)$", pkgs, invert = invert, value=TRUE)

.githubRepos <- function(pkgs) {
    pkgs <- .rRepos(pkgs, invert = TRUE)
    grep("^[^/]+/.+", pkgs, value=TRUE)
}

.reposInstall <-
    function(pkgs, lib, ...)
{
    doing <- .rRepos(pkgs)
    if (length(doing)) {
        pkgNames <- paste(sQuote(doing), collapse=", ")
        .message("Installing package(s) %s", pkgNames)
        install.packages(pkgs=doing, lib=lib, ...)
    }
    setdiff(pkgs, doing)
}

.githubInstall <-
    function(pkgs, ..., lib.loc=NULL)
{
    doing <- .githubRepos(pkgs)
    if (length(doing)) {
        pkgNames <- paste(sQuote(doing), collapse=", ")
        .message("Installing github package(s) %s", pkgNames)
        tryCatch({
            loadNamespace("remotes", lib.loc)
        }, error=function(e) {
            if (!"remotes" %in% rownames(installed.packages(lib.loc))) {
                if (is.null(lib.loc))
                    lib.loc <- .libPaths()
                stop(conditionMessage(e),
                    "\n    package 'remotes' not installed in library path(s)",
                    "\n        ", paste(lib.loc, collapse="\n        "),
                    "\n    install with 'install(\"remotes\")', and re-run your install() command",
                    call.=FALSE)
            } else
                .stop("'loadNamespace(\"remotes\")' failed:\n    %s",
                      conditionMessage(e))
        })
        remotes::install_github(doing, ...)
    }
    setdiff(pkgs, doing)
}

.biocInstall <-
    function(pkgs, repos, ask, update, site_repository = character(),
        lib.loc=NULL, lib=.libPaths()[1], instlib=NULL, ...,
        version = BiocManager::version())
{
    if (!missing(repos))
        .stop("'repos' argument to 'install' not allowed")

    if (!(is.character(update) || is.logical(update)) ||
        (is.logical(update) && 1L != length(update)))
        .stop("'update' must be character() or logical(1)")

    biocMirror <- getOption("BioC_mirror", "https://bioconductor.org")
    .message("BioC_mirror: %s", biocMirror)

    .message("Using Bioconductor %s (BiocManager %s), %s.",
        version, packageVersion("BiocManager"),
        sub(" version", "", R.version.string))

    if (!suppressPackageStartupMessages(
            requireNamespace("utils", quietly=TRUE)
        ))
        .stop("failed to load package 'utils'")

    orepos <- options(repos=repositories(site_repository))
    on.exit(options(orepos))

    if (length(pkgs)) {
        todo <- .reposInstall(pkgs, lib=lib, ...)
        todo <- .githubInstall(todo, ...)
    }

    ## early exit if update
    if (isFALSE(update))
        return(invisible(pkgs))

    oldPkgs <- old.packages(lib.loc, checkBuilt=TRUE)
    if (is.null(oldPkgs))
        return(invisible(pkgs))

    oldPkgs <- .package_filter_masked(oldPkgs)
    oldPkgs <- .package_filter_unwriteable(oldPkgs, instlib)

    if (nrow(oldPkgs)) {
        pkgList <- paste(oldPkgs[,"Package"], collapse="', '")
        if (ask) {
            .message("Old packages: '%s'", pkgList)

            answer <-
                .getAnswer("Update all/some/none? [a/s/n]: ",
                    allowed = c("a", "A", "s", "S", "n", "N"))

            switch(answer,
                a = update.packages(lib.loc, oldPkgs=oldPkgs, ask=FALSE,
                    instlib=instlib),
                s = update.packages(lib.loc, oldPkgs=oldPkgs, ask=TRUE,
                    instlib=instlib),
                n = invisible(pkgs))
        } else {
            .message("Updating packages '%s'", pkgList)
            update.packages(lib.loc, oldPkgs=oldPkgs, ask=ask, instlib=instlib)
        }
    }

    invisible(pkgs)
}

#'
#' @name install
#' @aliases BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS
#'
#' @title Install or update Bioconductor and CRAN packages
#'
#' @description The \code{install} function installs or updates Bioconductor
#' and CRAN packages in a Bioconductor release. Upgrading to a new Bioconductor
#' release requires additional steps; see \url{https://bioconductor.org/install}.
#'
#' @details
#' Installation of Bioconductor and CRAN packages use R's standard functions
#' for library management -- \code{install.packages()},
#' \code{available.packages()}, \code{update.packages()}. Installation of
#' github packages uses the \code{install_github()} function from the
#' \code{remotes} package. For this reason it usually makes sense, when
#' complicated installation options are needed, to invoke \code{install()}
#' separately for Bioconductor / CRAN packages and for github packages.
#'
#' When installing CRAN or Bioconductor packages, typical arguments include:
#' \code{lib.loc}, passed to \code{\link{old.packages}} and used to determine
#' the library location of installed packages to be updated; and \code{lib},
#' passed to \code{\link{install.packages}} to determine the library location
#' where \code{pkgs} are to be installed.
#'
#' When installing github packages, \code{...} is passed to the \pkg{remotes}
#' package functions \code{\link[remotes]{install_github}} and
#' \code{remotes:::install}. A typical use is to build vignettes, via
#' \code{dependencies=TRUE, build_vignettes=TRUE}.
#'
#' \env{BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS} is an environment
#' variable or global \code{options()} which, when set to \code{FALSE}, avoids
#' the R and Bioconductor version checks that are done by querying an online
#' configuration file.
#'
#' Setting \env{BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS} to \code{FALSE} can
#' speed package loading when internet access is slow or non-existent, but may
#' result in out-of-date information about the current release and development
#' versions of Bioconductor.
#'
#' @param pkgs \code{character()} of package names to install or
#'     update.  A missing value and \code{update=FALSE} updates
#'     installed packages. Package names containing a \sQuote{/} are
#'     treated as github repositories and installed using the
#'     \code{install_github()} function of the \code{remotes}
#'     package.
#' @param ... Additional arguments.
#' @param update \code{logical(1)}. When \code{FALSE}, install asks
#'     the user whether old packages should be update.  When
#'     \code{TRUE}, the user is not prompted to update old packages.
#' @param site_repository \code{character()} representing an
#'     additional repository in which to look for packages to
#'     install. This repository will be prepended to the default
#'     repositories (which you can see with
#'     \code{\link{repositories}}).
#' @param ask \code{logical(1)} indicating whether to prompt user
#'     before installed packages are updated, or the character string
#'     'graphics', which brings up a widget for choosing which
#'     packages to update.  If TRUE, user can choose whether to update
#'     all outdated packages without further prompting, to pick and
#'     choose packages to update, or to cancel updating (in a
#'     non-interactive session, no packages will be
#'     updated). Otherwise, the value is passed to
#'     \code{\link{update.packages}}.
#' @param version `character(1)` Bioconductor version to install,
#'     e.g., `version = "3.7"`.
#'
#' @return \code{install()} returns the \code{pkgs} argument, invisibly.
#' @seealso
#'
#' \code{\link{repositories}} returns the Bioconductor and CRAN
#' repositories used by \code{install}.
#'
#' \code{\link{install.packages}} installs the packages themselves.
#'
#' \code{\link{update.packages}} updates all installed packages.
#'
#' \code{\link{chooseBioCmirror}} lets you choose from a list of all public
#' Bioconductor mirror URLs.
#'
#' \code{\link{chooseCRANmirror}} lets you choose from a list of all public
#' CRAN mirror URLs.
#'
#' @keywords environment
#' @examples
#'
#' \dontrun{
#' ## Change default Bioconductor and CRAN mirrors
#' chooseCRANmirror()
#'
#' ## installs default packages (if not already installed) and updates
#' ## previously installed packages
#' install()
#'
#' ## Now install a CRAN package:
#' install("survival")
#'
#' ## install a Bioconductor package, but don't update all installed
#' ## packages as well:
#' install("GenomicRanges", update=TRUE)
#'
#' ## Install default packages, but do not update any package whose name
#' ## starts with "org." or "BSgenome."
#' install(update=c("^org\.", "^BSgenome\."))
#'
#' ## install a package from source:
#' install("IRanges", type="source")
#'
#' }
#' ## Show the Bioconductor and CRAN repositories that will be used to
#' ## install/update packages.
#' repositories()
#'
#' ## Use local archive rather than current online configuration
#' ## information. Set this prior to loading the BiocInstaller package.
#' options(BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS = FALSE)
#'
#' @export
install <-
    function(pkgs = character(), ..., site_repository = character(),
        update = TRUE, ask = TRUE, version = BiocManager::version())
{
    stopifnot(
        is.character(pkgs), !anyNA(pkgs),
        length(site_repository) <= 1L,
        is.character(site_repository), !any(is.na(site_repository)),
        is.logical(update), length(update) == 1L, !is.na(update),
        is.logical(ask), length(ask) == 1L, !is.na(ask),
        length(version) == 1L
    )
    version <- package_version(version)
    if (version[, 1:2] != version)
        .stop("'version' %s must have two components, e.g., '3.7'", version)

    if (version == .BIOCVERSION_SENTINEL) {
        version <- .version_choose_best()
        pkgs <- unique(c("BiocVersion", pkgs))
    } else {
        cmp <- .compare_version(version, version())
        if (cmp != 0) {
            version <- .version_validate(version)
            ## helper to check if versions match or prompt for switch
            action <- if (cmp < 0) "Downgrade" else "Upgrade"
            txt <- sprintf(
                "%s Bioconductor to version %s? [y/n]: ", action, version
            )
            answer <- .getAnswer(txt, allowed = c("y", "Y", "n", "N"))
            if ("n" == answer)
                .stop("Bioconductor version not changed")
            pkgs <- unique(c("BiocVersion", pkgs))
        }
    }

    .biocInstall(pkgs, ask=ask, site_repository=site_repository,
        update=update, ..., version = version)
}
