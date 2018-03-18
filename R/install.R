#' @name install
#'
#' @title Install or update Bioconductor and CRAN packages
#'
#'
#' @description \code{install} installs or updates Bioconductor and CRAN
#' packages in a Bioconductor release.  Upgrading to a new Bioconductor release
#' requires additional steps; see \url{https://bioconductor.org/install}.
#'
#' \env{BIOCINSTALLER_ONLINE_DCF} is an environment variable or global
#' \code{options()} which, when set to \code{FALSE}, uses configuration
#' information from a local archive rather than consulting the current online
#' version.
#'
#' Installation of Bioconductor and CRAN packages use R's standard functions
#' for library management -- \code{install.packages()},
#' \code{available.packages()}, \code{update.packages()}. Installation of
#' github packages uses the \code{install_github()} function from the
#' \code{devtools} package. For this reason it usually makes sense, when
#' complicated installation options are needed, to invoke \code{install()}
#' separately for Bioconductor / CRAN packages and for github packages.
#'
#' Setting \env{BIOCINSTALLER_ONLINE_DCF} to \code{FALSE} can speed package
#' loading when internet access is slow or non-existent, but may result in
#' out-of-date information about the current release and development versions
#' of Bioconductor.
#'
#' @aliases install BIOCINSTALLER_ONLINE_DCF
#' @param pkgs \code{character()} of package names to install or
#'     update.  A missing value and \code{update=FALSE} updates
#'     installed packages, perhaps also installing \code{Biobase},
#'     \code{IRanges}, and \code{AnnotationDbi} if they are not
#'     already installed. Package names containing a \sQuote{/} are
#'     treated as github repositories and installed using the
#'     \code{install_github()} function of the \code{devtools}
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
#' When installing CRAN or Bioconductor packages, typical arguments include:
#' \code{lib.loc}, passed to \code{\link{old.packages}} and used to determine
#' the library location of installed packages to be updated; and \code{lib},
#' passed to \code{\link{install.packages}} to determine the library location
#' where \code{pkgs} are to be installed.
#'
#' When installing github packages, \code{...} is passed to the \pkg{devtools}
#' package functions \code{\link[devtools]{install_github}} and
#' \code{\link[devtools]{install}}. A typical use is to build vignettes, via
#' \code{dependencies=TRUE, build_vignettes=TRUE}.
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
#' options(BIOCINSTALLER_ONLINE_DCF = FALSE)
#'
#' @export
install <-
    function(pkgs = character(), ..., site_repository = character(),
        update = TRUE, ask = TRUE, version = Bioconductor::version())
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
