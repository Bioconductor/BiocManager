.valid_pkgs_too_new <-
    function(instPkgs, availPkgs)
{
    idx <- rownames(availPkgs) %in% rownames(instPkgs)
    vers <- availPkgs[idx, "Version"]
    idx <- package_version(vers) <
        package_version(instPkgs[names(vers), "Version"])
    too_new <- names(vers)[idx]
    instPkgs[too_new, c("Version", "LibPath"), drop=FALSE]
}

.valid <- function(pkgs = installed.packages(lib.loc, priority=priority),
    lib.loc=NULL, priority="NA", type=getOption("pkgType"),
    filters=NULL, ..., version = BiocManager::version()) {

    version <- .version_validate(version)
    repos <- repositories(version = version)
    contribUrl <- contrib.url(repos, type=type)

    availPkgs <- available.packages(contribUrl, type=type, filters=filters)

    out_of_date <- old.packages(lib.loc, repos=repos, instPkgs=pkgs,
        available=availPkgs, checkBuilt=TRUE, type=type)

    too_new <- .valid_pkgs_too_new(pkgs, availPkgs)

    result <- !nrow(too_new) && is.null(out_of_date)

    if (!result) {
        result <- structure(
            list(out_of_date = out_of_date, too_new = too_new),
            class="biocValid"
        )
    }
    result
}

#' Validate installed package versions against correct versions.
#'
#' Check that installed packages are consistent (neither out-of-date
#' nor too new) with the version of R and _Bioconductor_ in use.
#'
#' @details This function compares the version of installed packages
#'     to the version of packages associated with the version of _R_
#'     and _Bioconductor_ currently in use.
#'
#'     Packages are reported as 'out-of-date' if a more recent version
#'     is available at the repositories specified by
#'     `BiocManager::repositories()`.  Usually, `BiocManager::install()` is
#'     sufficient to update packages to their most recent version.
#'
#'     Packages are reported as 'too new' if the installed version is
#'     more recent than the most recent available in the
#'     `BiocManager::repositories()`. It is possible to down-grade by
#'     re-installing a too new package "PkgA" with
#'     `BiocManger::install("PkgA")`. It is important for the user to
#'     understand how their installation became too new, and to avoid
#'     this in the future.
#'
#' @param pkgs A character() vector of package names for checking, or
#'     a matrix as returned by `\link{installed.packages}`.
#' @param lib.loc A character() vector of library location(s) of
#'     packages to be validated; see `\link{installed.packages}()`.
#' @param priority character(1) Check validity of all, "base", or
#'     "recommended" packages; see `\link{installed.packages}()`.
#' @param type character(1) The type of available package (e.g.,
#'     binary, source) to check validity against; see
#'     `\link{available.packages}()`.
#' @param filters character(1) Filter available packages to check
#'     validity against; see `\link{available.packages}()`.
#' @param \dots Additional arguments, passed to
#'     `BiocManager::\link{install}()` when `fix=TRUE`.
#' @return `biocValid` list object with elements `too_new` and
#'     `out_of_date` containing `data.frame`s with packages and their
#'     installed locations that are too new or out-of-date for the
#'     current version of _Bioconductor_.
#' @author Martin Morgan \email{martin.morgan@@roswellpark.org}
#' @seealso `BiocManager::\link{install}()` to update installed
#'     packages.
#' @keywords environment
#' @examples
#' BiocManager::valid()
#' @md
#' @export valid
valid <-
    function(pkgs = installed.packages(lib.loc, priority=priority),
             lib.loc=NULL, priority="NA", type=getOption("pkgType"),
             filters=NULL, ...)
{
    if (!is.matrix(pkgs)) {
        if (is.character(pkgs)) {
            pkgs <- installed.packages(pkgs, lib.loc=lib.loc)
        } else {
            .stop(
                "'pkgs' must be a character vector of package names,
                 or a matrix like that returned by 'installed.packages()'"
            )
        }
    }
    result <- .valid(pkgs, lib.loc, priority, type, filters, ...)
    if (!isTRUE(result)) {
        out_of_date <- result$out_of_date
        too_new <- result$too_new
        if (NROW(out_of_date) + NROW(too_new) != 0L) {
            .warning(
                "%d packages out-of-date; %d packages too new",
                NROW(out_of_date), NROW(too_new)
            )
        }
    }
    result
}

#' @rdname valid
#' @param x A `biocValid` object returned by `BiocManager::valid()`.
#' @return `print()` is invoked for its side effect.
#' @export
print.biocValid <-
    function(x, ...)
{
    cat("\n* sessionInfo()\n\n")
    print(sessionInfo())

    cat(
        "\nBioconductor version '", as.character(version()), "'",
        "\n",
        "\n  * ", NROW(x$out_of_date), " packages out-of-date",
        "\n  * ", NROW(x$too_new), " packages too new",
        sep = ""
    )

    n <- NROW(x$too_new) + NROW(x$out_of_date)
    if (n == 0L) {
        cat("\n\nInstallation valid\n")
        return()
    }

    fmt <-'  BiocManager::install(%s, update = TRUE, ask = FALSE)'
    if (n == 1L) {
        fmt <- sprintf(fmt, '"%s"')
    } else {
        fmt <- sprintf(fmt, 'c(\n    "%s"\n  )')
    }

    pkgs0 <- sort(unique(c(rownames(x$too_new), rownames(x$out_of_date))))
    pkgs <- paste(strwrap(
        paste(pkgs0, collapse='", "'),
        width = getOption("width") - 4L
    ), collapse="\n    ")
    cat(
        "\n\ncreate a valid installation with",
        "\n\n", sprintf(fmt, pkgs), "\n\n",
        sep = ""
    )
    cat("more details: BiocManager::valid()$too_new, BiocManager::valid()$out_of_date\n\n")
}
