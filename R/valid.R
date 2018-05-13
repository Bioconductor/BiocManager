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

.unwritableDirectories <- function(libPaths) {
    rootOwned <- file.access(libPaths, 2) == -1
    if (any(rootOwned))
        .warning("libraries cannot be written to %s",
                 paste(.sQuote(libPaths[rootOwned]), collapse=" "))
    libPaths[rootOwned]
}

.valid_fix <-
    function(pkgs, lib.loc, ...)
{
    install(pkgs, lib.loc, ...)
    .warning(
        "updated or downgraded package(s) '%s'",
        paste0(pkgs, collapse="', '")
    )

    pkgs
}

#'
#' Validate installed package versions against correct versions.
#'
#' Check that installed packages are consistent (neither out-of-date
#' nor too new) with the version of R and Bioconductor in use, using
#' \code{install} for validation.
#'
#' This function compares the version of installed packages to the
#' version of packages associated with the version of R and
#' Bioconductor appropriate for the BiocManager package currently in
#' use.
#'
#' Packages are reported as \sQuote{out-of-date} if a more recent
#' version is available at the repositories specified by
#' \code{repositories()}.  Usually, \code{install()} is sufficient to
#' update packages to their most recent version.
#'
#' Packages are reported as \sQuote{too new} if the installed version
#' is more recent than the most recent available in the
#' \code{repositories()} repositories. It is possible to down-grade by
#' re-installing a too new package \dQuote{PkgA} with
#' \code{install("PkgA")}. It is important for the user to understand
#' how their installation became too new, and to avoid this in the
#' future.
#'
#' @param pkgs A character() vector of package names for checking, or
#'     a matrix as returned by \code{\link{installed.packages}}.
#' @param lib.loc A character() vector of library location(s) of
#'     packages to be validated; see \code{\link{installed.packages}}.
#' @param priority character(1) Check validity of all, \dQuote{base},
#'     or \dQuote{recommended} packages; see
#'     \code{\link{installed.packages}}.
#' @param type character(1) The type of available package (e.g.,
#'     binary, source) to check validity against; see
#'     \code{\link{available.packages}}.
#' @param filters character(1) Filter available packages to check
#'     validity against; see \code{\link{available.packages}}.
#' @param silent logical(1) Report how packages are invalid
#'     (\code{silent=FALSE}, default) and abort execution, or return a
#'     logical(1) (\code{silent=TRUE}) indicating the overall validity
#'     of installed packages.
#' @param \dots Additional arguments, passed to \code{\link{install}}
#'     when \code{fix=TRUE}.
#' @return \code{biocValid} list object with elements `too_new` and
#'     `out_of_date` containing `data.frame`s with packages and their
#'     installed locations that are too new or out-of-date for the
#'     current version of Bioconductor.
#' @author Martin Morgan \email{martin.morgan@@roswellpark.org}
#' @seealso \code{\link{install}} to update installed packages.
#' @keywords environment
#' @examples
#' valid()
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
    repos <- repositories()
    contribUrl <- contrib.url(repos, type=type)

    availPkgs <- available.packages(contribUrl, type=type, filters=filters)
    out_of_date <- old.packages(lib.loc, repos=repositories(),
        instPkgs=pkgs, available=availPkgs, checkBuilt=TRUE,
        type=type)
    too_new <- .valid_pkgs_too_new(pkgs, availPkgs)

    result <- structure(
        list(out_of_date=out_of_date, too_new = too_new),
        class="biocValid"
    )

    if (NROW(out_of_date) + NROW(too_new) != 0L) {
        .warning(
            "%d packages out-of-date; %d packages too new",
            NROW(out_of_date), NROW(too_new)
        )
    }

    result
}

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

    if ((NROW(x$too_new) == 0L) && (NROW(x$out_of_date) == 0L)) {
        cat("\n\nInstallation valid\n")
        return()
    }

    fmt <-
        if (NROW(x$too_new) == 1L) {
            '  install("%s", update = TRUE, ask = FALSE)'
        } else {
            '  install(c(\n    "%s",\n    update = TRUE, ask = FALSE\n  ))'
        }
    pkgs <- paste(strwrap(
        paste(rownames(x$too_new), collapse='", "'),
        width = getOption("width") - 4L
    ), collapse="\n    ")
    cat(
        "\n\ncreate a valid installation with",
        "\n\n", sprintf(fmt, pkgs), "\n\n",
        sep = ""
    )
}
