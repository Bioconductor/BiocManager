.tooNewPkgs <-
    function(instPkgs, availPkgs)
{
    idx <- rownames(availPkgs) %in% rownames(instPkgs)
    vers <- availPkgs[idx, "Version"]
    idx <- package_version(vers) <
        package_version(instPkgs[names(vers), "Version"])
    tooNew <- names(vers)[idx]
    instPkgs[tooNew, c("Version", "LibPath"), drop=FALSE]
}



#' Validate installed package versions against current and appropriate versions.
#'
#'
#' Check that installed packages are consistent (neither out-of-date nor too
#' new) with the version of R and Bioconductor in use, using \code{install}
#' for validation.
#'
#'
#' This function compares the version of installed packages to the version of
#' packages associated with the version of R and Bioconductor appropriate for
#' the Bioconductor package currently in use.
#'
#' Packages are reported as \sQuote{out-of-date} if a more recent version is
#' available at the repositories specified by \code{installRepos()}.
#' Usually, \code{install()} is sufficient to update packages to their most
#' recent version.
#'
#' Packages are reported as \sQuote{too new} if the installed version is more
#' recent than the most recent available in the \code{installRepos()}
#' repositories. It is possible to down-grade by re-installing a too new
#' package \dQuote{PkgA} with \code{install("PkgA")}. It is important for the
#' user to understand how their installation became too new, and to avoid this
#' in the future.
#'
#' @param pkgs A character list of package names for checking, or a matrix as
#' returned by \code{\link{installed.packages}}.
#' @param lib.loc The library location(s) of packages to be validated; see
#' \code{\link{installed.packages}}.
#' @param priority check validity of all, \dQuote{base}, or
#' \dQuote{recommended} packages; see \code{\link{installed.packages}}.
#' @param type The type of available package (e.g., binary, source) to check
#' validity against; see \code{\link{available.packages}}.
#' @param filters Filter available packages to check validity against; see
#' \code{\link{available.packages}}.
#' @param silent Report how packages are invalid (\code{silent=FALSE}, default)
#' and abort execution, or return a logical(1) (\code{silent=TRUE}) indicating
#' the overall validity of installed packages.
#' @param \dots Additional arguments, passed to \code{\link{install}} when
#' \code{fix=TRUE}.
#' @param fix When \code{TRUE}, invoke \code{install} to reinstall (update or
#' downgrade, as appropriate) invalid packages.
#' @return \code{logical(1)} indicating overall validity of installed packages.
#' @author Martin Morgan \url{mtmorgan@@fhcrc.org}
#' @seealso \code{\link{install}} to update installed packages.
#' @keywords environment
#' @examples
#' try(valid())
#' @export valid
valid <-
    function(pkgs = installed.packages(lib.loc, priority=priority),
             lib.loc=NULL, priority="NA", type=getOption("pkgType"),
             filters=NULL, silent=FALSE, ..., fix=FALSE)
{
    if (!is.matrix(pkgs)) {
        if (is.character(pkgs))
            pkgs <- installed.packages(pkgs, lib.loc=lib.loc)
        else
            .stop("'pkgs' must be a character vector of package names,
                   or a matrix like that returned by 'installed.packages()'")
    }
    repos <- installRepos()
    contribUrl <- contrib.url(repos, type=type)

    availPkgs <- available.packages(contribUrl, type=type, filters=filters)
    oldPkgs <- old.packages(lib.loc, repos=installRepos(),
        instPkgs=pkgs, available=availPkgs, checkBuilt=TRUE,
        type=type)
    tooNewPkgs <- .tooNewPkgs(pkgs, availPkgs)

    libPaths <- unique(pkgs[,"LibPath"])

    valid <- (NROW(oldPkgs) == 0) && (NROW(tooNewPkgs) == 0)
    if (valid)
        return(valid)

    if (!silent) {
        result <- structure(
            list(oldPkgs=oldPkgs, tooNewPkgs = tooNewPkgs, libPaths = libPaths),
            class="biocValid"
        )
        print(result)
    }
    .unwritableDirectories(libPaths)
    if (fix) {
        pkgs <- c(rownames(oldPkgs), rownames(tooNewPkgs))
        install(pkgs, lib.loc=lib.loc, ...)
        .warning("updated or downgraded package(s) %s",
                 paste(.sQuote(pkgs), collapse=" "))
    } else {
        msg <- character()
        if (NROW(oldPkgs))
            msg <-
                c(msg, sprintf("%d package(s) out of date", NROW(oldPkgs)))
        if (NROW(tooNewPkgs))
            msg <-
                c(msg, sprintf("%d package(s) too new", NROW(tooNewPkgs)))
        .stop(paste(msg, collapse="; "))
    }

    invisible(valid)
}

.unwritableDirectories <- function(libPaths) {
    rootOwned <- file.access(libPaths, 2) == -1
    if (any(rootOwned))
        .warning("libraries cannot be written to %s",
                 paste(.sQuote(libPaths[rootOwned]), collapse=" "))
    libPaths[rootOwned]
}

print.biocValid <-
    function(x, ...)
{
    cat("\n* sessionInfo()\n\n")
    print(sessionInfo())
    cat("\n")
    cat("Library path directories:\n  ")
    cat(paste(x$libPaths, collapse = "\n  "), "\n")
    cat("\n")
    if (NROW(x$oldPkgs)) {
        cat("* Out-of-date packages\n")
        print(x$oldPkgs)
        cat("\nupdate with install()\n\n")
    }

    if (NROW(x$tooNewPkgs)) {
        cat("* Packages too new for Bioconductor version ",
            .sQuote(as.character(biocVersion())), "\n\n", sep="")
        print(x$tooNewPkgs)
        pkgs <- paste(.dQuote(rownames(x$tooNewPkgs)), collapse=", ")
        msg <- .msg(ifelse(NROW(x$tooNewPkgs) == 1L, "install(%s)",
                           "install(c(%s))"), pkgs)
        cat("\ndowngrade with ", msg, "\n\n", sep="")
    }
}
