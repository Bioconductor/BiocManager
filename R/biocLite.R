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
            loadNamespace("devtools", lib.loc)
        }, error=function(e) {
            if (!"devtools" %in% rownames(installed.packages(lib.loc))) {
                if (is.null(lib.loc))
                    lib.loc <- .libPaths()
                stop(conditionMessage(e),
                    "\n    package 'devtools' not installed in library path(s)",
                    "\n        ", paste(lib.loc, collapse="\n        "),
                    "\n    install with 'biocLite(\"devtools\")', and re-run your biocLite() command",
                    call.=FALSE)
            } else
                .stop("'loadNamespace(\"devtools\")' failed:\n    %s",
                      conditionMessage(e))
        })
        devtools::install_github(doing, ...)
    }
    setdiff(pkgs, doing)
}

.biocLiteInstall <-
    function(pkgs, repos, ask, suppressUpdates, siteRepos=character(),
        lib.loc=NULL, lib=.libPaths()[1], instlib=NULL, ...)
{
    if (!missing(repos))
        .stop("'repos' argument to 'biocLite' not allowed")

    if (!(is.character(suppressUpdates) || is.logical(suppressUpdates)) ||
        (is.logical(suppressUpdates) && 1L != length(suppressUpdates)))
        .stop("'suppressUpdates' must be character() or logical(1)")

    biocMirror <- getOption("BioC_mirror", "https://bioconductor.org")
    .message("BioC_mirror: %s", biocMirror)

    .message("Using Bioconductor %s (BiocInstaller %s), %s.",
        biocVersion(), packageVersion("BiocInstaller"),
        sub(" version", "", R.version.string))

    if (!suppressPackageStartupMessages(require("utils", quietly=TRUE)))
        .stop("failed to load package 'utils'")

    orepos <- options(repos=biocinstallRepos(siteRepos))
    on.exit(options(orepos))

    if (length(pkgs)) {
        todo <- .reposInstall(pkgs, lib=lib, ...)
        todo <- .githubInstall(todo, ...)
    }

    ## early exit if suppressUpdates
    if (is.logical(suppressUpdates) && suppressUpdates)
        return(invisible(pkgs))

    oldPkgs <- old.packages(lib.loc, checkBuilt=TRUE)
    if (is.null(oldPkgs))
        return(invisible(pkgs))

    oldPkgs <- .package_filter_suppress_updates(oldPkgs, suppressUpdates)
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

.getAnswer <- function(msg, allowed)
{
    if (interactive()) {
        repeat {
            cat(msg)
            answer <- readLines(n = 1)
            if (answer %in% allowed)
                break
        }
        tolower(answer)
    } else {
        "n"
    }
}

bioc <-
    function(pkgs = "Bioconductor", suppressUpdates = FALSE,
        siteRepos=character(), ask = TRUE, ...)
{
    if (isDevel())
        stop("To get the development version of Bioconductor, run 'useDevel()'")
    if (missing(pkgs))
        pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]

    .biocLiteInstall(pkgs, ask=ask, siteRepos=siteRepos,
        suppressUpdates=suppressUpdates, ...)
}

bioc_devel <-
    function(pkgs = "Bioconductor", suppressUpdates = FALSE,
        siteRepos = character(), ask = TRUE, ...)
{
    if (!isDevel())
        stop("To revert to Bioconductor release, run 'useRelease()'")
    if (missing(pkgs))
        pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]

    .biocLiteInstall(pkgs, ask=ask, siteRepos=siteRepos,
                     suppressUpdates=suppressUpdates, ...)
}
