.mbniFilter <-
    function(pkgs, ..., type=getOption("pkgType"))
{
    repos <- getOption("repos")
    ## MBNI packages not always available
    doing <- character()
    if ((type %in% c("mac.binary", "mac.binary.leopard")) &&
        ("MBNI" %in% names(repos)))
    {
        url <- contrib.url(repos[["MBNI"]])
        doing <- intersect(pkgs, row.names(available.packages(url)))
        if (length(doing)) {
            pkgNames <- paste(sQuote(doing), collapse=", ")
            .message("MBNI Brain Array packages %s are not available as
                      Mac binaries, use biocLite with type='source'",
                     pkgNames)
        }
    }
    setdiff(pkgs, doing)
}

.reposInstall <-
    function(pkgs, lib, ...)
{
    ## non-'github' packages
    doing <- grep("/", pkgs, invert=TRUE, value=TRUE)
    if (length(doing)) {
        pkgNames <- paste(sQuote(doing), collapse=", ")
        .message("Installing package(s) %s", pkgNames)
        install.packages(pkgs=doing, lib=lib, ...)
    }
    setdiff(pkgs, doing)
}

.githubInstall <-
    function(pkgs, ...)
{
    doing <- grep("/", pkgs, value=TRUE)
    if (length(doing)) {
        pkgNames <- paste(sQuote(doing), collapse=", ")
        if (!requireNamespace("devtools", quietly=TRUE))
            .stop("github installation of %s only available after
                   biocLite(\"devtools\")", pkgNames)
        .message("Installing github package(s) %s", pkgNames)
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

    biocMirror <- getOption("BioC_mirror",
                            sprintf("%s//bioconductor.org", .protocol()))
    .message("BioC_mirror: %s", biocMirror)

    .message("Using Bioconductor %s (BiocInstaller %s), %s.",
             biocVersion(), packageVersion("BiocInstaller"),
             sub(" version", "", R.version.string))

    if (!suppressPackageStartupMessages(require("utils", quietly=TRUE)))
        .stop("failed to load package 'utils'")

    orepos <- options(repos=biocinstallRepos(siteRepos))
    on.exit(options(orepos))

    if (length(pkgs)) {
        todo <- .mbniFilter(pkgs, ...)
        todo <- .reposInstall(todo, lib=lib, ...)
        todo <- .githubInstall(todo, ...)
    }

    ## early exit if suppressUpdates
    if (is.logical(suppressUpdates) && suppressUpdates)
        return(invisible(pkgs))
    pkgsToUpdate <- old.packages(lib.loc)
    if (is.null(pkgsToUpdate))
        return(invisible(pkgs))

    if (!is.logical(suppressUpdates)) {
        pkgsToUpdate <-
            filterPackagesToUpdate(suppressUpdates, pkgsToUpdate)
        suppressUpdates <- FALSE
    }

    oldPkgs <- pkgsToUpdate
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
            update.packages(lib.loc, oldPkgs=oldPkgs, ask=ask)
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

biocLite <-
    function(pkgs=c("Biobase","IRanges","AnnotationDbi"),
             suppressUpdates=FALSE,
             suppressAutoUpdate=FALSE,
             siteRepos=character(), ask=TRUE, ...)
{
    if (missing(pkgs))   # biocLite() update w/out installing defaults
        pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]
    if (!suppressAutoUpdate && !.isCurrentBiocInstaller()) {
        on.exit(.updateBiocInstaller(pkgs, ask=ask,
                                     suppressUpdates=suppressUpdates,
                                     siteRepos=siteRepos, ...))
    } else if ("BiocUpgrade" %in% pkgs) {
        .biocUpgrade()
    } else {
        .biocLiteInstall(pkgs, ask=ask, siteRepos=siteRepos,
                         suppressUpdates=suppressUpdates, ...)
    }
}
