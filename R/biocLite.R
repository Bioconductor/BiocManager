## TODO: should probably print out a message about how to use mirrors,
## the way sourcing biocLite.R does now.

biocinstallRepos <-
    function()
{
    old.opts <- options("repos")
    on.exit(options(old.opts))

    ## 1: + CRAN
    ## 2: + CRAN (extras)
    ## 3: + BioC software
    ## 4: + BioC annotation
    ## 5: + BioC experiment
    ## 6: + BioC extra
    ## 7:   R-Forge
    ## 8:   rforge.net

    setRepositories(ind=c(3, 1:2, 4:6))
    repos <- getOption("repos")

    if ("@CRAN@" %in% repos)
        repos["CRAN"] <- "http://cran.fhcrc.org"
    if (includeMBNI)
        repos[["MBNI"]] <- mbniUrl

    repos
}

biocLiteInstall <-
    function(pkgs, repos, ask, suppressUpdates, ...)
{
    if (!missing(repos))
        .stop("'repos' argument to 'biocLite' not allowed")

    if (!(is.character(suppressUpdates) || is.logical(suppressUpdates)) ||
        (is.logical(suppressUpdates) && 1L != length(suppressUpdates)))
        .stop("'suppressUpdates' must be character() or logical(1)")

    type <- list(...)[["type"]]
    if (is.null(type))
        type <- getOption("pkgType")

    biocMirror <- getOption("BioC_mirror", "http://www.bioconductor.org")
    .message("BioC_mirror = '%s', change using chooseBioCmirror().",
             biocMirror)

    version <- getRversion()
    thisRVer <- sprintf("%d.%d", version$major, version$minor)
    .message("Using R version %s, BiocInstaller package version %s.",
             thisRVer, packageVersion("BiocInstaller"))

    if (!suppressPackageStartupMessages(require("utils", quietly=TRUE)))
        .stop("failed to load package 'utils'")
    if (compareVersion(thisRVer, NEXT_R_DEVEL_VERSION) >= 0)
        .message("Temporarily using Bioconductor version %s",
                 BIOC_VERSION)

    repos <- biocinstallRepos()

    if (length(pkgs)) {
        if ((type %in% c("mac.binary", "mac.binary.leopard")) &&
            ("MBNI" %in% names(repos)))
        {
            url <- contrib.url(repos[["MBNI"]])
            mbniPkgs <- intersect(pkgs,
                                  row.names(available.packages(url)))
            if (length(mbniPkgs) > 0)
                .message("MBNI Brain Array packages '%s' are not
                         available as Mac binaries, use biocLite with
                         type='source'",
                         paste(mbniPkgs, collapse="' '"))
        }

        .message("Installing package(s) '%s'",
                 paste(pkgs, collapse="' '"))
        install.packages(pkgs=pkgs, repos=repos, ...)
    }

    ## early exit if suppressUpdates
    if (is.logical(suppressUpdates) && suppressUpdates)
        return(invisible(pkgs))
    pkgsToUpdate <- old.packages(repos=repos)
    if (is.null(pkgsToUpdate))
        return(invisible(pkgs))

    if (!is.logical(suppressUpdates)) {
        pkgsToUpdate <-
            filterPackagesToUpdate(suppressUpdates, pkgsToUpdate)
        suppressUpdates <- FALSE
    }
    oldPkgs <- getUpdatablePackages(pkgsToUpdate)
    if (nrow(oldPkgs)) {
        pkgList <- paste(oldPkgs[,"Package"], collapse="' '")
        .message("Updating packages '%s'", pkgList)
        update.packages(repos=repos, oldPkgs=oldPkgs, ask=ask)
    }

    invisible(pkgs)
}

biocLite <-
    function(pkgs=c("Biobase","IRanges","AnnotationDbi"),
             suppressUpdates=FALSE,
             suppressAutoUpdate=FALSE,
             ask=TRUE, ...)
{
    svnRev <- as.integer(R.Version()$`svn rev`)
    if (svnRev < 55733)
        .stop("R-2.14 r55733 or newer required.")
    
    if (!suppressAutoUpdate && !bioconductorPackageIsCurrent()) {
        on.exit(updateBioconductorPackage(pkgs, ask=ask,
                                          suppressUpdates=suppressUpdates,
                                          ...))
    } else {
        biocLiteInstall(pkgs, ask=ask,
                        suppressUpdates=suppressUpdates, ...)
    }
}
