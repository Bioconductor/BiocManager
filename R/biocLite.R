## TODO: should probably print out a message about how to use mirrors,
## the way sourcing biocLite.R does now.

biocinstallRepos <-
    function(siteRepos=character())
{
    ## siteRepos argument is public, but need biocVersion internally
    .biocinstallRepos(siteRepos=siteRepos, BIOC_VERSION)
}

.biocinstallRepos <-
    function(siteRepos=character(), biocVersion)
{
    old.opts <- options("repos")
    on.exit(options(old.opts))

    ## Starting at some point in R-2.14, Omegahat is included in
    ## the list of available repositories, on windows only, it seems.

    ## on mac and linux:
    
    ## 1: + CRAN
    ## 2: + CRAN (extras)
    ## 3: + BioC software
    ## 4: + BioC annotation
    ## 5: + BioC experiment
    ## 6: + BioC extra
    ## 7:   R-Forge
    ## 8:   rforge.net
    
    ## on windows:
    
    ## 1: + CRAN
    ## 2: + CRAN (extras)
    ## 3:   Omegahat 
    ## 4:   BioC software
    ## 5:   BioC annotation
    ## 6:   BioC experiment
    ## 7:   BioC extra
    ## 8:   R-Forge
    ## 9:   rforge.net
    
    ## So it's probably better not to rely on the numbers.
    
    setRepositories(ind=1:20) # in case more repos are added
    repos <- getOption("repos")

    biocMirror <- getOption("BioC_mirror", "http://bioconductor.org")
    biocPaths <- c(BioCsoft="bioc", BioCann="data/annotation",
                    BioCexp="data/experiment", BioCextra="extra")
    biocRepos <- paste(biocMirror, "packages", biocVersion,
                        biocPaths, sep="/")
    repos[names(biocPaths)] <- biocRepos

    keepRepos <- if (.Platform$OS.type %in% "windows") {
        c(names(biocPaths), "CRAN", "CRANextra")
    } else {
        c(names(biocPaths), "CRAN")
    }
    repos <- repos[keepRepos]

    ## This needs to be commented out a few months (3? 4?) after the
    ## next development cycle has started, when we are confident that
    ## no developper is still using an early R devel with a
    ## tools:::.BioC_version_associated_with_R_version still pointing
    ## to the release repository.
    if (!IS_USER)
    {
        ## comment repos here as they become available.
        inactive <- c(
                      ##   "BioCsoft"
                      ## , "BioCextra"
                      ## , "BioCann"
                      ## , "BioCexp"
                      )

        ## No need to touch below.
        tmpRepos <- paste(biocMirror, "packages", DOWNGRADE_VERSION,
                          biocPaths[inactive], sep="/")
        repos[inactive] <- tmpRepos
    }
    
    repos <- subset(repos, !is.na(repos))

    if ("@CRAN@" %in% repos)
        repos["CRAN"] <- "http://cran.fhcrc.org"
    if (includeMBNI &&
        (getOption("pkgType") %in% c("source", "win.binary")))
        repos[["MBNI"]] <- mbniUrl
    
    c(siteRepos=siteRepos, repos)
}

biocLiteInstall <-
    function(pkgs, repos, ask, suppressUpdates, siteRepos=character(),
             lib.loc=.libPaths(), lib=.libPaths()[1], ...)
{
    if (!missing(repos))
        .stop("'repos' argument to 'biocLite' not allowed")

    if (!(is.character(suppressUpdates) || is.logical(suppressUpdates)) ||
        (is.logical(suppressUpdates) && 1L != length(suppressUpdates)))
        .stop("'suppressUpdates' must be character() or logical(1)")

    type <- list(...)[["type"]]
    if (is.null(type))
        type <- getOption("pkgType")

    biocMirror <- getOption("BioC_mirror", "http://bioconductor.org")
    .message("BioC_mirror: %s", biocMirror)

    version <- getRversion()
    thisRVer <- sprintf("%d.%d", version$major, version$minor)
    .message("Using R version %s, BiocInstaller version %s.",
             thisRVer, packageVersion("BiocInstaller"))

    if (!suppressPackageStartupMessages(require("utils", quietly=TRUE)))
        .stop("failed to load package 'utils'")
    if (compareVersion(thisRVer, NEXT_R_DEVEL_VERSION) >= 0)
        .message("Temporarily using Bioconductor version %s",
                 BIOC_VERSION)

    repos <- biocinstallRepos(siteRepos)

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
        install.packages(pkgs=pkgs, lib=lib, repos=repos, ...)
    }

    ## early exit if suppressUpdates
    if (is.logical(suppressUpdates) && suppressUpdates)
        return(invisible(pkgs))
    pkgsToUpdate <- old.packages(repos=repos, lib.loc=lib.loc)
    if (is.null(pkgsToUpdate))
        return(invisible(pkgs))

    if (!is.logical(suppressUpdates)) {
        pkgsToUpdate <-
            filterPackagesToUpdate(suppressUpdates, pkgsToUpdate)
        suppressUpdates <- FALSE
    }

    oldPkgs <- getUpdatablePackages(pkgsToUpdate)
    if (nrow(oldPkgs)) {
        pkgList <- paste(oldPkgs[,"Package"], collapse="', '")
        if (ask==TRUE) {
            .message("Old packages: '%s'", pkgList)

            answer <-
                .getAnswer("Update all/some/none? [a/s/n]: ",
                           allowed = c("a", "A", "s", "S", "n", "N"))

            switch(answer,
                   a = update.packages(repos=repos, oldPkgs=oldPkgs, ask=FALSE),
                   s = update.packages(repos=repos, oldPkgs=oldPkgs, ask=TRUE),
                   n = invisible(pkgs))   
        } else {
            .message("Updating packages '%s'", pkgList)
            update.packages(repos=repos, oldPkgs=oldPkgs, ask=ask)
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

.checkSvnRevision <-
    function(svnRev)
{
    cmd <- "R.Version()[['svn rev']]"
    minSvnRev <- 55733
    if (is.character(svnRev) && 1L == length(svnRev) &&
        "unknown" == svnRev) {
        msg <- sprintf("%s == 'unknown' (expected > r%d)",
                       cmd, minSvnRev)
        .warning(msg)
    } else if (length(svnRev) != 1) {
        .warning("length(%s) != 1", cmd)
    } else {
        svnInt <-
            withCallingHandlers(as.integer(svnRev), warning=function(w) {
                msg <- sprintf("%s ('%s') cannot be coerced to integer",
                               cmd, svnRev)
                .warning(msg)
                invokeRestart("muffleWarning")
            })
        if (!is.na(svnInt) && minSvnRev >= svnInt) {
            msg <- sprintf("%s >= %d required, %d found",
                           cmd, minSvnRev, svnInt)
            .stop(msg)
        }
    }
    TRUE
}

biocLite <-
    function(pkgs=c("Biobase","IRanges","AnnotationDbi"),
             suppressUpdates=FALSE,
             suppressAutoUpdate=FALSE,
             siteRepos=character(), ask=TRUE, ...)
{
    tryCatch({
        .checkSvnRevision(R.Version()[['svn rev']])
    }, error=function(e) {
        .stop("biocLite: %s", conditionMessage(e), call.=FALSE)
    }, warning=function(w) {
        .warning("biocLite: %s", conditionMessage(w), call.=FALSE)
    })
    if (!suppressAutoUpdate && !bioconductorPackageIsCurrent()) {
        on.exit(updateBioconductorPackage(pkgs, ask=ask,
                                          suppressUpdates=suppressUpdates,
                                          siteRepos=siteRepos, ...))
    } else if ("BiocUpgrade" %in% pkgs) {
        .biocUpgrade()
    } else {
        biocLiteInstall(pkgs, ask=ask, siteRepos=siteRepos,
                        suppressUpdates=suppressUpdates, ...)
    }
}
