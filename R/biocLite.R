## TODO: should probably print out a message about how to use mirrors,
## the way sourcing biocLite.R does now.

biocinstallRepos <-
    function()
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
    
    raw.repos <- getOption("repos")
    
    
    repos <- raw.repos[c("BioCsoft", "CRAN", "CRANextra", "BioCann",
            "BioCexp", "BioCextra")]

    ## This needs to be commented out a few months (3? 4?) after the
    ## development cycle for BioC 2.10 has started, when we are confident
    ## that no developper is still using an early R 2.15 with a
    ## tools:::.BioC_version_associated_with_R_version still pointing to
    ## BioC 2.9.
    if (getRversion() >= "2.15") {
        ## Add repos here as they become available.
        active_hutch_repos <- "BioCsoft"
        #active_hutch_repos <- c(active_hutch_repos, "BioCann")
        #active_hutch_repos <- c(active_hutch_repos, "BioCexp")
        active_hutch_repos <- c(active_hutch_repos, "BioCextra")

        ## No need to touch below.
        bioc_repos <- c(BioCsoft="bioc",
                        BioCann="data/annotation",
                        BioCexp="data/experiment",
                        BioCextra="extra")
        tmp_repos <- paste("http://bioconductor.org/packages/2.10",
                           bioc_repos[active_hutch_repos], sep="/")
        repos[names(tmp_repos)] <- tmp_repos
    }
    
    repos <- subset(repos, !is.na(repos))
    

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
    .message("BioC_mirror: '%s'",
             biocMirror)

    version <- getRversion()
    thisRVer <- sprintf("%d.%d", version$major, version$minor)
    .message("Using R version %s, BiocInstaller version %s.",
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
        
        if (ask==TRUE) {
            .message("The following packages are outdated: '%s'", pkgList)
            answer <- getAnswer("Would you like to update all/some/none? [a/s/n]: ",
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



getAnswer <- function(msg, allowed){
    if (interactive()) {
    repeat {
            cat(msg)
            answer <- readLines(n = 1)
            if (answer %in% allowed)
                break
        }
        tolower(answer)
    } else {
        return("n")
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
             ask=TRUE, ...)
{
    tryCatch(.checkSvnRevision(R.Version()[['svn rev']]),
             error=function(e) {
                 .stop("biocLite: %s", conditionMessage(e), call.=FALSE)
             }, warning=function(w) {
                 .warning("biocLite: %s", conditionMessage(w), call.=FALSE)
             })
    if (!suppressAutoUpdate && !bioconductorPackageIsCurrent()) {
        on.exit(updateBioconductorPackage(pkgs, ask=ask,
                                          suppressUpdates=suppressUpdates,
                                          ...))
    } else {
        biocLiteInstall(pkgs, ask=ask,
                        suppressUpdates=suppressUpdates, ...)
    }
}

## Package groups corresponding to books:

monograph_group <- function() 
{
    warning("'arrayMagic' package is no longer available")
    c("affycomp", "affydata", "affypdnn", "affyPLM",
    "ALL", "ALLMLL", "AmpAffyExample", "annaffy",
    "AnnBuilder", "annotate", "arrayQuality",
    "beta7", "Biobase", "bioDist", "Biostrings",
    "cMAP", "CoCiteStats", "convert",
    "e1071", "edd", "estrogen", "exactRankTests",
    "facsDorit", "factDesign", "gbm", "gcrma",
    "geneplotter", "golubEsets", "GOstats", "gpls",
    "graph", "hexbin", "hgu133a", "hgu133atagcdf",
    "hgu133acdf", "hgu133bcdf", "hgu2beta7", "hgu95av2",
    "hgu95av2cdf", "hgu95av2probe", "hopach",
    "hsahomology", "hu6800cdf", "hu6800probe",
    "humanLLMappings", "ipred", "KEGG", "KEGGSOAP",
    "kidpack", "limma", "locfit", "LogitBoost",
    "matchprobes", "mclust", "mlbench",
    "MLInterfaces", "multtest", "pamr", "prada",
    "PROcess", "ProData", "randomForest", "rat2302",
    "RbcBook1", "RBGL", "RColorBrewer", "RCurl",
    "Rgraphviz", "rrcov", "simpleaffy",
    "sma", "SpikeInSubset", "SSOAP", "statmod",
    "vsn", "XML", "xtable", "YEAST", "yeastExpData")
}


lite_group <- function()
{
    c(
        "affy", "affydata", "affyPLM", "annaffy", "annotate",
        "Biobase", "biomaRt", "Biostrings", "DynDoc", "gcrma",
        "genefilter", "geneplotter", "hgu95av2.db", "limma",
        "marray", "multtest",
        "vsn", "xtable", "affyQCReport"
    )
}

graph_group <- function()
{
    c("graph", "Rgraphviz", "RBGL")
}

RBioinf_group <- function()
{
    c(
        lite_group(), graph_group(),
        "RBioinf", "BiocCaseStudies",
        "XML", "RCurl",
        "biomaRt", "GEOquery",
        "KEGG", "KEGGSOAP",
        "hgu95av2", "hgu95av2probe", "hgu95av2cdf", "human.db0",
        "BSgenome.Hsapiens.UCSC.hg18"
    )
}

biocases_group <- function()
{
    c(
        lite_group(), graph_group(),
        "ALL",
        "apComplex",
        "bioDist",
        "BiocCaseStudies",
        "biocGraph",
        "biomaRt",
        "CCl4",
        "CLL",
        "Category",
        "class",
        "convert",
        "GO.db",
        "GOstats",
        "GSEABase",
        "hgu133a.db",
        "hgu95av2cdf",
        "hgu95av2probe",
        "hopach",
        "KEGG.db",
        "kohonen",
        "lattice",
        "latticeExtra",
        "MASS",
        "matchprobes",
        "MLInterfaces",
        "org.Hs.eg.db",
        "ppiStats",
        "randomForest",
        "RColorBrewer",
        "Rintact",
        "sma",
        "weaver",
        "yeastExpData"
    )
}
