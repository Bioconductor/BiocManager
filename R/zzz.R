R_VERSION_MAX <- IS_USER <- IS_END_OF_LIFE <- IS_UPGRADEABLE <- UPGRADE_IS_DEVEL <-
  IS_DOWNGRADEABLE <- UPGRADE_VERSION <- DOWNGRADE_VERSION <-
  NEXT_R_DEVEL_VERSION <- NULL

## The following values are updated with each Bioc release; see .onLoad
BIOC_VERSION <- package_version("3.4")     # Bioc version for this package
# R_VERSION_MAX <- package_version("3.3.0")  # Maximum version of R for
#                                            # this version of BiocInstaller
# IS_USER <- FALSE                           # TRUE if this version of
#                                            # Bioconductor is the
#                                            # current release version
# IS_END_OF_LIFE <- FALSE                    # TRUE if this version of
#                                            # Bioconductor is no longer
#                                            # the release version
#
# IS_UPGRADEABLE <- FALSE                    # TRUE if a more recent
#                                            # version (release or
#                                            # devel) of Bioconductor is
#                                            # available for this
#                                            # version of R
# UPGRADE_IS_DEVEL <- TRUE                   # TRUE if UPGRADE_VERSION
#                                            # is for devel use only
# IS_DOWNGRADEABLE <- FALSE                  # TRUE if an older version
#                                            # (release or devel) of
#                                            # Bioconductor is available
#                                            # for this version of R
# UPGRADE_VERSION <- package_version("3.2")  # Bioconductor version for
#                                            # upgrade, if
#                                            # IS_UPGRADEABLE == TRUE
# DOWNGRADE_VERSION <- package_version("3.2") # Bioconductor version for
#                                            # downgrade, if
#                                            # IS_DOWNGRADEABLE == TRUE
#
# NEXT_R_DEVEL_VERSION <- "3.4.0" # next (not-yet-supported) version of R

## Change when the status of MBNI changes.
## Make sure this change is propagated to users, even
## if builds have stopped for a particular version of BioC.
## See biocLite.R:biocinstallRepos to include / exclude package types
includeMBNI <- FALSE
mbniUrl <- "http://brainarray.mbni.med.umich.edu/bioc"

.protocol <- local({
    PROTOCOL <- NULL
    function() {
        useHTTPS <- getOption("useHTTPS")
        if (!is.null(useHTTPS)) {
            PROTOCOL <<- if (useHTTPS) "https:" else "http:"
        } else if (is.null(PROTOCOL)) {
            useHTTPS <- TRUE
            withCallingHandlers({
                tryCatch({
                    fcon <- file("https://bioconductor.org/index.html")
                    on.exit(close(fcon), add=TRUE)
                    readLines(fcon, 1L)
                }, error=function(e) {
                    useHTTPS <<- FALSE
                })
            }, warning=function(e) {
                useHTTPS <<- FALSE
                invokeRestart("muffleWarning")
            })
            PROTOCOL <<- if (useHTTPS) "https:" else "http:"
        }
        PROTOCOL
    }
})

globalVariables("repos")           # used in 'bootstrap' functions

.onLoad <-
    function(libname, pkgname)
{
    fl <- system.file(package="BiocInstaller", "scripts",
                       "BiocInstaller.dcf")
    dcf <- read.dcf(fl)
    opt <- getOption("BIOCINSTALLER_ONLINE_DCF",
                     Sys.getenv("BIOCINSTALLER_ONLINE_DCF", TRUE))
    if (opt) {
        tryCatch({
            con <- url(paste0(.protocol(), "//bioconductor.org/BiocInstaller.dcf"))
            on.exit(close(con))
            dcf <- read.dcf(conn)
        }, error=function(e) {})
    }

    biocInstallerVars <- dcf[dcf[, "BIOC_VERSION"] == BIOC_VERSION,]

    R_VERSION_MAX <<- package_version(biocInstallerVars[['R_VERSION_MAX']])
    IS_USER <<- as.logical(biocInstallerVars[['IS_USER']])
    IS_END_OF_LIFE <<- as.logical(biocInstallerVars[['IS_END_OF_LIFE']])
    IS_UPGRADEABLE <<- as.logical(biocInstallerVars[['IS_UPGRADEABLE']])
    UPGRADE_IS_DEVEL <<- as.logical(biocInstallerVars[['UPGRADE_IS_DEVEL']])
    IS_DOWNGRADEABLE <<- as.logical(biocInstallerVars[['IS_DOWNGRADEABLE']])
    UPGRADE_VERSION <<- package_version(biocInstallerVars[['UPGRADE_VERSION']])
    DOWNGRADE_VERSION <<- package_version(biocInstallerVars[['DOWNGRADE_VERSION']])
    NEXT_R_DEVEL_VERSION <<- package_version(biocInstallerVars[['NEXT_R_DEVEL_VERSION']])
}

.onAttach <-
    function(libname, pkgname)
{
    .message("Bioconductor version %s (BiocInstaller %s), ?biocLite for help",
             biocVersion(), packageVersion("BiocInstaller"))
     Rversion <- getRversion()
     if (IS_END_OF_LIFE) {
         if (IS_UPGRADEABLE)
             .message("A newer version of Bioconductor is available for
                       this version of R, ?BiocUpgrade for help")
         else
             .message("A new version of Bioconductor is available after
                       installing the most recent version of R; see
                       http://bioconductor.org/install")
     } else if (Rversion > R_VERSION_MAX) {
         if (Rversion >= NEXT_R_DEVEL_VERSION)
             .message("Bioconductor does not yet support R version %s", Rversion)
         else
             .warning("BiocInstaller version %s is too old for R version %s;
                       remove.packages(\"BiocInstaller\"), re-start R, then
                       source(\"%s//bioconductor.org/biocLite.R\")",
                      BIOC_VERSION, Rversion, .protocol())
     }

}
