R_VERSION_MAX <- IS_USER <- IS_END_OF_LIFE <- IS_UPGRADEABLE <-
    UPGRADE_IS_DEVEL <- IS_DOWNGRADEABLE <- UPGRADE_VERSION <-
        DOWNGRADE_VERSION <- NEXT_R_DEVEL_VERSION <- NULL

## The following values are updated with each Bioc release; see .onLoad
BIOC_VERSION <- package_version("3.7")       # Bioc version for this package
# R_VERSION_MAX <- package_version("3.3.0")  # Maximum version of R for
#                                            # this version of Bioconductor pkg
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

globalVariables("repos")           # used in 'bootstrap' functions

.onLoad <-
    function(libname, pkgname)
{
    fl <- system.file(package="Bioconductor", "scripts",
                      "Bioconductor.dcf")
    dcf <- read.dcf(fl)
    opt <- getOption(
        "BIOCINSTALLER_ONLINE_DCF",
        Sys.getenv("BIOCINSTALLER_ONLINE_DCF", TRUE)
    )
    if (opt) {
        tryCatch({
            con <- url("https://bioconductor.org/BiocInstaller.dcf")
            on.exit(close(con))
            dcf <- read.dcf(con)
        }, error=function(e) {})
    }

    bioconductorVars <- dcf[dcf[, "BIOC_VERSION"] == BIOC_VERSION,]

    R_VERSION_MAX <<- package_version(bioconductorVars[['R_VERSION_MAX']])
    IS_USER <<- as.logical(bioconductorVars[['IS_USER']])
    IS_END_OF_LIFE <<- as.logical(bioconductorVars[['IS_END_OF_LIFE']])
    IS_UPGRADEABLE <<- as.logical(bioconductorVars[['IS_UPGRADEABLE']])
    UPGRADE_IS_DEVEL <<- as.logical(bioconductorVars[['UPGRADE_IS_DEVEL']])
    IS_DOWNGRADEABLE <<- as.logical(bioconductorVars[['IS_DOWNGRADEABLE']])
    UPGRADE_VERSION <<- package_version(bioconductorVars[['UPGRADE_VERSION']])
    DOWNGRADE_VERSION <<- package_version(bioconductorVars[['DOWNGRADE_VERSION']])
    NEXT_R_DEVEL_VERSION <<- package_version(bioconductorVars[['NEXT_R_DEVEL_VERSION']])
}

.onAttach <-
    function(libname, pkgname)
{
    .message("Bioconductor version %s (package version %s), ?install for help",
             biocVersion(), packageVersion("Bioconductor"))
     Rversion <- getRversion()
     if (IS_END_OF_LIFE) {
         if (IS_UPGRADEABLE)
             .message("A newer version of Bioconductor is available for
                       this version of R, ?useRelease for help")
         else if (Rversion > R_VERSION_MAX)
             .warning("Bioconductor version %s is too old for R version %s;
                 see https://bioconductor.org/install/#troubleshoot-biocinstaller",
                 BIOC_VERSION, Rversion)
         else
             .message("A new version of Bioconductor is available after
                       installing the most recent version of R; see
                       http://bioconductor.org/install")
     } else if (Rversion > R_VERSION_MAX) {
         if (Rversion >= NEXT_R_DEVEL_VERSION)
             .message("Bioconductor does not yet support R version %s", Rversion)
         else
             .warning("Bioconductor version %s is too old for R version %s;
                 see https://bioconductor.org/install/#troubleshoot-biocinstaller",
                 BIOC_VERSION, Rversion)
     }

}
