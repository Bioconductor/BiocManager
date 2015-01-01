## The following values are updated with each Bioc release; see .onLoad
BIOC_VERSION <- package_version("3.1")    # Bioc version for this package
R_VERSION <- package_version("3.2.0")      # R version for this package
IS_USER <- FALSE                           # TRUE if this version of
                                           # Bioconductor is the
                                           # current release version
IS_END_OF_LIFE <- FALSE                    # TRUE if this version of
                                           # Bioconductor is no longer
                                           # the release version

IS_UPGRADEABLE <- FALSE                    # TRUE if a more recent
                                           # version (release or
                                           # devel) of Bioconductor is
                                           # available for this
                                           # version of R
UPGRADE_IS_DEVEL <- TRUE                   # TRUE if UPGRADE_VERSION
                                           # is for devel use only
IS_DOWNGRADEABLE <- FALSE                  # TRUE if an older version
                                           # (release or devel) of
                                           # Bioconductor is available
                                           # for this version of R
UPGRADE_VERSION <- package_version("3.2")  # Bioconductor version for
                                           # upgrade, if
                                           # IS_UPGRADEABLE == TRUE
DOWNGRADE_VERSION <- package_version("3.0") # Bioconductor version for
                                           # downgrade, if
                                           # IS_DOWNGRADEABLE == # TRUE

NEXT_R_DEVEL_VERSION <- "3.3.0" # next (not-yet-supported) version of R

## Change when the status of MBNI changes. 
## Make sure this change is propagated to users, even 
## if builds have stopped for a particular version of BioC.
## See biocLite.R:biocinstallRepos to include / exclude package types
includeMBNI <- FALSE
mbniUrl <- "http://brainarray.mbni.med.umich.edu/bioc"

globalVariables("contribUrl")           # used in 'bootstrap' functions

.onAttach <-
    function(libname, pkgname) 
{
    .message("Bioconductor version %s (BiocInstaller %s), ?biocLite for help",
             biocVersion(), packageVersion("BiocInstaller"))
    if (IS_END_OF_LIFE) {
        if (IS_UPGRADEABLE)
            .message("A newer version of Bioconductor is available for
                      this version of R, ?BiocUpgrade for help")
        else
            .message("A new version of Bioconductor is available after
                      installing the most recent version of R; see
                      http://bioconductor.org/install")
    }
}
