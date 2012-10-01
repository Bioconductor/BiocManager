## The following values are updated with each Bioc release; see .onLoad
BIOC_VERSION <- package_version("2.12")    # Bioc version of this package
IS_END_OF_LIFE <- FALSE                    # is BIOC_VERSION out-of-date?
NEXT_R_DEVEL_VERSION <- "2.17.0" # next (not-yet-supported) version of R

## Change when the status of MBNI changes. 
## Make sure this change is propagated to users, even 
## if builds have stopped for a particular version of BioC.
## See biocLite.R:.biocinstallRepos to include / exclude package types
includeMBNI <- FALSE
mbniUrl <- "http://brainarray.mbni.med.umich.edu/bioc"

globalVariables("contribUrl")           # used in 'bootstrap' functions

IS_USER <- IS_UPGRADEABLE <- IS_DOWNGRADEABLE <- UPGRADE_VERSION <-
    DOWNGRADE_VERSION <- NULL

.onLoad <-
    function(libname, pkgname)
{
    # USER or DEVEL?
    IS_USER <<- (packageVersion(pkgname)$minor %% 2L) == 0L
    ## Allowable within-R changes
    IS_UPGRADEABLE <<- IS_USER && ((BIOC_VERSION$minor %% 2L) == 0L)
    IS_DOWNGRADEABLE <<- !IS_USER && ((BIOC_VERSION$minor %% 2L) != 0L)
    ## Up- and downgrade versions, whether accessible or not
    vers <- sprintf("%s.%s", BIOC_VERSION$major, BIOC_VERSION$minor + 1L)
    UPGRADE_VERSION <<- package_version(vers)
    vers <- sprintf("%s.%s", BIOC_VERSION$major, BIOC_VERSION$minor - 1L)
    DOWNGRADE_VERSION <<- package_version(vers)
}

.onAttach <-
    function(libname, pkgname) 
{
    .message("BiocInstaller version %s, ?biocLite for help",
             packageVersion("BiocInstaller"))
    if (IS_END_OF_LIFE) {
        if (IS_UPGRADEABLE)
            .message("A newer version of Bioconductor is available for this version of R, ?BiocUpgrade for help")
        else
            .message("A newer version of Bioconductor is available after installing a new version of R, ?BiocUpgrade for help")
    }
}
