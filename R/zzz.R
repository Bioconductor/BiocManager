## The following values are updated with each Bioc release; see .onLoad
BIOC_VERSION <- package_version("3.0")    # Bioc version for this package
R_VERSION <- package_version("3.1.0")      # R version for this package
IS_END_OF_LIFE <- FALSE                    # is BIOC_VERSION out-of-date?
NEXT_R_DEVEL_VERSION <- "3.2.0" # next (not-yet-supported) version of R
## DON'T FORGET TO FIX THE TEMPORARY HACK BELOW FOR BioC 3.1

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
    IS_UPGRADEABLE <<-
        (## Allowable within-R changes
         (IS_USER && ((BIOC_VERSION$minor %% 2L) == 0L)) ||
         ## between-R change
         (getRversion()$minor >= R_VERSION$minor + 1L))
    IS_DOWNGRADEABLE <<- !IS_USER && ((BIOC_VERSION$minor %% 2L) == 0L)
    ## Up- and downgrade versions, whether accessible or not
    vers <- if (BIOC_VERSION == "2.14") {
        "3.0"
    } else {
        sprintf("%s.%s", BIOC_VERSION$major, BIOC_VERSION$minor + 1L)
    }
    UPGRADE_VERSION <<- package_version(vers)
    vers <- if (BIOC_VERSION == "3.0") {
        "2.14"
    } else {
        sprintf("%s.%s", BIOC_VERSION$major, BIOC_VERSION$minor - 1L)
    }
    DOWNGRADE_VERSION <<- package_version(vers)
}

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
    if (R.version$major == "3" &&
      R.version$minor == "0.0" && 
      Sys.info()["sysname"] %in% c("Darwin", "Windows"))
    {
        .message("Upgrade to R-3.0.1 or some packages may not load properly.")
    }
}
