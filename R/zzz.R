## The following values are updated with each Bioc release; see .onLoad
BIOC_VERSION <- package_version("3.2")     # Bioc version for this package
R_VERSION_MAX <- package_version("3.3.0")  # Maximum version of R for
                                           # this version of BiocInstaller
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
IS_DOWNGRADEABLE <- TRUE                   # TRUE if an older version
                                           # (release or devel) of
                                           # Bioconductor is available
                                           # for this version of R
UPGRADE_VERSION <- package_version("3.2")  # Bioconductor version for
                                           # upgrade, if
                                           # IS_UPGRADEABLE == TRUE
DOWNGRADE_VERSION <- package_version("3.1") # Bioconductor version for
                                           # downgrade, if
                                           # IS_DOWNGRADEABLE == # TRUE

NEXT_R_DEVEL_VERSION <- "3.3.0" # next (not-yet-supported) version of R

## Change when the status of MBNI changes. 
## Make sure this change is propagated to users, even 
## if builds have stopped for a particular version of BioC.
## See biocLite.R:biocinstallRepos to include / exclude package types
includeMBNI <- FALSE
mbniUrl <- "http://brainarray.mbni.med.umich.edu/bioc"

.protocol <- local({
    PROTOCOL <- NULL
    function() {
        if (is.null(PROTOCOL)) {
            con <- file(fl <- tempfile(), "a")
            on.exit(close(con))
            sink(con, type="message")
            tryCatch({
                close(file("https://bioconductor.org"))
            }, error=identity)
            sink(type="message")
            flush(con)
            PROTOCOL <<- if (length(readLines(fl))) {
                "http:"
            } else "https:"
        }
        PROTOCOL
    }
})

globalVariables("repos")           # used in 'bootstrap' functions

.onAttach <-
    function(libname, pkgname) 
{
    Rversion <- getRversion()
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
    } else if (Rversion >= R_VERSION_MAX) {
        if (Rversion >= NEXT_R_DEVEL_VERSION)
            .message("Bioconductor does not yet support R version %s", Rversion)
        else
            .warning("BiocInstaller version %s is too old for R version %s;
                      remove.packages(\"BiocInstaller\"), re-start R, then
                      source(\"%s//bioconductor.org/biocLite.R\")",
                     BIOC_VERSION, Rversion, .protocol())
    }
}
