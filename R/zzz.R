## The following two values are updated with each bioC release
BIOC_VERSION <- "2.11"            # this version of Bioconductor
NEXT_R_DEVEL_VERSION <- "2.16.0" # next (not-yet-supported) version of R


## Change when the status of MBNI changes. 
## Make sure this change is propagated to users, even 
## if builds have stopped for a particular version of BioC.
includeMBNI <- FALSE
mbniUrl <- "http://brainarray.mbni.med.umich.edu/bioc"

.onAttach <-
    function(libname, pkgname) 
{
    packageStartupMessage(.msg("BiocInstaller version %s, ?biocLite for help",
             packageVersion("BiocInstaller")))
}
