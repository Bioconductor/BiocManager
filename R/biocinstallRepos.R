## TODO: should probably print out a message about how to use mirrors,
## the way sourcing biocLite.R does now.

biocinstallRepos <-
    function(siteRepos=character(), version=biocVersion())
{
    biocVersion <- as.package_version(version)

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

    biocMirror <- getOption("BioC_mirror",
                            sprintf("%s//bioconductor.org", .protocol()))
    biocPaths <- c(BioCsoft="bioc", BioCann="data/annotation",
                    BioCexp="data/experiment")#, BioCworkflows="workflows")
    biocRepos <- paste(biocMirror, "packages", biocVersion,
                        biocPaths, sep="/")
    repos[names(biocPaths)] <- biocRepos

    keepRepos <- c(names(biocPaths), "CRAN")
    repos <- repos[keepRepos]

    ## This needs to be commented out a few months (3? 4?) after the
    ## next development cycle has started, when we are confident that
    ## no developper is still using an early R devel with a
    ## tools:::.BioC_version_associated_with_R_version still pointing
    ## to the release repository.
    if (isDevel())
    {
        ## comment repos here as they become available.
        inactive <- c(
                      ##   "BioCsoft"
                      ## , "BioCann"
                      ## , "BioCexp"
                      )

        ## No need to touch below.
        tmpRepos <- paste(biocMirror, "packages", DOWNGRADE_VERSION,
                          biocPaths[inactive], sep="/")
        repos[inactive] <- tmpRepos
    }

    repos <- subset(repos, !is.na(repos))

    CRAN_repo <- repos[["CRAN"]]
    ## Microsoft R Open is shipped with getOption("repos")[["CRAN"]]
    ## pointing to a *snapshot* of CRAN (e.g.
    ## https://mran.microsoft.com/snapshot/2017-05-01), and not
    ## to a CRAN mirror that is current. For the current release and devel
    ## BioC versions, biocinstallRepos() needs to point to a CRAN mirror
    ## that is current so biocLite() and biocValid() behave the same for
    ## all BioC users, whether they use mainstream R or Microsoft R Open.
    ## However, since old versions of BioC are frozen, it would probably
    ## make sense to point to a *snapshot* of CRAN instead of a CRAN mirror
    ## that is current.
    snapshot_pattern <- "/snapshot/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
    if (CRAN_repo == "@CRAN@" || grepl(snapshot_pattern, CRAN_repo))
        repos[["CRAN"]] <- "https://cran.rstudio.com"

    c(siteRepos=siteRepos, repos)
}
