repos <- biocinstallRepos()

test_biocinstallRepos_named_repositories <- function()
{

    allOS <- c("BioCsoft", "CRAN", "BioCann", "BioCexp", "BioCextra")
    nonLinux <- "CRANextra"

    checkTrue(all(allOS %in% names(repos)))
    if (Sys.info()[["sysname"]] %in% c("Windows", "Darwin"))
        checkTrue(nonLinux %in% names(repos))
}

test_biocinstallRepos_noNA_repositories <- function()
{
    checkTrue(!any(is.na(repos)))
}

test_biocinstallRepos_order <- function()
{
    checkIdentical("BioCsoft", names(repos)[[1]])
}
