.biocUpgrade <-
    function()
{
    if (!IS_UPGRADEABLE) {
        .stop("Bioconductor version %s cannot be upgraded with
               R version %s", biocVersion(), getRversion())
    }
    if (IS_UPGRADEABLE && UPGRADE_IS_DEVEL)
        .stop("Bioconductor version %s can be upgraded, but only to 'devel';
               see ?useDevel. Use biocLite() without any arguments to update
               installed packages", biocVersion())

    txt <- sprintf("Upgrade all packages to Bioconductor version %s? [y/n]: ",
                   UPGRADE_VERSION)
    answer <- .getAnswer(txt, allowed = c("y", "Y", "n", "N"))
    if ("y" == answer)
        .update(UPGRADE_VERSION, TRUE)
}

isDevel <- function() !IS_USER

useDevel <-
    function(devel=TRUE)
{
    if (devel) {
        if (isDevel())
            .stop("'devel' version already in use")
        if (IS_END_OF_LIFE)
            .stop("'devel' version not available")
        if (!IS_UPGRADEABLE)
            .stop("'devel' version requires a more recent R")
        biocVers <- UPGRADE_VERSION
    } else {
        if (!isDevel())
            .stop("'devel' version not in use")
        if (!IS_DOWNGRADEABLE)
            .stop("'devel' version cannot be down-graded with this version of R")
        biocVers <- DOWNGRADE_VERSION
    }
    .update(biocVers, FALSE)
}

useRelease <-
    function(release=TRUE)
{
    if (release) {
        if (isDevel())
            ## prompt user
    }
}
.update <-
    function(biocVersion, biocLiteAfterUpdate = FALSE)
{
## TODO: write mechanism to upgrade and downgrade packages
}
