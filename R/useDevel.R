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
