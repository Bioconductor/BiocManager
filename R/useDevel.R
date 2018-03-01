isDevel <- function() !IS_USER

useDevel <- function()
{
    if (isDevel())
        .stop("'devel' version already in use")
    if (!IS_UPGRADEABLE)
        .stop("'devel' version requires a more recent R")

    .update(UPGRADE_VERSION, FALSE)
}

useRelease <- function()
{
    if (!isDevel())
        .stop("'release' version already in use")

    txt <- sprintf(
        "Downgrade all packages to Bioconductor version %s? [y/n]: ",
        DOWNGRADE_VERSION)
    answer <- .getAnswer(txt, allowed = c("y", "Y", "n", "N"))
    if ("y" == answer)
        .update(DOWNGRADE_VERSION, TRUE)
}

.update <-
    function(biocVersion, biocLiteAfterUpdate = FALSE)
{
## TODO: write mechanism to upgrade and downgrade packages
}

