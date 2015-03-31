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

useDevel <-
    function(devel=TRUE)
{
    if (devel) {
        if (!IS_USER)
            .stop("'devel' version already in use")
        if (IS_END_OF_LIFE)
            .stop("'devel' version not available")
        if (!IS_UPGRADEABLE)
            .stop("'devel' version requires a more recent R")
        biocVers <- UPGRADE_VERSION
    } else {
        if (IS_USER)
            .stop("'devel' version not in use")
        if (!IS_DOWNGRADEABLE)
            .stop("'devel' version cannot be down-graded with this version of R")
        biocVers <- DOWNGRADE_VERSION
    }
    .update(biocVers, FALSE)
}

.update <-
    function(biocVersion, biocLiteAfterUpdate = FALSE)
{
    .dbg("before, version is %s", packageVersion("BiocInstaller"))
    bootstrap <-
        function()
    {
        if (nchar(Sys.getenv("BIOCINSTALLER_TEST_REPOS")))
            repos = Sys.getenv("BIOCINSTALLER_TEST_REPOS")

        if ("package:BiocInstaller" %in% search())
            detach("package:BiocInstaller", unload=TRUE, force=TRUE)
        ## repos will be in bootstrap's environment
        suppressWarnings(tryCatch({
            install.packages("BiocInstaller", repos=repos)
        }, error=function(err) {
            assign("failed", TRUE, "biocBootstrapEnv")
            NULL
        }))
        library(BiocInstaller)
        BiocInstaller:::.updateFinish()
    }
    biocBootstrapEnv <- new.env()
    biocBootstrapEnv[["repos"]] <- biocinstallRepos(version=biocVersion)
    biocBootstrapEnv[["biocLiteAfterUpdate"]] <- biocLiteAfterUpdate
    .stepAside(biocBootstrapEnv, bootstrap)
}

.updateFinish <-
    function()
{
    failed <- exists("failed", "biocBootstrapEnv")
    biocLiteAfterUpdate <- get("biocLiteAfterUpdate", "biocBootstrapEnv")
    detach("biocBootstrapEnv")
    .dbg("after, version is %s", packageVersion("BiocInstaller"))
    vers <- packageVersion("BiocInstaller")
    if (!failed) {
        .message("'BiocInstaller' changed to version %s", vers)
        if (biocLiteAfterUpdate)
            biocLite(character(), ask=FALSE)
    } else
        .warning("update failed, using BiocInstaller version %s",
                 vers, call.=FALSE)
}
