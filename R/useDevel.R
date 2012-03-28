useDevel <-
    function(devel=TRUE)
{
    if (devel && .isDevel())
        .stop("'devel' version already in use")
    if (!devel && !.isDevel())
        .stop("'release' version already in use")
    
    .dbg("before, version is '%s'", packageVersion("BiocInstaller"))
    bootstrap <-
        function()
    {
        if ("package:BiocInstaller" %in% search())
            detach("package:BiocInstaller", unload=TRUE, force=TRUE)
        ## contribUrl will be in bootstrap's environment
        suppressWarnings(tryCatch({
            install.packages("BiocInstaller", contriburl=contribUrl)
        }, error=function(err) {
            assign("failed", TRUE, "biocBootstrapEnv")
            NULL
        }))
        library(BiocInstaller)
        BiocInstaller:::.useDevelFinish()
    }
    biocBootstrapEnv <- new.env()
    biocBootstrapEnv[["contribUrl"]] <- .getContribUrl(devel)
    .stepAside(biocBootstrapEnv, bootstrap)
}

.useDevelFinish <-
    function()
{
    failed <- exists("failed", "biocBootstrapEnv")
    detach("biocBootstrapEnv")
    .dbg("after, version is %s", packageVersion("BiocInstaller"))
    vers <- packageVersion("BiocInstaller")
    if (!failed)
        .message("'BiocInstaller' changed to version %s", vers)
    else
        .warning("''useDevel' failed, using BiocInstaller version '%s'",
                 vers, call.=FALSE)
}
