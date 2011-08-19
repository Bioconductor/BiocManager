getContribUrl <-
    function()
{
    .contribUrl <-
        function(repos)
    {
        contribUrl <- contrib.url(repos)
        pkgs <- available.packages(contribUrl)
        if (!"BiocInstaller" %in% rownames(pkgs))
            .stop("'BiocInstaller' package not in repository '%s'", repos,
                  call.=FALSE)
        contribUrl
    }
    repos <- biocinstallRepos()["BioCsoft"]
    suppressWarnings(tryCatch({
        .contribUrl(repos)
    }, error=function(err) {
        version <- getRversion()
        currentVersion <- sprintf("%d.%d", version$major, version$minor)
        lowerVersion <- .lowerRVersionString(version)
        oldRepos <- sub(currentVersion, lowerVersion, repos)
        .message("%s, using '%s'", conditionMessage(err), oldRepos)
        .contribUrl(oldRepos)
    }))
}

bioconductorPackageIsCurrent <-
    function()
{
    installedSentinel <- availableSentinel <- package_version("0.0.0")
    installedVersion <-
        tryCatch(packageVersion("BiocInstaller"),
                 error = function(err) installedSentinel)
    ap <- available.packages(getContribUrl())
    availableVersion <-
        if ("BiocInstaller" %in% rownames(ap))
            package_version(ap["BiocInstaller", "Version"])
        else
            availableSentinel
    if ((installedVersion == availableVersion) &&
        (installedVersion == installedSentinel))
        .stop("'BiocInstaller' package not installed, and not available")
    availableVersion <= installedVersion
}

updateBioconductorPackage <-
    function(pkgs, ask, suppressUpdates, ...)
{
    .dbg("before, version is '%s'", packageVersion("BiocInstaller"))
    bootstrap <-
        function()
    {
        if ("package:BiocInstaller" %in% search())
            detach("package:BiocInstaller", unload=TRUE, force=TRUE)
        ## contribUrl will be in bootstrap's environment
        install.packages("BiocInstaller", repos=NULL, contriburl=contribUrl)
        library(BiocInstaller)
        BiocInstaller:::.updateBioconductorPackageFinish()
    }
    biocBootstrapEnv <- new.env()
    environment(bootstrap) <- biocBootstrapEnv
    biocBootstrapEnv[["bootstrap"]] <- bootstrap
    biocBootstrapEnv[["pkgs"]] <- pkgs
    biocBootstrapEnv[["ask"]] <- ask
    biocBootstrapEnv[["suppressUpdates"]] <- suppressUpdates
    biocBootstrapEnv[["contribUrl"]] <- getContribUrl()
    biocBootstrapEnv[["dotArgs"]] <- list(...)
    attach(biocBootstrapEnv)
    on.exit(eval(bootstrap(), biocBootstrapEnv))
}

.updateBioconductorPackageFinish <-
    function()
{
    args <- c(list(pkgs=get("pkgs", "biocBootstrapEnv"),
                   ask=get("ask", "biocBootstrapEnv"),
                   suppressUpdates=get("suppressUpdates", "biocBootstrapEnv")),
              get("dotArgs", "biocBootstrapEnv"))
    detach("biocBootstrapEnv")
    .dbg("after, version is %s", packageVersion("BiocInstaller"))
    .message("'BiocInstaller' package updated to version %s.",
             packageVersion("BiocInstaller"))
    do.call(biocLiteInstall, args)
}
