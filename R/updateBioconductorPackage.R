getContribUrl <-
    function()
{
    .contribUrl <-
        function(repos)
    {
        contribUrl <- contrib.url(repos)
        pkgs <- available.packages(contribUrl)
        if (!"Bioconductor" %in% rownames(pkgs))
            .stop("'Bioconductor' package not in repository '%s'", repos,
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
        tryCatch(packageVersion("Bioconductor"),
                 error = function(err) installedSentinel)
    ap <- available.packages(getContribUrl())
    availableVersion <-
        if ("Bioconductor" %in% rownames(ap))
            package_version(ap["Bioconductor", "Version"])
        else
            availableSentinel
    if ((installedVersion == availableVersion) &&
        (installedVersion == installedSentinel))
        .stop("'Bioconductor' package not installed, and not available")
    availableVersion <= installedVersion
}

updateBioconductorPackage <-
    function(pkgs, ask, suppressUpdates, ...)
{
    .dbg("before, version is '%s'", packageVersion("Bioconductor"))
    bootstrap <-
        function()
    {
        if ("package:Bioconductor" %in% search())
            detach("package:Bioconductor", unload=TRUE, force=TRUE)
        ## contribUrl will be in bootstrap's environment
        install.packages("Bioconductor", repos=NULL, contriburl=contribUrl)
        library(Bioconductor)
        Bioconductor:::.updateBioconductorPackageFinish()
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
    .dbg("after, version is %s", packageVersion("Bioconductor"))
    .message("'Bioconductor' package updated to version %s.",
             packageVersion("Bioconductor"))
    do.call(biocLiteInstall, args)
}
