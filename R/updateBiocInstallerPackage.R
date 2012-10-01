.getContribUrl <-
    function(biocVersion)
{
    .contribUrl <-
        function(repos)
    {
        contribUrl <- contrib.url(repos)
        pkgs <- available.packages(contribUrl)
        if (!"BiocInstaller" %in% rownames(pkgs))
            .stop("'BiocInstaller' package not in repository %s", repos,
                  call.=FALSE)
        contribUrl
    }
    repos <- .biocinstallRepos(biocVersion=biocVersion)["BioCsoft"]
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
    contribUrl <- .getContribUrl(BIOC_VERSION)
    ap <- available.packages(contribUrl)
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
    .dbg("before, version is %s", packageVersion("BiocInstaller"))
    bootstrap <-
        function()
    {
        if ("package:BiocInstaller" %in% search())
            detach("package:BiocInstaller", unload=TRUE, force=TRUE)
        ## contribUrl will be in bootstrap's environment
        suppressWarnings(tryCatch({
            update.packages(contriburl=contribUrl, ask=FALSE,
                            checkBuilt=TRUE, oldPkgs="BiocInstaller")
        }, error=function(err) {
            assign("failed", TRUE, "biocBootstrapEnv")
            NULL
        }))
        library(BiocInstaller)
        BiocInstaller:::.updateBioconductorPackageFinish()
    }
    biocBootstrapEnv <- new.env()
    biocBootstrapEnv[["pkgs"]] <- pkgs[pkgs != "BiocInstaller"]
    biocBootstrapEnv[["ask"]] <- ask
    biocBootstrapEnv[["suppressUpdates"]] <- suppressUpdates
    biocBootstrapEnv[["contribUrl"]] <- .getContribUrl(BIOC_VERSION)
    biocBootstrapEnv[["dotArgs"]] <- list(...)
    
    .stepAside(biocBootstrapEnv, bootstrap)
}

.updateBioconductorPackageFinish <-
    function()
{
    args <- c(list(pkgs=get("pkgs", "biocBootstrapEnv"),
                   ask=get("ask", "biocBootstrapEnv"),
                   suppressUpdates=get("suppressUpdates", "biocBootstrapEnv")),
              get("dotArgs", "biocBootstrapEnv"))
    failed <- exists("failed", "biocBootstrapEnv")
    detach("biocBootstrapEnv")
    .dbg("after, version is %s", packageVersion("BiocInstaller"))
    vers <- packageVersion("BiocInstaller")
    if (!failed)
        .message("'BiocInstaller' updated to version %s", vers)
    else
        .warning("'BiocInstaller' update failed, using version %s",
                 vers, call.=FALSE)
    if ("BiocUpgrade" %in% args$pkgs) {
        .biocUpgrade()
    } else {
        do.call(biocLiteInstall, args)
    }
}
