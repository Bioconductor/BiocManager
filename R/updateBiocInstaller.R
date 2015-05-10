.getContribUrl <-
    function(biocVersion)
{
    .contribUrl <-
        function(repos)
    {
        contribUrl <- contrib.url(repos)
        pkgs <- available.packages(contribUrl)
        if (nrow(pkgs) == 0L)
            .stop("no packages in repository (no internet connection?)",
                  call.=FALSE)
        else if (!"BiocInstaller" %in% rownames(pkgs))
            .stop("'BiocInstaller' package not in repository",
                  call.=FALSE)
        contribUrl
    }
    repos <- biocinstallRepos(version=biocVersion)["BioCsoft"]
    suppressWarnings(tryCatch({
        .contribUrl(repos)
    }, error=function(err) {
        version <- getRversion()
        currentVersion <- sprintf("%d.%d", version$major, version$minor)
        lowerVersion <- .lowerRVersionString(version)
        oldRepos <- sub(currentVersion, lowerVersion, repos)
        if (oldRepos == repos)
            .stop("'%s' while trying %s", conditionMessage(err),
                  repos, call.=FALSE)
        .message("'%s' while trying %s, trying %s", conditionMessage(err),
                 repos, oldRepos)
        .contribUrl(oldRepos)
    }))
}

.isCurrentBiocInstaller <- 
    function()
{
    installedSentinel <- availableSentinel <- package_version("0.0.0")
    installedVersion <-
        tryCatch(packageVersion("BiocInstaller"),
                 error = function(err) installedSentinel)
    contribUrl <- .getContribUrl(biocVersion())
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

.updateBiocInstaller <-
    function(pkgs, ask, suppressUpdates, lib.loc=NULL, ...)
{
    .dbg("before, version is %s", packageVersion("BiocInstaller"))
    bootstrap <-
        function()
    {
        if ("package:BiocInstaller" %in% search())
            detach("package:BiocInstaller", unload=TRUE, force=TRUE)
        ## repos will be in bootstrap's environment
        suppressWarnings(tryCatch({
            update.packages(lib.loc, repos=repos, ask=FALSE,
                            checkBuilt=TRUE, oldPkgs="BiocInstaller")
        }, error=function(err) {
            assign("failed", TRUE, "biocBootstrapEnv")
            NULL
        }))
        library(BiocInstaller)
        BiocInstaller:::.updateBiocInstallerFinish()
    }
    biocBootstrapEnv <- new.env()
    biocBootstrapEnv[["pkgs"]] <- pkgs[pkgs != "BiocInstaller"]
    biocBootstrapEnv[["ask"]] <- ask
    biocBootstrapEnv[["suppressUpdates"]] <- suppressUpdates
    biocBootstrapEnv[["repos"]] <- biocinstallRepos(version=biocVersion())
    biocBootstrapEnv[["dotArgs"]] <- list(...)
    
    .stepAside(biocBootstrapEnv, bootstrap)
}

.updateBiocInstallerFinish <-
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
        do.call(.biocLiteInstall, args)
    }
}

## FIXME: DEFUNCT after BiocInstaller version 1.18.0
.updateBioconductorPackageFinish <- .updateBiocInstallerFinish
biocLiteInstall <- .biocLiteInstall
