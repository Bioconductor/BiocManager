.rRepos <- function(pkgs, invert = FALSE)
    grep("^(https?://.*|[^/]+)$", pkgs, invert = invert, value=TRUE)

.githubRepos <- function(pkgs) {
    pkgs <- .rRepos(pkgs, invert = TRUE)
    grep("^[^/]+/.+", pkgs, value=TRUE)
}

.reposInstall <-
    function(pkgs, lib, ...)
{
    doing <- .rRepos(pkgs)
    if (length(doing)) {
        pkgNames <- paste(sQuote(doing), collapse=", ")
        .message("Installing package(s) %s", pkgNames)
        install.packages(pkgs=doing, lib=lib, ...)
    }
    setdiff(pkgs, doing)
}

.githubInstall <-
    function(pkgs, ..., lib.loc=NULL)
{
    doing <- .githubRepos(pkgs)
    if (length(doing)) {
        pkgNames <- paste(sQuote(doing), collapse=", ")
        .message("Installing github package(s) %s", pkgNames)
        tryCatch({
            loadNamespace("devtools", lib.loc)
        }, error=function(e) {
            if (!"devtools" %in% rownames(installed.packages(lib.loc))) {
                if (is.null(lib.loc))
                    lib.loc <- .libPaths()
                stop(conditionMessage(e),
                    "\n    package 'devtools' not installed in library path(s)",
                    "\n        ", paste(lib.loc, collapse="\n        "),
                    "\n    install with 'install(\"devtools\")', and re-run your install() command",
                    call.=FALSE)
            } else
                .stop("'loadNamespace(\"devtools\")' failed:\n    %s",
                      conditionMessage(e))
        })
        devtools::install_github(doing, ...)
    }
    setdiff(pkgs, doing)
}

.biocInstall <-
    function(pkgs, repos, ask, suppressUpdates, siteRepos=character(),
        lib.loc=NULL, lib=.libPaths()[1], instlib=NULL, ...,
        version = BiocVersion::version())
{
    if (!missing(repos))
        .stop("'repos' argument to 'install' not allowed")

    if (!(is.character(suppressUpdates) || is.logical(suppressUpdates)) ||
        (is.logical(suppressUpdates) && 1L != length(suppressUpdates)))
        .stop("'suppressUpdates' must be character() or logical(1)")

    biocMirror <- getOption("BioC_mirror", "https://bioconductor.org")
    .message("BioC_mirror: %s", biocMirror)

    .message("Using Bioconductor %s (package version %s), %s.",
        version, packageVersion("Bioconductor"),
        sub(" version", "", R.version.string))

    if (!suppressPackageStartupMessages(
            requireNamespace("utils", quietly=TRUE)
        ))
        .stop("failed to load package 'utils'")

    orepos <- options(repos=installRepos(siteRepos))
    on.exit(options(orepos))

    if (length(pkgs)) {
        todo <- .reposInstall(pkgs, lib=lib, ...)
        todo <- .githubInstall(todo, ...)
    }

    ## early exit if suppressUpdates
    if (is.logical(suppressUpdates) && suppressUpdates)
        return(invisible(pkgs))

    oldPkgs <- old.packages(lib.loc, checkBuilt=TRUE)
    if (is.null(oldPkgs))
        return(invisible(pkgs))

    oldPkgs <- .package_filter_suppress_updates(oldPkgs, suppressUpdates)
    oldPkgs <- .package_filter_masked(oldPkgs)
    oldPkgs <- .package_filter_unwriteable(oldPkgs, instlib)

    if (nrow(oldPkgs)) {
        pkgList <- paste(oldPkgs[,"Package"], collapse="', '")
        if (ask) {
            .message("Old packages: '%s'", pkgList)

            answer <-
                .getAnswer("Update all/some/none? [a/s/n]: ",
                    allowed = c("a", "A", "s", "S", "n", "N"))

            switch(answer,
                a = update.packages(lib.loc, oldPkgs=oldPkgs, ask=FALSE,
                    instlib=instlib),
                s = update.packages(lib.loc, oldPkgs=oldPkgs, ask=TRUE,
                    instlib=instlib),
                n = invisible(pkgs))
        } else {
            .message("Updating packages '%s'", pkgList)
            update.packages(lib.loc, oldPkgs=oldPkgs, ask=ask, instlib=instlib)
        }
    }

    invisible(pkgs)
}

.getAnswer <- function(msg, allowed)
{
    if (interactive()) {
        repeat {
            cat(msg)
            answer <- readLines(n = 1)
            if (answer %in% allowed)
                break
        }
        tolower(answer)
    } else {
        "n"
    }
}
.dQuote <- function(x)
    sprintf('"%s"', as.character(x))

.sQuote <- function(x)
    sprintf("'%s'", as.character(x))

.msg <-
    function(fmt, ..., width=getOption("width"))
    ## Use this helper to format all error / warning / message text
{
    txt <- strwrap(sprintf(fmt, ...), width=width, exdent=2)
    paste(txt, collapse="\n")
}

# use as Bioconductor:::.opts$get()  Bioconductor:::.opts$set(TRUE)
.opts = local({
    debug <- FALSE
    list(get=function() debug, set=function(x) {
        old <- debug
        debug <<- x
        old
    })
})

.dbg <-
    function(...)
{
    if (.opts$get()) {
        .msg(...)
    }
}

.message <-
    function(..., appendLF=TRUE)
{
    message(.msg(...), appendLF=appendLF)
}

.stop <-
    function(..., call.=FALSE)
{
    stop(.msg(...), call.=call.)
}

.warning <-
    function(..., call.=FALSE, immediate.=FALSE)
{
    warning(.msg(...), call.=call., immediate.=immediate.)
}

.lowerRVersionString <-
    function(version=getRversion())
{
    if (0L == version$minor) {
        major <- version$major - 1L
        minor <- version$minor
    } else {
        major <- version$major
        minor <- version$minor - 1L
    }
    paste(major, minor, sep=".")
}

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
        else if (!"Bioconductor" %in% rownames(pkgs))
            .stop("'Bioconductor' package not in repository",
                  call.=FALSE)
        contribUrl
    }
    repos <- installRepos(version=biocVersion)["BioCsoft"]
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
