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

# use as BiocInstaller:::.opts$get()  BiocInstaller:::.opts$set(TRUE)
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
