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

# bootstrap() should take care of unloading BiocInstaller 
# and reloading it.

.stepAside <-
    function(biocBootstrapEnv, bootstrap) 
{
    environment(bootstrap) <- biocBootstrapEnv
    biocBootstrapEnv[["bootstrap"]] <- bootstrap
    attach(biocBootstrapEnv)
    on.exit(eval(bootstrap(), biocBootstrapEnv))
}
