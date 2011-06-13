.msg <-
    function(fmt, ..., width=0.9 * getOption("width"))
    ## Use this helper to format all error / warning / message text
{
    txt <- strwrap(sprintf(fmt, ...), width=width, indent=4)
    paste(sub("^ +", "", txt), collapse="\n")
}

# use as BiocInstaller:::.opts$get()  BiocInstaller:::.opts$set(TRUE)
.opts = local({
    debug <- FALSE
    list(get=function() debug, set=function(x) {
        old < debug
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
    function(..., call.=TRUE)
{
    stop(.msg(...), call.=call.)
}

.warning <-
    function(..., call.=TRUE, immediate.=FALSE)
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
