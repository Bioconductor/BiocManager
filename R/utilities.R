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

# use as BiocManager:::.opts$get()  BiocManager:::.opts$set(TRUE)
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
