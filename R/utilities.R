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

.sQuote <- function(x)
    sprintf("'%s'", as.character(x))

.url_exists <-
    function(url)
{
    identical(length(.inet_readChar(url, 1L)), 1L)
}

.msg <-
    function(fmt, ..., width=getOption("width"))
    ## Use this helper to format all error / warning / message text
{
    txt <- strwrap(sprintf(fmt, ...), width=width, exdent=2)
    paste(txt, collapse="\n")
}

.message <-
    function(..., appendLF=TRUE)
{
    message(.msg(...), appendLF=appendLF)
    invisible(TRUE)
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
    invisible(TRUE)
}

isDevel <-
    function()
{
    version() == .version_bioc("devel")
}

isRelease <-
    function()
{
    version() == .version_bioc("release")
}
