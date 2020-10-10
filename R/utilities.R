.is_CRAN_check <-
    function()
{
    !interactive() && ("CheckExEnv" %in% search())
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

.sQuote <- function(x)
    sprintf("'%s'", as.character(x))

.url_exists <-
    function(url)
{
    suppressWarnings(tryCatch({
        identical(nchar(.inet_readChar(url, 1L)), 1L)
    }, error = function(...) {
        FALSE
    }))
}

.msg <-
    function(
        fmt, ...,
        width=getOption("width"), indent = 0, exdent = 2, wrap. = TRUE
    )
    ## Use this helper to format all error / warning / message text
{
    txt <- sprintf(fmt, ...)
    if (wrap.) {
        txt <- strwrap(
            sprintf(fmt, ...), width=width, indent = indent, exdent=exdent
        )
        paste(txt, collapse="\n")
    } else {
        txt
    }
}

.message <-
    function(..., domain = NULL, appendLF=TRUE)
{
    message(.msg(...), domain = NULL, appendLF=appendLF)
    invisible(TRUE)
}

.packageStartupMessage <-
    function(..., domain = NULL, appendLF = TRUE)
{
    packageStartupMessage(.msg(...), domain = domain, appendLF = appendLF)
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

## testthat helper functions

.skip_if_misconfigured <-
    function()
{
    if (!"BiocVersion" %in% rownames(installed.packages()))
        return(NULL)

    R_version <- getRversion()
    bioc_version <- packageVersion("BiocVersion")[, 1:2]

    test_ver <- tryCatch({
        .version_validity(bioc_version)
    }, error = function(err) {
        conditionMessage(err)
    })

    if (!isTRUE(test_ver)) {
        msg <- sprintf("mis-configuration, R %s, Bioc %s, Reason %s",
            R_version, bioc_version, test_ver)
        testthat::skip(msg)
    }
}

.skip_if_BiocVersion_not_available <-
    function()
{
    if (!"BiocVersion" %in% rownames(installed.packages()))
        testthat::skip("BiocVersion not installed")
}
