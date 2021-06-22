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
        txt,
        width=getOption("width"), indent = 0, exdent = 2,
        wrap. = TRUE
    )
    ## Use this helper to format all error / warning / message text
{
    if (wrap.) {
        txt <- strwrap(txt, width=width, indent = indent, exdent=exdent)
        paste(txt, collapse="\n")
    } else {
        txt
    }
}

.message <-
    function(txt, ..., call. = FALSE, domain = NULL, appendLF=TRUE)
{
    ## call. = FALSE provides compatibility with .stop(), but is ignored
    message(.msg(txt, ...), domain = NULL, appendLF=appendLF)
    invisible(TRUE)
}

.packageStartupMessage <-
    function(txt, ..., domain = NULL, appendLF = TRUE)
{
    packageStartupMessage(.msg(txt, ...), domain = domain, appendLF = appendLF)
    invisible(TRUE)
}

.stop <-
    function(txt, ..., call.=FALSE)
{
    stop(.msg(txt, ...), call.=call.)
}

.warning <-
    function(txt, ..., call.=FALSE, immediate.=FALSE)
{
    warning(.msg(txt, ...), call.=call., immediate.=immediate.)
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
    R_version <- getRversion()
    bioc_version <- version()

    test_ver <- tryCatch({
        .version_validity(bioc_version)
    }, error = function(err) {
        conditionMessage(err)
    })

    if (!isTRUE(test_ver)) {
        txt <- gettextf(
            "mis-configuration, R %s, Bioconductor %s", R_version, bioc_version,
            domain = "R-BiocManager"
        )
        testthat::skip(txt)
    }
}

.skip_if_BiocVersion_not_available <-
    function()
{
    if (!"BiocVersion" %in% rownames(installed.packages())) {
        msg <- gettext("BiocVersion not installed", domain = "R-BiocManager")
        testthat::skip(msg)
    }
}
