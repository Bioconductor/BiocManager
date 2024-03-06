.is_CRAN_check <-
    function()
{
    !interactive() && ("CheckExEnv" %in% search())
}

.is_character <-
    function(x, na.ok = FALSE, zchar = FALSE)
{
    is.character(x) &&
        (na.ok || all(!is.na(x))) &&
        (zchar || all(nzchar(x)))
}

.is_scalar_character <- function(x, na.ok = FALSE, zchar = FALSE)
    length(x) == 1L && .is_character(x, na.ok, zchar)

.is_scalar_logical <- function(x, na.ok = FALSE)
    is.logical(x) && length(x) == 1L && (na.ok || !is.na(x))

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
    function(..., call. = FALSE, domain = NULL, appendLF=TRUE)
{
    ## call. = FALSE provides compatibility with .stop(), but is ignored
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
    R_version <- getRversion()
    bioc_version <- version()

    test_ver <- tryCatch({
        .version_validity(bioc_version)
    }, error = function(err) {
        conditionMessage(err)
    })

    if (!isTRUE(test_ver)) {
        msg <- sprintf(
            "mis-configuration, R %s, Bioconductor %s", R_version, bioc_version
        )
        testthat::skip(msg)
    }
}

.skip_if_BiocVersion_not_available <-
    function()
{
    if (!"BiocVersion" %in% rownames(installed.packages()))
        testthat::skip("BiocVersion not installed")
}
