.is_CRAN_check <-
    function()
{
    !interactive() && ("CheckExEnv" %in% search())
}

.getAnswer <- function(msg, allowed)
{
    if (interactive()) {
        allowed_values <- unique(tolower(allowed))
        allowed_string <- paste(allowed_values, collapse = "/")
        msg <- sprintf("%s [%s] ", trimws(msg), allowed_string)
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

#' @useDynLib BiocManager, .registration=TRUE
.hash <-
    function(x)
{
    .Call(.hash_impl, x)
}

.gettext_digest_option <-
    function()
{
    value <- getOption("BiocManager.add_translation_digest", TRUE)
    digest_option <- as.logical(value)
    if (!(isTRUE(digest_option) || isFALSE(digest_option))) {
        value <- as.character(value)
        if (!(is.character(value) && length(value) == 1L))
            value <- "???"
        msg <- gettextf(
            "The value of 'getOption(\"BiocManager.add_translation_digest\")' could not be coerced to `TRUE` or `FALSE`. See the help page `?BiocManager` for more information. The value of the option is: '%s'.",
            value,
            digest = TRUE
        )
        .stop(msg)
    }

    digest_option
}

.gettext_add_digest_prefix <-
    function(msg, translation, digest)
{
    if (digest) {
        hash <- .hash(msg)
        sprintf("[%s] %s", hash, translation)
    } else {
        ## no-op
        translation
    }
}

gettext <-
    function(msg, domain = NULL, digest = .gettext_digest_option())
{
    translation <- base::gettext(msg, domain = domain)
    .gettext_add_digest_prefix(msg, translation, digest)
}

gettextf <-
    function(fmt, ..., domain = NULL, digest = .gettext_digest_option())
{
    translation <- base::gettextf(fmt, ..., domain = domain)
    .gettext_add_digest_prefix(fmt, translation, digest)
}

.msg <-
    function(
        txt,
        width=getOption("width") - 7L, indent = 0, exdent = 2,
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
            "This session is mis-configured. It has R version '%s' and Bioconductor version '%s'.",
            R_version, bioc_version,
            domain = "R-BiocManager"
        )
        testthat::skip(txt)
    }
}

.skip_if_BiocVersion_not_available <-
    function()
{
    if (!"BiocVersion" %in% rownames(installed.packages())) {
        msg <- gettext(
            "The 'BiocVersion' package is not installed.",
            domain = "R-BiocManager"
        )
        testthat::skip(msg)
    }
}
