#' @importFrom utils available.packages install.packages old.packages
#'     update.packages
NULL

.inet_warning <-
    function(w)
{
    if (.is_CRAN_check()) {
        .message(conditionMessage(w))
    } else {
        warning(w)
    }
    invokeRestart("muffleWarning")
}

.inet_error <-
    function(e)
{
    if (.is_CRAN_check()) {
        .message(conditionMessage(e))
    } else {
        stop(e)
    }
}

.inet_readChar <-
    function(...)
{
    withCallingHandlers({
        tryCatch({
            readChar(...)
        }, error = function(e) {
            .inet_error(e)
            character()
        })
    }, warning = .inet_warning)
}

#' @importFrom utils download.file
.inet_readLines <-
    function(...)
{
    withCallingHandlers({
        tryCatch({
            tmp_config <- tempfile()
            download.file(..., destfile = tmp_config, quiet = TRUE)
            readLines(tmp_config)
        }, error = function(e) {
            .inet_error(e)
            e
        })
    }, warning = .inet_warning)
}

.inet_available.packages <-
    function(...)
{
    withCallingHandlers({
        tryCatch({
            available.packages(...)
        }, error = function(e) {
            .inet_error(e)
            colnames <- c(
                "Package", "Version", "Priority", "Depends",
                "Imports", "LinkingTo", "Suggests", "Enhances",
                "License", "License_is_FOSS", "License_restricts_use",
                "OS_type", "Archs", "MD5sum", "NeedsCompilation",
                "File", "Repository"
            )
            matrix(character(0), ncol = 17, dimnames = list(NULL, colnames))
        })
    }, warning = .inet_warning)
}        

.inet_install.packages <-
    function(...)
{
    ## More generous timeout for large package download, see
    ## `?download.file` and, for instance,
    ## https://stat.ethz.ch/pipermail/bioc-devel/2020-November/017448.html
    if (identical(as.integer(getOption("timeout")), 60L)) { # change default only
        otimeout <- options(timeout = 300L)
        on.exit(options(otimeout))
    }
    withCallingHandlers({
        tryCatch({
            install.packages(...)
        }, error = function(e) {
            .inet_error(e)
            invisible(NULL)
        })
    }, warning = function(w) {
        msg <- conditionMessage(w)
        if (grepl("not available", msg)) {
            msg <- gsub(
                "this version of R",
                paste0("Bioconductor version ", "'", version(), "'"),
                msg
            )
            w <- simpleWarning(msg, conditionCall(w))
        }
        .inet_warning(w)
    })
}

.inet_old.packages <-
    function(...)
{
    withCallingHandlers({
        tryCatch({
            old.packages(...)
        }, error = function(e) {
            .inet_error(e)
            invisible(NULL)
        })
    }, warning = .inet_warning)
}        

.inet_update.packages <-
    function(...)
{
    ## see .inet_old.packages for implementation note
    if (identical(as.integer(getOption("timeout")), 60L)) {
        otimeout <- options(timeout = 300L)
        on.exit(options(otimeout))
    }
    withCallingHandlers({
        tryCatch({
            update.packages(...)
        }, error = function(e) {
            .inet_error(e)
            invisible(NULL)
        })
    }, warning = .inet_warning)
}        
