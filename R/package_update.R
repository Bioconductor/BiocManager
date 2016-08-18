.package_filter_suppress_updates <-
    function(pkgs, suppressUpdates)
{
    if (!is.logical(suppressUpdates)) {
        ## alternate form of argument: vector of regex
        regex <- paste(suppressUpdates, collapse="|")
        hits <- !grepl(regex, pkgs[,"Package"])
        pkgs[hits, , drop=FALSE]
    } else {
        pkgs
    }
}

.package_filter_masked <-
    function(pkgs)
{
    idx <- order(match(pkgs[, "LibPath"], .libPaths()))
    dup <- duplicated(pkgs[idx,"Package"])[order(idx)]
    pkgs[!dup,, drop=FALSE]
}

.package_filter_unwriteable <-
    function(pkgs, instlib=NULL)
{
    if (!nrow(pkgs)) return(pkgs)

    libs <- if (is.null(instlib)) pkgs[,"LibPath"] else instlib

    ulibs <- unique(libs)
    status <- dir.exists(ulibs)

    if (.Platform$OS.type == "windows") {
        status[status] <- vapply(ulibs[status], function(lib) {
            ## from tools::install.R: file.access() unreliable on
            ## Windows
            fn <- file.path(lib, paste0("_test_dir", Sys.getpid()))
            unlink(fn, recursive = TRUE) # precaution
            res <- try(dir.create(fn, showWarnings = FALSE))
            if (inherits(res, "try-error") || !res) status <- FALSE
            else unlink(fn, recursive = TRUE)
            status
        }, logical(1))
    } else
        status[status] <- file.access(ulibs[status], 2L) == 0

    status <- status[match(libs, ulibs)]
    if (!all(status))
        .message(
            "installation path not writeable, unable to update packages: %s",
            paste(pkgs[!status, "Package"], collapse=", "))

    pkgs[status,, drop=FALSE]
}
