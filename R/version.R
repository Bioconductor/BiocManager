.VERSION_HELP <- "see https://bioconductor.org/install"

.VERSION_UNKNOWN <-
    "Bioconductor version cannot be determined; no internet connection?"

.VERSION_MAP_UNABLE_TO_VALIDATE <-
    "Bioconductor version cannot be validated; no internet connection?"

.NO_ONLINE_VERSION_DIAGNOSIS <-
    "Bioconductor online version validation disabled;
    see ?BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS"

.VERSION_SENTINEL <- local({
    version <- package_version(list())
    class(version) <- c("unknown_version", class(version))
    version
})

.VERSION_MAP_SENTINEL <- data.frame(
    Bioc = .VERSION_SENTINEL,
    R = .VERSION_SENTINEL,
    BiocStatus = factor(
        factor(),
        levels = c("out-of-date", "release", "devel", "future")
    )
)

.version_compare <-
    function(v1, v2)
{
    ## return -1, 0, or 1 when v1 is <, ==, or > v2
    if (v1 < v2)
        -1L
    else if (v1 > v2)
        1L
    else 0L
}

.version_validity_online_check <-
    function()
{
    opt <- getOption("BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS")
    if (is.null(opt)) {
        opt <- Sys.getenv("BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS", TRUE)
    }

    isTRUE(as.logical(opt))
}

.version_map_get <-
    function()
{
    if (!.version_validity_online_check()) {
        return(.VERSION_MAP_SENTINEL)
    }

    config <- "https://bioconductor.org/config.yaml"
    txt <- suppressWarnings(tryCatch({
        readLines(config)
    }, error = identity))
    if (inherits(txt, "error"))
        return(.VERSION_MAP_SENTINEL)

    grps <- grep("^[^[:blank:]]", txt)
    start <- match(grep("r_ver_for_bioc_ver", txt), grps)
    map <- txt[seq(grps[start] + 1, grps[start + 1] - 1)]
    map <- trimws(gsub("\"", "", sub(" #.*", "", map)))

    pattern <- "(.*): (.*)"
    bioc <- package_version(sub(pattern, "\\1", map))
    r <- package_version(sub(pattern, "\\2", map))

    pattern <- "^release_version: \"(.*)\""
    release <- package_version(
        sub(pattern, "\\1", grep(pattern, txt, value=TRUE))
    )
    pattern <- "^devel_version: \"(.*)\""
    devel <- package_version(
        sub(pattern, "\\1", grep(pattern, txt, value=TRUE))
    )
    status <- rep("out-of-date", length(bioc))
    status[bioc == release] <- "release"
    status[bioc == devel] <- "devel"

    ## append final version for 'devel' R
    bioc <- c(
        bioc, max(bioc)
        ## package_version(paste(unlist(max(bioc)) + 0:1, collapse = "."))
    )
    r <- c(r, package_version(paste(unlist(max(r)) + 0:1, collapse = ".")))
    status <- c(status, "future")

    rbind(.VERSION_MAP_SENTINEL, data.frame(
        Bioc = bioc, R = r,
        BiocStatus = factor(
            status,
            levels = c("out-of-date", "release", "devel", "future")
        )
    ))
}

.version_map <- local({
    version_map <- .VERSION_MAP_SENTINEL
    function() {
        if (identical(version_map, .VERSION_MAP_SENTINEL))
            version_map <<- .version_map_get()
        version_map
    }
})

.version_validity <-
    function(version)
{
    if (!.version_validity_online_check()) {
        .warning(.NO_ONLINE_VERSION_DIAGNOSIS)
        version <- .local_version()
    }

    if (identical(version, "devel"))
        version <- .version_bioc("devel")
    version <- .package_version(version)

    if (identical(version, .VERSION_SENTINEL))
        return(.VERSION_MAP_UNABLE_TO_VALIDATE)

    if (version[, 1:2] != version)
        return(sprintf(
            "version '%s' must have two components, e.g., '3.7'", version
        ))

    map <- .version_map()
    if (identical(map, .VERSION_MAP_SENTINEL))
        return(.VERSION_MAP_UNABLE_TO_VALIDATE)

    if (!version %in% map$Bioc)
        return(sprintf(
            "unknown Bioconductor version '%s'; %s", version, .VERSION_HELP
        ))

    required <- map$R[map$Bioc == version]
    if (!getRversion()[, 1:2] %in% required)
        return(sprintf(
            "Bioconductor version '%s' requires R version '%s'; %s",
            version, required, .VERSION_HELP
        ))

    TRUE
}

.version_is_not_future <-
    function(version)
{
    map <- .version_map()
    if (identical(map, .VERSION_MAP_SENTINEL))
        return(.VERSION_MAP_UNABLE_TO_VALIDATE)

    r_version <- getRversion()[, 1:2]
    status <- map$BiocStatus[map$Bioc == version & map$R == r_version]
    if (identical(status, "future"))
        return(sprintf(
            "Bioconductor does not yet formally support R version '%s'",
            r_version
        ))

    TRUE
}

.version_validate <-
    function(version)
{
    if (identical(version, "devel"))
        version <- .version_bioc("devel")
    version <- .package_version(version)

    txt <- .version_validity(version)
    isTRUE(txt) || .stop(txt)

    version
}

.version_recommend <-
    function(version)
{
    release <- .version_bioc("release")
    if (version < release) {
        return(sprintf(
            "Bioconductor version '%s' is out-of-date; the current release
             version '%s' is available with R version '%s'; %s",
            version, release, .version_R("release"), .VERSION_HELP
        ))
    }

    TRUE
}

.version_choose_best <-
    function()
{
    map <- .version_map()
    if (identical(map, .VERSION_MAP_SENTINEL))
        return(.VERSION_MAP_UNABLE_TO_VALIDATE)

    map <- map[map$R == getRversion()[, 1:2],]
    if ("release" %in% map$BiocStatus)
        idx <- map$BiocStatus == "release"
    else if ("devel" %in% map$BiocStatus)
        idx <- map$BiocStatus == "devel"
    else if ("out-of-date" %in% map$BiocStatus)
        idx <- map$BiocStatus == "out-of-date"
    else
        idx <- map$BiocStatus == "future"

    tail(map$Bioc[idx], 1)
}

.version_bioc <-
    function(type)
{
    map <- .version_map()
    if (identical(map, .VERSION_MAP_SENTINEL))
        return(.VERSION_MAP_UNABLE_TO_VALIDATE)

    map$Bioc[map$BiocStatus == type]
}

.version_R <-
    function(type)
{
    map <- .version_map()
    if (identical(map, .VERSION_MAP_SENTINEL))
        return(.VERSION_MAP_UNABLE_TO_VALIDATE)

    map$R[map$BiocStatus == type]
}

.local_version <-
    function()
{
    if ("BiocVersion" %in% rownames(installed.packages()))
        packageVersion("BiocVersion")[, 1:2]
    else
        .VERSION_SENTINEL
}

#' Version of Bioconductor currently in use.
#'
#' `version()` reports the version of _Bioconductor_ appropropriate
#' for this version of R, or the version of _Bioconductor_ requested
#' by the user.
#'
#' `version()` (and all functions requiring version information) fails
#' when version cannot be validated e.g., because internet access is
#' not available.
#'
#' @return A two-digit version, e.g., `3.8`, of class
#'     `package_version` describing the version of _Bioconductor_ in
#'     use.
#'
#' @md
#' @examples
#' BiocManager::version()
#'
#' @export
version <-
    function()
{
    if ("BiocVersion" %in% rownames(installed.packages()))
        packageVersion("BiocVersion")[, 1:2]
    else
        .version_choose_best()
}

.package_version <-
    function(x)
{
    if (!inherits(x, "package_version"))      # preserved full class attributes
        x <- package_version(x)
    x
}

#' @rdname version
#'
#' @param x An `unknown_version` instance used to represent the
#'     situation when the version of Bioconductor in use cannot be
#'     determined.
#'
#' @param ... Additional arguments, ignored.
#'
#' @md
#' @export
print.unknown_version <-
    function(x, ...)
{
    cat("'unknown'\n")
}
