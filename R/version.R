.VERSION_HELP <- "see https://bioconductor.org/install"

.VERSION_UNKNOWN <-
    "Bioconductor version cannot be determined; no internet connection?"

.VERSION_MAP_UNABLE_TO_VALIDATE <-
    "Bioconductor version cannot be validated; no internet connection?"

.NO_ONLINE_VERSION_DIAGNOSIS <-
    "Bioconductor online version validation disabled;
    see ?BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS"

.LEGACY_INSTALL_CMD <-
    "source(\"https://bioconductor.org/biocLite.R\")"

.VERSION_MAP_SENTINEL <- data.frame(
    Bioc = package_version(list()),
    R = package_version(list()),
    BiocStatus = factor(
        factor(),
        levels = c("out-of-date", "release", "devel", "future")
    )
)

.version_sentinel <-
    function(msg)
{
    version <- package_version(list())
    structure(
        unclass(version),
        msg = msg,
        class = c("version_sentinel", class(version))
    )
}

.version_sentinel_msg <-
    function(x)
{
    attr(x, "msg")
}

#' @export
format.version_sentinel <-
    function(x, ...)
{
    paste0("unknown version: ", .version_sentinel_msg(x))
}

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

.VERSION_MAP <- local({
    WARN_NO_ONLINE_CONFIG <- TRUE
    environment()
})

.version_validity_online_check <-
    function()
{
    opt <- Sys.getenv("BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS", TRUE)
    opt <- getOption("BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS", opt)
    opt <- isTRUE(as.logical(opt))

    if (.VERSION_MAP$WARN_NO_ONLINE_CONFIG && !opt) {
        .VERSION_MAP$WARN_NO_ONLINE_CONFIG <- FALSE
        .warning(.NO_ONLINE_VERSION_DIAGNOSIS)
    }
    opt
}

.version_map_get_online_config <-
    function(config)
{
    txt <- tryCatch(.inet_readLines(config), error = identity)
    if (inherits(txt, "error") && startsWith(config, "https://")) {
        config <- sub("https", "http", config)
        txt <- tryCatch(.inet_readLines(config), error = identity)
    }
    txt
}

.version_map_get_online <-
    function(config)
{
    toggle_warning <- FALSE
    withCallingHandlers({
        txt <- .version_map_get_online_config(config)
    }, warning = function(w) {
        if (!.VERSION_MAP$WARN_NO_ONLINE_CONFIG)
            invokeRestart("muffleWarning")
        toggle_warning <<- TRUE
    })
    if (toggle_warning)
        .VERSION_MAP$WARN_NO_ONLINE_CONFIG <- FALSE

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
    if (max(r) == package_version("3.6")) {
        future_r <- package_version("4.0")
    } else {
        future_r <- package_version(paste(unlist(max(r)) + 0:1, collapse = "."))
    }
    r <- c(r, future_r)
    status <- c(status, "future")

    rbind(.VERSION_MAP_SENTINEL, data.frame(
        Bioc = bioc, R = r,
        BiocStatus = factor(
            status,
            levels = c("out-of-date", "release", "devel", "future")
        )
    ))
}

.version_map_get_offline <-
    function()
{
    bioc <- tryCatch({
        packageVersion("BiocVersion")[, 1:2]
    }, error = identity)
    if (inherits(bioc, "error"))
        return(.VERSION_MAP_SENTINEL)

    r <- package_version(R.Version())[,1:2]

    status <- c("out-of-date", "release", "devel", "future")
    rbind(.VERSION_MAP_SENTINEL, data.frame(
        Bioc = bioc, R = r,
        BiocStatus = factor(
            NA,
            levels = c("out-of-date", "release", "devel", "future")
        )
    ))
}

.version_map_get <-
    function(config = NULL)
{
    if (!.version_validity_online_check())
        .version_map_get_offline()
    else {
        if (is.null(config))
            config <- "https://bioconductor.org/config.yaml"
        .version_map_get_online(config)
    }
}

.version_map <- local({
    version_map <- .VERSION_MAP_SENTINEL
    function() {
        if (identical(version_map, .VERSION_MAP_SENTINEL))
            version_map <<- .version_map_get()
        version_map
    }
})

.get_R_version <- function()
    getRversion()


.version_validity <-
    function(version, map = .version_map(), r_version = .get_R_version())
{
    if (identical(version, "devel"))
        version <- .version_bioc("devel")
    version <- .package_version(version)

    if (inherits(version, "version_sentinel"))
        return(.version_sentinel_msg(version))

    if (version[, 1:2] != version)
        return(sprintf(
            "version '%s' must have two components, e.g., '3.7'", version
        ))

    if (identical(map, .VERSION_MAP_SENTINEL))
        return(.VERSION_MAP_UNABLE_TO_VALIDATE)

    if (!version %in% map$Bioc)
        return(sprintf(
            "unknown Bioconductor version '%s'; %s", version, .VERSION_HELP
        ))

    required <- map$R[map$Bioc == version & map$BiocStatus != "future"]
    r_version <- r_version[, 1:2]
    if (!r_version %in% required) {
        rec <- map[map$R == r_version, , drop = FALSE]
        rec_fun <- ifelse("devel" %in% rec$BiocStatus, head, tail)

        rec_msg <- if ("future" %in% rec$BiocStatus)
            "R version is too new"
        else
            sprintf(
                "use `BiocManager::install(version = '%s')` with R version %s",
                rec_fun(rec$Bioc, 1), r_version
            )

        return(.msg(
            "Bioconductor version '%s' requires R version '%s'; %s; %s",
            version, head(required, 1), rec_msg, .VERSION_HELP
        ))
    }

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
    isTRUE(txt) || ifelse(.is_CRAN_check(), .message(txt), .stop(txt))

    version
}

.r_version_lt_350 <-
    function()
{
    getRversion() < package_version("3.5.0")
}

.version_recommend <-
    function(version)
{
    release <- .version_bioc("release")
    if (is.package_version(release) && version < release) {
        if (.r_version_lt_350())
            return(sprintf(
                "Bioconductor version '%s' is out-of-date; BiocManager does
                 not support R version '%s'. For older installations of
                 Bioconductor, use '%s' and refer to the 'BiocInstaller'
                 vignette on the Bioconductor website",
                version, getRversion(), .LEGACY_INSTALL_CMD
            ))
        else
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
        return(.version_sentinel(.VERSION_MAP_UNABLE_TO_VALIDATE))

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

    version <- map$Bioc[map$BiocStatus == type]
    if (is.na(version))
        version <- .VERSION_UNKNOWN
    version
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
    tryCatch({
        packageVersion("BiocVersion")[, 1:2]
    }, error = function(e) {
        .version_sentinel("package 'BiocVersion' not installed")
    })
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
    tryCatch({
        packageVersion("BiocVersion")[, 1:2]
    }, error = function(e) {
        .version_choose_best()
    })
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
print.version_sentinel <-
    function(x, ...)
{
    cat(format(x), "\n")
}
