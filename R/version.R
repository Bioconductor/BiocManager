.VERSION_HELP <- "see https://bioconductor.org/install"

.VERSION_UNKNOWN <-
    "Bioconductor version cannot be determined; no internet connection?
    See #troubleshooting section in vignette"

.VERSION_MAP_UNABLE_TO_VALIDATE <-
    "Bioconductor version cannot be validated; no internet connection?
    See #troubleshooting section in vignette"

.VERSION_MAP_MISCONFIGURATION <-
    "Bioconductor version map cannot be validated; is it misconfigured?
    See #troubleshooting section in vignette"

.VERSION_TYPE_MISSPECIFICATION <-
    "Bioconductor version cannot be validated; is type input misspecified?
    See #troubleshooting section in vignette"

.NO_ONLINE_VERSION_DIAGNOSIS <-
    "Bioconductor online version validation disabled;
    see ?BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS"

.VERSION_TAGS <-
    c("out-of-date", "release", "devel", "future")

.VERSION_MAP_SENTINEL <- data.frame(
    Bioc = package_version(character()),
    R = package_version(character()),
    BiocStatus = factor(
        factor(),
        levels = .VERSION_TAGS
    )
)

## version-specific options

.version_force_version <-
    function()
{
    ## check R_BIOC_VERSION environment variable
    force_version <- Sys.getenv("R_BIOC_VERSION", "")

    ## return either NA (do no force version) or the version to use
    if (nzchar(force_version)) {
        package_version(force_version)
    } else {
        NA
    }
}

.version_sentinel <-
    function(msg)
{
    version <- package_version(NA_character_, strict = FALSE)
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
    if (inherits(txt, "error")) {
        if (startsWith(config, "https://"))
            config <- sub("https", "http", config)
        else if (!startsWith(config, "http"))
            config <- paste0("file://", config)
        txt <- tryCatch(.inet_readLines(config), error = identity)
    }
    txt
}

.version_map_config_element <-
    function(txt, tag)
{
    grps <- grep("^[^[:blank:]]", txt)
    start <- match(grep(tag, txt), grps)
    if (!length(start))
        return(setNames(character(), character()))
    end <- ifelse(length(grps) < start + 1L, length(txt), grps[start + 1] - 1L)
    map <- txt[seq(grps[start] + 1, end)]
    map <- trimws(gsub("\"", "", sub(" #.*", "", map)))

    pattern <- "(.*): (.*)"
    key <- sub(pattern, "\\1", map)
    value <- sub(pattern, "\\2", map)
    setNames(value, key)
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

    if (!length(txt) || inherits(txt, "error"))
        return(.VERSION_MAP_SENTINEL)

    bioc_r_map <- .version_map_config_element(txt, "r_ver_for_bioc_ver")
    if (!length(bioc_r_map))
        return(.VERSION_MAP_SENTINEL)
    bioc <- package_version(names(bioc_r_map))
    r <- package_version(unname(bioc_r_map))

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
            levels = .VERSION_TAGS
        )  
    ))
}

.version_map_get_offline <-
    function()
{
    bioc <- .version_BiocVersion()
    if (is.na(bioc))
        return(.VERSION_MAP_SENTINEL)

    r <- .version_R_version()[,1:2]

    status <- .VERSION_TAGS
    rbind(.VERSION_MAP_SENTINEL, data.frame(
        Bioc = bioc, R = r,
        BiocStatus = factor(
            NA,
            levels = status
        )
    ))
}

.version_map_get <-
    function(config = NULL)
{
    if (!.version_validity_online_check())
        .version_map_get_offline()
    else {
        if (is.null(config) || !nchar(config))
            config <- "https://bioconductor.org/config.yaml"
        .version_map_get_online(config)
    }
}

.version_map <- local({
    version_map <- .VERSION_MAP_SENTINEL
    function() {
        config <- Sys.getenv("BIOCONDUCTOR_CONFIG_FILE")
        config <- getOption("BIOCONDUCTOR_CONFIG_FILE", config)
        if (identical(version_map, .VERSION_MAP_SENTINEL))
            version_map <<- .version_map_get(config)
        version_map
    }
})

.version_field <-
    function(field)
{
    map <- .version_map()
    if (identical(map, .VERSION_MAP_SENTINEL))
        return(NA)
    idx <- match(version(), map[["Bioc"]])
    map[idx, field]
}

.version_R_version <- function()
    getRversion()

.version_BiocVersion_installed <- function()
    nzchar(system.file(package = "BiocVersion"))

.version_BiocVersion <-
    function()
{
    if (.version_BiocVersion_installed())
        packageVersion("BiocVersion")[, 1:2]
    else
        .version_sentinel("BiocVersion is not installed")
}

.version_string <-
    function(bioc_version = version())
{
    sprintf(
        "Bioconductor version %s (BiocManager %s), %s",
        bioc_version, packageVersion("BiocManager"),
        sub(" version", "", R.version.string)
    )
}

## .version_validity() returns TRUE if the version is valid for this
## version of R, or a text string (created with sprintf()) explaining why
## the version is invalid. It does NOT call message / warning / etc
## directly.
.version_validity <-
    function(version, map = .version_map(), r_version = .version_R_version(),
             check_future = FALSE)
{
    if (!is.na(.version_force_version())) {
        return(sprintf(
            "Using environment variable R_BIOC_VERSION = '%s'",
            version
        ))
    }
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

    if (!all(.VERSION_TAGS %in% map$BiocStatus))
        return(.VERSION_MAP_MISCONFIGURATION)

    if (!version %in% map$Bioc)
        return(sprintf(
            "unknown Bioconductor version '%s'; %s", version, .VERSION_HELP
        ))

    required <- map$R[map$Bioc == version & !map$BiocStatus %in% "future"]
    r_version <- r_version[, 1:2]
    if (!r_version %in% required) {
        rec <- map[map$R == r_version, , drop = FALSE]
        one_up <- required
        one_up[, 2] <- as.integer(required[, 2]) + 1L
        if (r_version == one_up && "future" %in% rec$BiocStatus) {
            if (check_future) {
                return(sprintf(
                    "Bioconductor does not yet build and check packages for R
                     version %s, using unsupported Bioconductor version %s; %s",
                    r_version, version, .VERSION_HELP
                ))
            }
        } else {
            rec_fun <- ifelse("devel" %in% rec$BiocStatus, head, tail)
            rec_msg <- sprintf(
                "use `version = '%s'` with R version %s",
                rec_fun(rec$Bioc, 1), r_version
            )

            return(sprintf(
                "Bioconductor version '%s' requires R version '%s'; %s; %s",
                version, head(required, 1), rec_msg, .VERSION_HELP
            ))
        }
    }

    TRUE
}

.version_validate <-
    function(version)
{
    if (identical(version, "devel"))
        version <- .version_bioc("devel")
    version <- .package_version(version)
    force_version <- .version_force_version()
    if (identical(version, force_version))
        return(version)

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
                 not support R version '%s'. Older installations of
                 Bioconductor are no longer supported as the 'BiocInstaller'
                 script has been defunct. Use an R version greater than '3.5'
                 and install the latest version of Bioconductor with
                'BiocManager'.",
                version, getRversion()
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

    if (!all(.VERSION_TAGS %in% map$BiocStatus))
        return(.version_sentinel(.VERSION_MAP_MISCONFIGURATION))

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

    if (!all(.VERSION_TAGS %in% map$BiocStatus))
        return(.VERSION_MAP_MISCONFIGURATION)

    if (!type %in% .VERSION_TAGS)
        return(.VERSION_TYPE_MISSPECIFICATION)

    version <- map$Bioc[map$BiocStatus == type]
    if (!length(version) || is.na(version))
        version <- .VERSION_UNKNOWN
    version
}

.version_R <-
    function(type)
{
    map <- .version_map()
    if (identical(map, .VERSION_MAP_SENTINEL))
        return(.VERSION_MAP_UNABLE_TO_VALIDATE)

    if (!all(.VERSION_TAGS %in% map$BiocStatus))
        return(.VERSION_MAP_MISCONFIGURATION)

    if (!type %in% .VERSION_TAGS)
        return(.VERSION_TYPE_MISSPECIFICATION)

    version <- map$R[map$BiocStatus == type]
    if (!length(version) || is.na(version))
        version <- .VERSION_UNKNOWN
    version
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
#' The environment variable `R_BIOC_VERSION` can be used to specify a
#' version that is not consistent with *Bioconductor* release
#' versioning. Use of this variable is strongly discouraged.
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
    bioc <- .version_force_version()
    if (is.na(bioc))
        bioc <- .version_BiocVersion()
    if (is.na(bioc))
        bioc <- .version_choose_best()

    bioc
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
    cat(format(x), "\n", sep = "")
}
