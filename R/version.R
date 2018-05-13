## sentinel -- BiocVersion package not installed
.BIOCVERSION_SENTINEL <- package_version("0.0")

.VERSION_HELP <- "see https://bioconductor.org/install"

.compare_version <-
    function(v1, v2)
{
    ## return -1, 0, or 1 when v1 is <, ==, or > v2
    if (v1 < v2)
        -1
    else if (v1 > v2)
        1
    else 0
}

.version_map_get <-
    function()
{
    config <- "https://bioconductor.org/config.yaml"
    txt <- readLines(config)
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
    status[bioc > devel] <- "future"

    data.frame(
        Bioc = bioc, R = r,
        BiocStatus = factor(
            status,
            levels = c("out-of-date", "release", "devel", "future")
        )
    )
}

.version_map <- local({
    version_map <- NULL
    function() {
        if (is.null(version_map))
            version_map <<- .version_map_get()
        version_map
    }
})

.version_validity <-
    function(version)
{
    version <- package_version(version)

    if (version[, 1:2] != version)
        return(.msg(
            "version '%s' must have two components, e.g., '3.7'", version
        ))

    map <- .version_map()

    if (!version %in% map$Bioc)
        return(sprintf("unknown Bioconductor version '%s'; %s",
                       version, .VERSION_HELP))

    required <- map$R[map$Bioc == version]
    if (required != getRversion()[, 1:2])
        return(sprintf(
            "Bioconductor version '%s' requires R version '%s'; %s",
            version, required, .VERSION_HELP
        ))

    status <- map$BiocStatus[map$Bioc == version]
    if (status == "future")
        return(sprintf(
            "Bioconductor version '%s' is not yet available; %s",
            version, .VERSION_HELP
        ))

    TRUE
}

.version_validate <-
    function(version)
{
    version <- package_version(version)

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
    map$Bioc[map$BiocStatus == type]
}

.version_R <-
    function(type)
{
    map <- .version_map()
    map$R[map$BiocStatus == type]
}

#' Version of Bioconductor currently installed
#'
#' The `install()` function arranges to install the BiocVersion
#' package. The BiocVersion package has a version number that
#' corresponds to the version of _Bioconductor_ in use. `version()`
#' reports on this version. If BiocVersion is not intalled,
#' `version()` returns a sentinel (version '0.0').
#'
#' @return A two-digit version, e.g., `3.7`, of class
#'     `package_version` describing the version of _Bioconductor_ in
#'     use. A version of `0.0` indicates that information on
#'     _Bioconductor_ version is not available (i.e., the BiocVersion
#'     package is not installed).
#'
#' @md
#' @examples
#' version()
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
