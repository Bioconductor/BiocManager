## sentinel -- BiocVersion package not installed
.BIOCVERSION_SENTINEL <- package_version("0.0")

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
        Status = factor(
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

.version_validate <-
    function(to)
{
    map <- .version_map()

    if (!to %in% map$Bioc)
        .stop("unknown Bioconductor version %s", to)

    status <- map$Status[map$Bioc == to]
    if (status == "out-of-date")
        .message("out-of-date Bioconductor version selected")

    required <- map$R[map$Bioc == to]
    if (required != getRversion()[, 1:2])
        .stop("Bioconductor version %s requires R version %s", to, required)

    if (status == "future")
        .stop("Bioconductor version %s is not yet available", to)

    to
}

.version_choose_best <-
    function()
{
    map <- .version_map()
    map <- map[map$R == getRversion()[, 1:2],]

    if ("release" %in% map$Status)
        idx <- map$Status == "release"
    else if ("devel" %in% map$Status)
        idx <- map$Status == "devel"
    else if ("out-of-date" %in% map$Status)
        idx <- map$Status == "out-of-date"
    else
        idx <- map$Status == "future"

    map$Bioc[idx]
}

#' Version of Bioconductor currently installed
#'
#' The `install()` function arranges to install the BiocVersion
#' package. The BiocVersion package has a version number that
#' corresponds to the version of _Bioconductor_ in use. `version()`
#' reports on this version. If BiocVersion is not intalled,
#' `version()` returns a sentinel (version '0.0').
#'
#' @return A two-digit version, e.g., `'3.7'`, of class
#'     `package_version` describing the version of _Bioconductor_ in
#'     use. A version of `'0.0'` indicates that information on
#'     _Bioconductor_ version is not available (i.e., the BiocVersion
#'     package is not installed).
#'
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
        .BIOCVERSION_SENTINEL
}
