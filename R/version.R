## sentinel -- BiocVersion package not installed
.BIOCVERSION_SENTINEL <- package_version("0.0")

#' @export
version <-
    function()
{
    if ("BiocVersion" %in% rownames(installed.packages()))
        packageVersion("BiocVersion")[, 1:2]
    else
        .BIOCVERSION_SENTINEL
}

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

.versionDiff <- function(biocver) {
    biocVersion <- Bioconductor::version()
    if (!identical(biocver$major, biocVersion$major))
        stop("Provide a valid Bioconductor version")
    biocver$minor - biocVersion$minor
}

