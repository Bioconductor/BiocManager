library(testthat)
library(BiocManager)

.skip_if_misconfigured <-
    function()
{
    if (!"BiocVersion" %in% rownames(installed.packages()))
        return(NULL)

    R_version <- getRversion()
    bioc_version <- packageVersion("BiocVersion")[, 1:2]
    if (R_version == "4.0.0" && bioc_version == "3.9")
        skip("mis-configuration, R 4.0, Bioc 3.9")
}

.skip_if_BiocVersion_not_available <-
    function()
{
    if (!"BiocVersion" %in% rownames(installed.packages()))
        skip("BiocVersion not installed")
}

test_check("BiocManager")
