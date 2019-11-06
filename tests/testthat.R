library(testthat)
library(BiocManager)

.skip_if_misconfigured <-
    function()
{
    if (!"BiocVersion" %in% rownames(installed.packages()))
        return(NULL)

    R_version <- getRversion()
    bioc_version <- packageVersion("BiocVersion")[, 1:2]

    test_ver <- tryCatch({
        BiocManager:::.version_validity(bioc_version)
    }, error = function(err) {
        conditionMessage(err)
    })

    if (!isTRUE(test_ver)) {
        msg <- sprintf("mis-configuration, R %s, Bioc %s, Reason %s",
            R_version, bioc_version, test_ver)
        skip(msg)
    }
}

.skip_if_BiocVersion_not_available <-
    function()
{
    if (!"BiocVersion" %in% rownames(installed.packages()))
        skip("BiocVersion not installed")
}

test_check("BiocManager")
