context("version returns a Bioconductor version")

test_that("version is package_version class", {
    expect_s3_class(version(), "package_version")
    expect_s3_class(version(), "numeric_version")
})

test_that("version has two components", {
    verNums <- strsplit(as.character(version()), "\\.")[[1L]]
    expect_identical(length(verNums), 2L)
})

test_that(".version_validate() validates version", {
    .version_validate <- BiocManager:::.version_validate

    expect_error(
        .version_validate("2.0"),
        "Bioconductor version 2.0 requires R version 2.5"
    )

    expect_error(
        .version_validate("1.2.3"),
        "'version' 1.2.3 must have two components, e.g., '3.7'"
    )

    expect_error(
        .version_validate("100.1"),
        "unknown Bioconductor version 100.1"
    )
})

test_that("Sentinel works when BiocVersion not installed", {
    skip("Removal of BiocVersion required")
    local({
        expect_identical(BiocManager::version(), package_version("0.0"))
    })
})
