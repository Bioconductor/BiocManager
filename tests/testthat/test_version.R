context("version returns a Bioconductor version")

test_that("version is package_version class", {
    expect_s3_class(version(), "package_version")
    expect_s3_class(version(), "numeric_version")
})

test_that("version has two components", {
    verNums <- strsplit(as.character(version()), "\\.")[[1L]]
    expect_identical(length(verNums), 2L)
})

test_that("Sentinel works when BiocVersion not installed", {
    skip("Removal of BiocVersion required")
    local({
        expect_identical(Bioconductor::version(), package_version("0.0"))
    })
})
