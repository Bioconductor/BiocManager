context("Check for the valid function")

test_that("valid returns an empty list without internet", {
    .skip_if_misconfigured()

    result <- with_mock(
        `BiocManager:::.repositories_filter` = function(...) {
            ..1[FALSE]
        }, BiocManager::valid()
    )

    expect_true(
        is(result, "biocValid")
    )

    expect_identical(
        lengths(result),
        c(out_of_date = 0L, too_new = 0L)
    )
})

test_that("out of date filtering of BiocVersion works", {
    expect_null(
        .valid_out_of_date_filter(NULL)
    )
    out_data_names <-  c(
        "Package", "LibPath", "Installed", "Built", "ReposVer", "Repository"
    )
    valid_out <- matrix(
        data = c(
            "BiocVersion", "/unit/test/lib/path", "3.17.1", "4.3.0",
            "3.18.0", "https://bioconductor.org/packages/3.18/bioc/src/contrib"
        ),
        nrow = 1, byrow = TRUE,
        dimnames = list(
            "BiocVersion",
            out_data_names
        )
    )
    expect_identical(
        .valid_out_of_date_filter(valid_out),
        matrix(
            data = character(0), nrow = 0,
            ncol = length(out_data_names),
            dimnames = list(NULL, out_data_names)
        )
    )
    valid_out <- matrix(
        data = c(
            "BiocVersion", "/unit/test/lib/path", "3.17.1", "4.3.0",
            "3.18.0", "https://bioconductor.org/packages/3.18/bioc/src/contrib",
            "apackage", "/unit/test/lib/path", "3.17.1", "4.3.0",
            "3.18.0", "https://bioconductor.org/packages/3.18/bioc/src/contrib"
        ),
        nrow = 2, byrow = TRUE,
        dimnames = list(
            c("BiocVersion", "apackage"),
            out_data_names
        )
    )
    expect_identical(
        .valid_out_of_date_filter(valid_out),
        matrix(
            data = c(
                "apackage", "/unit/test/lib/path", "3.17.1", "4.3.0", "3.18.0",
                "https://bioconductor.org/packages/3.18/bioc/src/contrib"
            ),
            nrow = 1, byrow = TRUE,
            dimnames = list("apackage", out_data_names)
        )
    )
})
