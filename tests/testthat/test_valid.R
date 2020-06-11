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
