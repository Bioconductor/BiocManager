context("Directory permissions")

test_that("check that directories are writeable", {
    if (.Platform$OS.type != "unix")
        return()

    .unwritableDirectories <- BiocManager:::.unwritableDirectories

    dir.create(f <- tempfile(), mode="400")
    dir.create(g <- tempfile())
    on.exit(Sys.chmod(f, mode="777"))

    res <- suppressWarnings({
        .unwritableDirectories(c(f, g))
    })

    expect_identical(f, res)
})
