context("Make use of various package type filters")

test_that("masked packages are filtered", {
    .filter <- BiocManager:::.package_filter_masked

    pkgs0 <- matrix(
        character(), 0, 2,
        dimnames=list(NULL, c("Package", "LibPath")))

    expect_identical(pkgs0, .filter(pkgs0))

    paths <- c(tempfile(), tempfile())
    for (path in paths) dir.create(path)
    oLibPaths <- .libPaths()
    on.exit(.libPaths(oLibPaths))
    .libPaths(paths)

    pkgs <- matrix(
        c("Foo", "Bar", "Baz", "Bim", paths, paths), 4, 2,
        dimnames=list(c("Foo", "Bar", "Baz", "Bim"), c("Package", "LibPath")))
    expect_identical(pkgs, .filter(pkgs))
    expect_identical(pkgs[c(1, 3, 2),], .filter(pkgs[c(1, 3, 2),]))

    pkgs <- matrix(
        c("Foo", "Bar", "Foo", paths, paths[2]), 3, 2,
        dimnames=list(c("Foo", "Bar", "Foo"), c("Package", "LibPath")))
    expect_identical(pkgs[1:2,], .filter(pkgs))
    pkgs <- pkgs[3:1,]
    expect_identical(pkgs[2:3,], .filter(pkgs))
})

test_that("unwriteable packages are not considered", {
    .filter <- BiocManager:::.package_filter_unwriteable
    ## setup
    dir.create(p0 <- tempfile())
    on.exit(unlink(p0, recursive=TRUE))

    pkgs0 <- matrix(
        character(), 0, 2,
        dimnames=list(NULL, c("Package", "LibPath")))

    pkgs <- pkgs0
    expect_identical(pkgs, .filter(pkgs, NULL))
    expect_identical(pkgs, .filter(pkgs, character()))
    expect_identical(pkgs, .filter(pkgs, tempdir()))

    pkgs <- matrix(c("Foo", p0), 1, byrow=TRUE,
                   dimnames=list("Foo", c("Package", "LibPath")))
    expect_identical(pkgs, .filter(pkgs, NULL))
    expect_identical(pkgs, .filter(pkgs, p0))

    p1 <- tempfile()
    pkgs <- matrix(c("Foo", p1), 1, byrow=TRUE,
                   dimnames=list("Foo", c("Package", "LibPath")))
    expect_identical(pkgs[FALSE,, drop=FALSE], .filter(pkgs, NULL))
    expect_identical(pkgs[FALSE,, drop=FALSE], .filter(pkgs, p1))
    expect_identical(pkgs, .filter(pkgs, p0))

    pkgs <- matrix(
        c("Foo", p0, "Bar", p1, "Baz", p0), 3, 2, byrow=TRUE,
        dimnames=list(c("Foo", "Bar", "Baz"), c("Package", "LibPath")))
    expect_identical(pkgs[c(1, 3),], .filter(pkgs, NULL))
    expect_identical(pkgs, .filter(pkgs, p0))
    expect_identical(pkgs0, .filter(pkgs, p1))

    msg <- tryCatch(.filter(pkgs, NULL), message=conditionMessage)
    expect_identical(
        "installation path not writeable, unable to update packages: Bar\n",
        msg)

    if (.Platform$OS.type == "windows")
        ## how to create a read-only directory?
        return(TRUE)

    dir.create(p2 <- tempfile(), mode="0400") # read but not write
    pkgs <- matrix(c("Foo", p2), 1, byrow=TRUE,
                   dimnames=list("Foo", c("Package", "LibPath")))
    expect_identical(pkgs0, .filter(pkgs, NULL))

    pkgs <- matrix(
        c("Foo", p0, "Bar", p2, "Baz", p0), 3, 2, byrow=TRUE,
        dimnames=list(c("Foo", "Bar", "Baz"), c("Package", "LibPath")))
    expect_identical(pkgs[c(1, 3),], .filter(pkgs, NULL))
    expect_identical(pkgs0, .filter(pkgs, p2))

    Sys.chmod(p2, mode="0700")
    unlink(p2, recursive=TRUE)
})

test_that("packages can be written", {
    skip("too idiosyncratic for standardized testing")

    lib <- system.file(package="BiocManager", "tests", "cases",
        "lib", "Biobase")
    dir.create(locked <- tempfile())
    file.copy(lib, locked, recursive=TRUE)

    oLibPaths <- .libPaths()
    on.exit(.libPaths(oLibPaths))
    .libPaths(c(locked, .libPaths()))

    Sys.chmod(locked, mode="0500")
    install()
    Sys.chmod(locked, mode="0700")
})
