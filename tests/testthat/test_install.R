context("install()")

test_that("Arguments are validated", {
    expect_error(
        install("foo", "bar"),
        "all '...' arguments to 'install\\(\\)' must be named"
    )
    expect_error(install(TRUE), "is.character\\(pkgs\\) is not TRUE")
    expect_error(install(ask="foo"), "is.logical\\(ask\\) is not TRUE")
})

test_that("Helpers filter the right packages", {
    .install_filter_r_repos <- BiocManager:::.install_filter_r_repos
    .install_filter_github_repos <- BiocManager:::.install_filter_github_repos

    r <- "foo"
    http <- c("http://foo.bar/baz", "https://foo.bar/baz")
    github <- c("foo/bar", "foo/bar@baz")
    all <- c(r, http, github)

    expect_identical(c(r, http), .install_filter_r_repos(all))
    expect_identical(github, .install_filter_github_repos(all))
})

test_that(".install_repos() works", {
    repos <- repositories()
    expect_identical(character(0), .install_repos(character(), repos = repos))
})

test_that(".install_github() works", {
    repos <- repositories()
    expect_identical(character(0), .install_github(character(), repos = repos))
})

test_that("Versions are checked in install", {
    expect_error(install(version = "0.1"))
    expect_error(install(1:3))
    expect_error(install(NA))
    expect_error(install(c("BioStrings", "S4Vectors", NA)))
    expect_error(install(site_repository = c("string1", "string2")))
    expect_error(install(TRUE))
    expect_error(install(ask = "No"))
    expect_error(install(ask = c("No", "Yes", NA)))
    expect_error(install(version = c("3.7", "3.6")))
    expect_error(install(version = character()))
    expect_error(install(version = ""))
    expect_error(install(version = "3.4.2"))
})

context("install(update = TRUE) filters un-updatable packages")

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

context("install(version =, ask=...) works")

test_that(".install_ask_up_or_down_grade() works non-interactively", {
    if (interactive())
        return(TRUE)
    expect_equal(
        FALSE,
        .install_ask_up_or_down_grade("xx", npkgs = 1L, cmp = 1L, ask = TRUE)
    )
    expect_equal(
        TRUE,
        .install_ask_up_or_down_grade("xx", npkgs = 1L, cmp = 1L, ask = FALSE)
    )
})

test_that("install() fails with different version (non-interactive)", {
    map <- BiocManager:::.version_map()
    incr <- 1L
    version <-
        package_version(paste(version()$major, version()$minor + incr, sep="."))
    expect_error(install(version = version))
})

context(".package_filter_unwriteable works", {
    dir.create(writeable_lib <- tempfile())
    dir.create(unwriteable_lib <- tempfile())
    Sys.chmod(unwriteable_lib, "0500")

    pkgnames <- c("Pkg1", "Pkg2", "Pkg3")
    npkgs <- length(pkgnames)
    instpkg <- structure(
        c(pkgnames,
          writeable_lib, writeable_lib, unwriteable_lib,
          "1.31.0", "1.4.1", "1.4.1",
          rep(NA, npkgs * 12),
          "3.6.0", "3.6.0", "3.6.0"
          ),
        .Dim = c(npkgs, 16L),
        .Dimnames = list(
            pkgnames,
            c(
                "Package", "LibPath", "Version", "Priority", "Depends",
                "Imports", "LinkingTo", "Suggests", "Enhances", "License",
                "License_is_FOSS", "License_restricts_use", "OS_type", "MD5sum",
                "NeedsCompilation", "Built"
            )
        )
    )

    expect_identical(NULL, .package_filter_unwriteable(NULL, NULL, instpkg))

    ## old.packages(repos = BiocManager::repos())

    pkgs <- structure(
        c(pkgnames[1:2],
          writeable_lib, writeable_lib,
          "2.14.2", "1.4.0",
          "3.5.1", "3.5.1",
          "2.14.4", "1.4.1",
          "https://bioconductor.org/packages/3.8/bioc/src/contrib",
          "https://bioconductor.org/packages/3.8/bioc/src/contrib"),
        .Dim = c(2L, 6L),
        .Dimnames = list(
            pkgnames[1:2],
            c("Package", "LibPath", "Installed", "Built", "ReposVer", "Repository")
        )
    )
    expect_identical(pkgs, .package_filter_unwriteable(pkgs, NULL, instpkg))


    ## one writeable, one unwriteable directory
    pkgs <- structure(
        c("Pkg1", "Pkg3",
          writeable_lib, unwriteable_lib,
          "2.14.2", "1.4.0",
          "3.5.1", "3.5.1",
          "2.14.4", "1.4.1",
          "https://bioconductor.org/packages/3.8/bioc/src/contrib",
          "https://bioconductor.org/packages/3.8/bioc/src/contrib"),
        .Dim = c(2L, 6L),
        .Dimnames = list(
            c("Pkg1", "Pkg3"),
            c("Package", "LibPath", "Installed", "Built", "ReposVer", "Repository")
        )
    )
    exp <- pkgs[1, , drop=FALSE]
    expect_identical(exp, .package_filter_unwriteable(pkgs, NULL, instpkg))

    ## two packages in same 'unwriteable' directory
    pkgnames <- c("Pkg1", "Pkg2", "Pkg3")
    pkgs <- structure(
        c(pkgnames,
          writeable_lib, unwriteable_lib, unwriteable_lib,
          "2.14.2", "1.4.0", "1.4.0",
          "3.5.1", "3.5.1", "3.5.1",
          "2.14.4", "1.4.1", "1.4.1",
          "https://bioconductor.org/packages/3.8/bioc/src/contrib",
          "https://bioconductor.org/packages/3.8/bioc/src/contrib",
          "https://bioconductor.org/packages/3.8/bioc/src/contrib"),
        .Dim = c(length(pkgnames), 6L),
        .Dimnames = list(
            pkgnames,
            c("Package", "LibPath", "Installed", "Built", "ReposVer", "Repository")
        )
    )
    exp <- pkgs[1, , drop=FALSE]
    expect_identical(exp, .package_filter_unwriteable(pkgs, NULL))

    ## one package in a writeable and an unwriteable library; both
    ## Pkg1 and Pkg2 can be updated
    pkgnames <- c("Pkg1", "Pkg1", "Pkg2")
    npkgs <- length(pkgnames)
    instpkg <- structure(
        c(pkgnames,
          unwriteable_lib, writeable_lib, writeable_lib,
          "1.31.0", "1.31.1",  "1.0.0", # more recent version in writeable
          rep(NA, npkgs * 12),
          rep("3.6.0", npkgs)
          ),
        .Dim = c(npkgs, 16L),
        .Dimnames = list(
            pkgnames,
            c(
                "Package", "LibPath", "Version", "Priority", "Depends",
                "Imports", "LinkingTo", "Suggests", "Enhances", "License",
                "License_is_FOSS", "License_restricts_use", "OS_type", "MD5sum",
                "NeedsCompilation", "Built"
            )
        )
    )

    pkgnames <- c("Pkg1", "Pkg2")
    npkgs <- length(pkgnames)
    pkgs <- structure(
        c(
            pkgnames,
            unwriteable_lib, writeable_lib,
            "1.31.0", "1.0.0",
            rep("3.6.0", npkgs),
            "1.31.1", "1.0.1",
            rep(NA, npkgs)
        ),
        .Dim = c(length(pkgnames), 6L),
        .Dimnames = list(
            pkgnames,
            c("Package", "LibPath", "Installed", "Built", "ReposVer", "Repository")
        )
    )

    exp <- pkgs[2, , drop = FALSE]
    expect_identical(exp, .package_filter_unwriteable(pkgs, NULL, instpkg))
})
