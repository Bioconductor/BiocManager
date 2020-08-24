context("version()")

inst_pkgs <- installed.packages()

test_that("version is package_version class", {
    expect_s3_class(version(), "package_version")
    expect_s3_class(version(), "numeric_version")
})

test_that("version has two components", {
    .skip_if_BiocVersion_not_available()
    verNums <- strsplit(as.character(version()), "\\.")[[1L]]
    expect_identical(length(verNums), 2L)
})

test_that(".version_validate() validates version", {
    skip_if_offline()

    .version_validate <- BiocManager:::.version_validate

    expect_error(
        .version_validate("2.0"),
        "Bioconductor version '2.0' requires R version '2.5'; .*"
    )

    expect_error(
        .version_validate("1.2.3"),
        "version '1.2.3' must have two components, e.g., '3.7'"
    )

    expect_error(
        .version_validate("100.1"),
        "unknown Bioconductor version '100.1'; .*"
    )

})

test_that(".version_recommend() recommends update", {
    skip_if_offline()
    expect_true(startsWith(
        .version_recommend("2.0"),
        "Bioconductor version '2.0' is out-of-date"
    ))
})

test_that(".version_validity_online_check() works", {
    ## environment variable
    withr::with_options(list(BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS=NULL), {
        withr::with_envvar(c(BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS=NA), {
            expect_identical(.version_validity_online_check(), TRUE)
        })

        withr::with_envvar(c(BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS=TRUE), {
            expect_identical(.version_validity_online_check(), TRUE)
        })

        withr::with_envvar(c(BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS=FALSE), {
            suppressWarnings({
                expect_identical(.version_validity_online_check(), FALSE)
            })
        })
    })

    ## options
    withr::with_envvar(c(BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS=NA), {
        withr::with_options(list(BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS=NULL), {
            expect_identical(.version_validity_online_check(), TRUE)
        })
    })

    withr::with_options(list(BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS=TRUE), {
        expect_identical(.version_validity_online_check(), TRUE)

        ## prefer option to env
        withr::with_envvar(c(BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS=FALSE), {
            expect_identical(.version_validity_online_check(), TRUE)
        })
    })

    withr::with_options(list(BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS=FALSE), {
        expect_identical(.version_validity_online_check(), FALSE)

        ## prefer option to env
        withr::with_envvar(c(BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS=TRUE), {
            expect_identical(.version_validity_online_check(), FALSE)
        })
    })
})

test_that(".version_validity('devel') works", {
    .skip_if_misconfigured()
    skip_if_offline()
    devel <- .version_bioc("devel")
    R_version <- getRversion()[,1:2]
    map <- .version_map()
    R_ok <- map$R[map$Bioc == devel]
    if (identical(version(), devel) || R_version %in% R_ok) {
        expect_true(.version_validity("devel"))
    } else {
        test <- paste0("Bioconductor version '", devel, "' requires R version")
        expect_true(startsWith(.version_validity("devel"), test))
    }
})

test_that(".version_validity(...) works", {
    .skip_if_misconfigured()
    skip_if_offline()

    .version_validity <- BiocManager:::.version_validity

    .get_R_ver = function(ver = "4.3.0") {
        rver <- package_version(ver)
        class(rver) <- c("R_system_version", class(rver))
        rver
    }

    .ver_map <- data.frame(
        Bioc = package_version(list("4.0", "4.1", "4.1")),
        R = package_version(list("4.3", "4.4", "4.5")),
        BiocStatus = c("release", "devel", "future")
    )

    expect_true(
        .version_validity("4.0", .ver_map, .get_R_ver())
    )

    expect_match(
        .version_validity("4.1", .ver_map, .get_R_ver()),
        "BiocManager::install"
    )

    expect_match(
        .version_validity("4.1", .ver_map, .get_R_ver("4.5.0")),
        "R version is too.*new"
    )
})

test_that(".version_validity() and BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS work",{
    .skip_if_BiocVersion_not_available()
    withr::with_options(list(BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS=FALSE), {
        expect_match(
            .version_validity("1.2"),
            "unknown Bioconductor version '1.2'; .*"
        )
    })
})

test_that(".version_validate() and BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS work",{
    .skip_if_BiocVersion_not_available()
    withr::with_options(list(BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS=FALSE), {
        expect_error(
            .version_validate("1.2"),
            "unknown Bioconductor version '1.2'; .*"
        )
    })
})


test_that(".version_map_get() and BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS work",{
    withr::with_options(list(BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS=FALSE), {
        value <- .version_map_get()
        if ("BiocVersion" %in% rownames(inst_pkgs))
            expect_identical(packageVersion("BiocVersion")[, 1:2], value[1, 1])
        else
            expect_identical(value, .VERSION_MAP_SENTINEL)
    })
})

test_that(".version_map_get() falls back to http", {
    .VERSION_MAP$WARN_NO_ONLINE_CONFIG <- TRUE
    ## better test ideas welcome...
    url <- "https://httpbin.org/status/404"
    msgs <- list()
    result <- withCallingHandlers({
        .version_map_get(url)
    }, warning = function(w) {
        msgs <<- c(msgs, list(w))
        invokeRestart("muffleWarning")
    })
    ## did we generate warnings and eventually fail gracefully?
    expect_identical(length(msgs), 2L)
    expect_true(all(vapply(msgs, is, logical(1), "simpleWarning")))
    msgs <- vapply(msgs, conditionMessage, character(1))
    expect_identical(sum(grepl("https://httpbin.org", msgs)), 1L)
    expect_identical(sum(grepl("http://httpbin.org", msgs)), 1L)
    expect_identical(result, .VERSION_MAP_SENTINEL)
})

test_that(".version_map_get() works with default mirror", {
    skip_if_offline()
    map <- .version_map_get_online("https://bioconductor.org/config.yaml")
    expect_identical(.version_map_get(), map)
})

test_that(".version_map_get() works with custom mirror", {
    map <- .version_map_get_online("bioc-mirror/config.yaml")
    withr::with_options(list(BioC_mirror = "bioc-mirror"), {
        expect_identical(.version_map_get(), map)
    })
})

test_that(".version_map_get() falls back to default mirror", {
    skip_if_offline()
    map <- .version_map_get_online("https://bioconductor.org/config.yaml")
    withr::with_options(list(BioC_mirror = "bioc-mirror-does-not-exist"), {
        expect_identical(.version_map_get(), map)
    })
})

test_that("BiocVersion version matches with .version_map()", {
    .skip_if_misconfigured()
    skip_if_offline()

    if (!"BiocVersion" %in% rownames(installed.packages()))
        skip("BiocVersion not installed")

    R_version <- getRversion()
    bioc_version <- packageVersion("BiocVersion")[, 1:2]

    expect_version <-
        function(bioc_version, R_version)
    {
        map <- .version_map()
        map <- map[map$R == R_version[,1:2], ]
        failure_message <- paste0(
            "BiocVersion package version '", bioc_version, "' does not match ",
            "BiocManager::.version_map() '", paste(map$Bioc, collapse="', '"),
            "'. Check configuration."
        )
        expect(bioc_version %in% map$Bioc, failure_message)
    }
    expect_version(bioc_version, R_version)
})
