## test_that("inet_readChar() fails on timeout", {
##     ## readability
##     mock <- testthat::with_mock
##     envvar <- withr::with_envvar
##     readMock <- function(...) stop("mock error")

##     expect_false(getOption("BIOCMANAGER_CRANCHECK_BEHAVIOR"))
##     expect_false(BiocManager:::.is_CRAN_check())

##     cat("BIOCMANAGER_CRANCHECK_BEHAVIOR: ", getOption("BIOCMANAGER_CRANCHECK_BEHAVIOR"), "\n")
##     cat(paste(names(Sys.getenv()), collapse="\n"), "\n")
##     if (!any(grepl("_CRAN_", names(Sys.getenv()))))
##         ## expect error not under --as-cran
##         expect_error(mock(readChar = readMock, available()), "mock error")

##     ## under CRAN (--as-cran) expect to find environmental variable(s)
##     ## "*_CRAN_*" and to generate message instead of error
##     expect_message({
##         envvar(
##             c(FAUX_CRAN_VAR = "any"),
##             mock(readChar = readMock, available())
##         )
##     }, "mock error")

## })
