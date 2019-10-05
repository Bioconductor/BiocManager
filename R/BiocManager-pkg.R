#' @importFrom utils packageVersion contrib.url head
#'     installed.packages sessionInfo tail
NULL

#' Install or update Bioconductor, CRAN, or GitHub packages
#'
#' This package provides tools for managing _Bioconductor_ and other
#' packages in a manner consistent with _Bioconductor_'s package
#' versioning and release system.
#'
#' @details Main functions are as follows; additional help is
#'     available for each function, e.g., `?BiocManager::version`.
#'
#' \describe{
#'
#'   \item{`BiocManager::install()`}{Install or update packages from
#'     _Bioconductor_, CRAN, and GitHub.}
#'
#'   \item{`BiocManager::version()`}{Report the version of
#'     _Bioconductor_ in use.}
#'
#'   \item{`BiocManager::available()`}{Return a `character()` vector
#'     of package names available (at `BiocManager::repositories()`)
#'     for installation.}
#'
#'   \item{`BiocManager::valid()`}{Determine whether installed packages
#'     are from the same version of _Bioconductor_.}
#'
#'   \item{`BiocManager::repositories()`}{_Bioconductor_ and other
#'     repository URLs to discover packages for installation.}
#'
#' }
#'
#'     The version of _Bioconductor_ in use is determined by the
#'     installed version of a second package, BiocVersion. BiocVersion
#'     is installed automatically during first use of
#'     `BiocManager::install()`. If BiocVersion has not yet been
#'     installed, the version is determined by code in base R.
#'
#' @md
#' @name BiocManager-pkg
#' @aliases BiocManager
#' @docType package
#'
#' @examples
#' R.version.string
#' packageVersion("BiocManager")
#' if ("BiocVersion" %in% rownames(installed.packages()))
#'     packageVersion("BiocVersion")
#' BiocManager::version()
"_PACKAGE"
