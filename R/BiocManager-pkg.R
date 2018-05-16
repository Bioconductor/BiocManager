#' @importFrom utils packageVersion available.packages contrib.url
#'     head install.packages installed.packages old.packages
#'     sessionInfo tail update.packages
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
#' \item{`BiocManager::valid()`}{Determine whether installed packages
#'     are from the same version of _Bioconductor_.}
#'
#'   \item{`BiocManager::repositories()`}{_Bioconductor_ and other
#'     repository URLs to discover packages for installation.}
#'
#' }
#'
#' @md
#' @name BiocManager-pkg
#' @aliases BiocManager
#' @docType package
"_PACKAGE"
