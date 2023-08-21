#' @importFrom utils packageVersion contrib.url head
#'     installed.packages sessionInfo tail
NULL

#' Install or update Bioconductor, CRAN, or GitHub packages
#'
#' This package provides tools for managing _Bioconductor_ and other
#' packages in a manner consistent with _Bioconductor_'s package
#' versioning and release system.
#'
#' @details
#'
#' Main functions are as follows; additional help is available for
#' each function, e.g., `?BiocManager::version`.
#'
#' - `BiocManager::install()`: Install or update packages from
#'   _Bioconductor_, CRAN, and GitHub.
#'
#' - `BiocManager::version()`: Report the version of _Bioconductor_ in
#'     use.
#'
#' - `BiocManager::available()`: Return a `character()` vector of
#'   package names available (at `BiocManager::repositories()`) for
#'   installation.
#'
#' - `BiocManager::valid()`: Determine whether installed packages are
#'     from the same version of _Bioconductor_.
#'
#' - `BiocManager::repositories()`: _Bioconductor_ and other
#'    repository URLs to discover packages for installation.
#'
#' The version of _Bioconductor_ in use is determined by the installed
#' version of a second package, BiocVersion. BiocVersion is installed
#' automatically during first use of `BiocManager::install()`. If
#' BiocVersion has not yet been installed, the version is determined
#' by code in base R.
#'
#' Options influencing package behavior (see `?options`, `?getOption`)
#' include:
#'
#' - `"repos"`, `"BiocManager.check_repositories"`: URLs of additional
#'   repositories for use by `BiocManger::install()`. See `?repositories`.
#'
#' - `"pkgType"`: The default type of packages to be downloaded and
#'   installed; see `?install.packages`.
#'
#' - `"timeout"`: The maximum time allowed for download of a single
#'   package, in seconds. _BiocManager_ increases this to 300 seconds
#'   to accommodate download of large BSgenome and other packages.
#'
#' System environment variables influencing package behavior include:
#'
#' - \env{BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS} advanced
#'   configuration to avoid _Bioconductor_ version checks. See
#'   `?install`.
#'
#' - \env{BIOCONDUCTOR_CONFIG_FILE} for offline use of BiocManager
#'   versioning functionality. See `?install`.
#'
#' - \env{BIOCONDUCTOR_USE_CONTAINER_REPOSITORY} opt out of binary package
#'   installations. See `?containerRepository`.
#'
#' - \env{BIOCMANAGER_CHECK_REPOSITORIES} silence messages regarding
#'   non-standard CRAN or Bioconductor repositories. See `?repositories`.
#'
#' - \env{BIOCMANAGER_SITE_REPOSITORY} configure a more permanent
#'   `site_repository` input to `repositories()`. See `?repositories`.
#'
#' @md
#' @aliases BiocManager
#'
#' @examples
#' R.version.string
#' packageVersion("BiocManager")
#' if (requireNamespace("BiocVersion", quietly = TRUE))
#'     packageVersion("BiocVersion")
#' BiocManager::version()
"_PACKAGE"
