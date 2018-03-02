#' Bioconductor version
#'
#'
#' This function reports the Bioconductor version in use, as determined by the
#' BiocInstaller package.
#'
#'
#' @return \code{package_version} representing the Bioconductor version in use.
#' @seealso
#'
#' \code{\link{install}} Installs/updates Bioconductor/CRAN packages.
#'
#' @keywords environment
#' @examples
#'
#' biocVersion()
#'
#' @export biocVersion
biocVersion <- function() BIOC_VERSION
