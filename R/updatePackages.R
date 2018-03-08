#'
#' Update previously installed Bioconductor or CRAN packages and their
#' dependencies.
#'
#'
#' Update previously installed Bioconductor and CRAN packages and their
#' dependencies. Use \code{\link{install}} to install new packages or to
#' update all out-of-date packages. Upgrading to a new Bioconductor release
#' requires additional steps; see \url{https://bioconductor.org/install}.
#'
#'
#' @param pkgs \code{character()} of package names to install or update.
#' @param dependencies \code{character()} describing out-of-date dependencies
#' that are also updated. Defaults to \code{c("Depends", "Imports",
#' "LinkingTo")} but can be a subset of \code{c("Depends", "Imports",
#' "LinkingTo", "Suggests", "Enhances")}.
#' @param repos \code{character()} of named repositories in which to look for
#' package updates, in the style of \code{installRepos()}.
#' @param \dots Additional arguments, passed to \code{\link{update.packages}}.
#' For example, \code{ask=FALSE} to avoid prompts to update individual
#' packages.
#' @return \sQuote{NULL}, invisibly.
#' @author Martin Morgan \url{mtmorgan@@fhcrc.org}
#' @seealso \code{\link{install}}
#' @keywords environment
#' @examples
#'
#' \dontrun{
#' updatePackages("GenomicRanges", ask=FALSE)
#' }
#'
#' @export updatePackages
updatePackages <-
    function(pkgs, dependencies = NA, repos = installRepos(), ...)
{
    if (identical(dependencies, NA))
        dependencies <- c("Depends", "Imports", "LinkingTo")
    avail <- available.packages(contriburl=contrib.url(repos))
    deps <- avail[pkgs, dependencies, drop=FALSE]
    deps <- unlist(apply(deps, 1, utils:::.clean_up_dependencies))
    deps <- unique(c(pkgs, deps))
    deps <- deps[deps %in% rownames(avail)]
    update.packages(oldPkgs=deps, repos=repos, ...)
}
