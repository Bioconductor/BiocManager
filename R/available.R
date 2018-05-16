#' Discover packages available for installation.
#'
#' @param pattern character(1) pattern to filter (via
#'     `grep(pattern=...)`) available packages; the filter is not case
#'     sensitive.
#'
#' @param include_installed logical(1) When `TRUE`, include installed
#'     package in list of available packages; when `FALSE`, exclude
#'     installed packages.
#'
#' @return `character()` vector of package names available for
#'     installation.
#'
#' @examples
#' avail <- available()
#' length(avail)
#'
#' available("bs.*hsapiens")   ## case-insensitve regular expression filter
#'
#' @md
#' @export
available <-
    function(pattern = "", include_installed = TRUE)
{
    stopifnot(
        is.character(pattern), length(pattern) == 1L, !is.na(pattern),
        is.logical(include_installed), length(include_installed) == 1L,
        !is.na(include_installed)
    )
    repos <- repositories()
    answer <- rownames(available.packages(repos = repos))
    answer <- grep(pattern, answer, value = TRUE, ignore.case = TRUE)
    if (!include_installed)
        answer <- setdiff(answer, rownames(installed.packages()))
    answer
}
