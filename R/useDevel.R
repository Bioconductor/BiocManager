#'
#' Get the 'devel' version of the Bioconductor package.
#'
#'
#' Downloads the 'devel' version of the 'Bioconductor' package so that all
#' subsequent invocations of \code{\link{install}} and
#' \code{\link{installRepos}} use the devel repositories.
#'
#' Displays the URLs of the repositories used by \code{\link{install}} to
#' install Bioconductor project and R-project CRAN packages.
#'
#' Should only be used with a release (or patched) version of R, freshly
#' installed.
#'
#'
#' The Bioconductor project has a 'release' branch and a 'devel' branch.
#' The branch in use depends on the version of R and the version of the
#' Bioconductor package.
#'
#' \code{useDevel()} installs the correct version of the Bioconductor package
#' for use of the devel version of Bioconductor, provided it is supported by
#' the version of R in use.
#'
#' \code{isDevel()} returns TRUE when the version of Bioconductor in use
#' corresponds to the 'devel' version of the Bioconductor package.
#'
#' In more detail, the version number of the Bioconductor package determines
#' whether to download packages from the release or devel repositories of
#' Bioconductor. In keeping with Bioconductor versioning conventions, if the
#' middle number (y in x.y.z) is even, the package is part of a release
#' version; if odd, it's part of a devel version.
#'
#' By default, when the Bioconductor package is first installed and when the
#' version of R supports the current release version of Bioconductor,
#' The Bioconductor package will use the release repository.
#'
#' To change the version of the Bioconductor package to support the 'devel'
#' branch of Bioconductor, run \code{useDevel()}. With argument \code{TRUE}
#' (the default), it will download the devel version of Bioconductor and
#' subsequently all packages downloaded with \code{\link{biocDevel}} will be
#' from the devel repository. You should run \code{\link{useDevel}} only once.
#'
#' During release cycles where both the release and devel version of
#' Bioconductor use the same version of R, it is possible to use release and
#' devel versions of the Bioconductor package with the same installation of R.
#' To do this, use the \code{R_LIBS_USER} environment variable. First, create
#' two separate directories for your BioC release and devel packages. Suggested
#' directory names are as follows:
#'
#' Linux:
#' \preformatted{
#' ~/R/x86_64-unknown-linux-gnu-library/3.2-bioc-release
#'
#' ~/R/x86_64-unknown-linux-gnu-library/3.2-bioc-devel
#'
#' Mac OS:
#'
#' ~/Library/R/3.2-bioc-release/library
#'
#' ~/Library/R/3.2-bioc-devel/library
#'
#' Windows:
#'
#' C:\Users\YOUR_USER_NAME\Documents\R\win-library\3.2-bioc-release
#'
#' C:\Users\YOUR_USER_NAME\Documents\R\win-library\3.2-bioc-devel
#' }
#'
#' (change YOUR_USER_NAME to your user name)
#'
#' Invoke "R for bioc-devel" or "R for bioc-release" from the command line as
#' follows:
#'
#' Linux:
#' \preformatted{
#' R_LIBS_USER=~/R/x86_64-unknown-linux-gnu-library/3.2-bioc-release R
#'
#' R_LIBS_USER=~/R/x86_64-unknown-linux-gnu-library/3.2-bioc-devel R
#'
#' Mac OS X:
#'
#' R_LIBS_USER=~~/Library/R/3.2-bioc-release/library R
#' R_LIBS_USER=~~/Library/R/3.2-bioc-devel/library R
#'
#' Windows:
#'
#' cmd /C "set
#' R_LIBS_USER=C:\Users\YOUR_USER_NAME\Documents\R\win-library\3.2-bioc-release
#' && R"
#'
#' cmd /C "set
#' R_LIBS_USER=C:\Users\YOUR_USER_NAME\Documents\R\win-library\3.2-bioc-devel
#' && R"
#' }
#'
#' (Note: this assumes that R.exe is in your PATH.)
#'
#' If you launch R in this way and then invoke \code{\link{.libPaths}}, you'll
#' see that the first item is your special release or devel directory. Packages
#' will be installed to that directory and that is the first place that
#' \code{\link{library}} will look for them.  \code{\link{install}},
#' \code{\link{install.packages}}, \code{\link{update.packages}} and friends
#' all respect this setting.
#'
#' On Linux and Mac OS X, you can create a bash alias to save typing. Add the
#' following to your ~/bash_profile:
#'
#' Linux
#'
#' \preformatted{
#' alias
#' Rdevel='R_LIBS_USER=~/R/x86_64-unknown-linux-gnu-library/3.2-bioc-devel R'
#'
#' alias
#' Rrelease='R_LIBS_USER=~/R/x86_64-unknown-linux-gnu-library/3.2-bioc-release
#' R'
#'
#' Mac OS X
#'
#' alias Rdevel='R_LIBS_USER=~/Library/R/3.2-bioc-devel/library R'
#' alias Rrelease='R_LIBS_USER=~/Library/R/3.2-bioc-release/library R'
#'
#' }
#' You can then invoke these from the command line as
#'
#' Rdevel
#'
#' ...and...
#'
#' Rrelease
#'
#' On Windows, you can create two shortcuts, one for devel and one for release.
#' Go to My Computer and navigate to a directory that is in your PATH. Then
#' right-click and choose New->Shortcut.
#'
#' in the "type the location of the item" box, put:
#'
#' \preformatted{
#' cmd /C "set
#' R_LIBS_USER=C:\Users\YOUR_USER_NAME\Documents\R\win-library\3.2-bioc-release
#' && R"
#'
#' ...for release and
#'
#' cmd /C "set
#' R_LIBS_USER=C:\Users\YOUR_USER_NAME\Documents\R\win-library\3.0-bioc-devel
#' && R"
#' }
#'
#' ...for devel.
#'
#' (again, it's assumed R.exe is in your PATH)
#'
#' Click "Next".
#'
#' In the "Type a name for this shortcut" box, type
#'
#' Rdevel
#'
#' or
#'
#' Rrelease
#'
#' You can invoke these from the command line as
#'
#' Rdevel.lnk
#'
#' ...and...
#'
#' Rrelease.lnk
#'
#' (You must type in the .lnk extension.)
#'
#' Because \code{R_LIBS_USER} is an environment variable, its value should be
#' inherited by any subprocesses started by R, so they should do the right
#' thing as well.
#'
#' @aliases useDevel isDevel
#'
#' @return \code{useDevel()}: Invisible NULL.
#'
#' \code{isDevel()}: logical(1) TRUE or FALSE.
#' @seealso \code{\link{installRepos}} returns the Bioconductor and CRAN
#' repositories used by \code{install}.
#'
#' \code{\link{install}} Installs/updates Bioconductor/CRAN packages.
#'
#' \code{\link{install.packages}} installs the packages themselves.
#'
#' \code{\link{chooseBioCmirror}} lets you choose from a list of all public
#' Bioconductor mirror URLs.
#'
#' \code{\link{chooseCRANmirror}} lets you choose from a list of all public
#' CRAN mirror URLs.
#' @keywords environment
#' @examples
#'
#' isDevel()
#'
#' \dontrun{useDevel()}
#'
#' @export useDevel
useDevel <- function()
{
    if (isDevel())
        .stop("'devel' version already in use")

    BiocVersion:::.options$set("BioC_Version", "devel")
    UPGRADE_VERSION <- biocVersion()

    txt <- sprintf(
        "Upgrade all packages to Bioconductor version %s? [y/n]: ",
        UPGRADE_VERSION)
    answer <- .getAnswer(txt, allowed = c("y", "Y", "n", "N"))

    if ("y" == answer)
        .update(UPGRADE_VERSION)
    else
        BiocVersion:::.options$set("BioC_Version", "release")
}

#'
#' Use the release version of Bioconductor
#'
#' This function will modify the repository pointer to the release version
#' if you are using devel. It will then prompt a downgrade of all packages
#' in the \code{.libPaths()[1]}.
#'
#' @export
useRelease <- function()
{
    if (!isDevel())
        .stop("'release' version already in use")

    BiocVersion:::.options$set("BioC_Version", "release")
    DOWNGRADE_VERSION <- biocVersion()

    txt <- sprintf(
        "Downgrade all packages to Bioconductor version %s? [y/n]: ",
        DOWNGRADE_VERSION)

    answer <- .getAnswer(txt, allowed = c("y", "Y", "n", "N"))
    if ("y" == answer)
        .update(DOWNGRADE_VERSION)
    else
        BiocVersion:::.options$set("BioC_Version", "devel")
}

.update <-
    function(biocVersion)
{
    .dbg("using version %s", biocVersion)
    if (isDevel())
        biocDevel()
    else
        install()
}

#' @rdname useDevel
#' @export
isDevel <- function() {
    as.logical(BiocVersion::version()$minor %% 2)
}
