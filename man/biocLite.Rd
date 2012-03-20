\name{biocLite}
\alias{biocLite}
\title{

  Install or update Bioconductor and CRAN packages

}
\description{

  \code{biocLite} installs or updates Bioconductor and CRAN packages,
  ensuring that packages from the appropriate version of Bioconductor
  are installed, and that all packages remain up to date.

}
\usage{
biocLite (pkgs=c("Biobase","IRanges","AnnotationDbi"),  
  suppressUpdates=FALSE,
  suppressAutoUpdate=FALSE,
  siteRepos=character(),
  ask=TRUE, ...)
}
\arguments{

  \item{pkgs}{\code{character()} of package names to install or update.
    A value of \code{character(0)} and \code{suppressUpdates=FALSE}
    updates packages without installing new ones.}

  \item{suppressUpdates}{\code{logical(1)} indicating whether to
    suppress automatic updating of all installed packages, or
    \code{character()} of regular expressions specifying which packages
    to NOT automatically update.}

  \item{suppressAutoUpdate}{\code{logical(1)} indicating whether the
    \code{BiocInstaller} package updates itself.}

  \item{siteRepos}{\code{character()} representing an additional
    repository in which to look for packages to install. This 
    repository will be prepended to the default repositories
    (which you can see with \code{\link{biocinstallRepos}}).}

  \item{ask}{\code{logical(1)} indicating whether to prompt user before
    installed packages are updated, or the character string 'graphics',
    which brings up a widget for choosing which packages to update.
    If TRUE, user can choose whether to update all outdated packages
    without further prompting, to pick and choose packages to update,
    or to cancel updating (in a non-interactive session, no packages
    will be updated). Otherwise, the value is passed to
    \code{\link{update.packages}}.} 

  \item{...}{Additional arguments. \code{lib.loc} is passed to
    \code{\link{old.packages}} (used to determine the library location
    of installed packages to be updated). \code{lib} is passed to
    \code{\link{install.packages}} (used to determine the library
    location where \code{pkgs} are to be installed).}

}
\details{

  The \code{biocLite()} function is generally used after sourcing the
  file \code{biocLite.R}. This will install the \code{BiocInstaller} package
  if it is not already installed. See example section for more information.

}

\value{
  \code{biocLite()} returns the \code{pkgs} argument, invisibly.
}
\seealso{

  \code{\link{biocinstallRepos}} returns the Bioconductor and CRAN
  repositories used by \code{biocLite}.

  \code{\link{install.packages}} installs the packages themselves.

  \code{\link{update.packages}} updates all installed packages.

  \code{\link{chooseBioCmirror}} lets you choose from a list of all
  public Bioconductor mirror URLs.

  \code{\link{chooseCRANmirror}} lets you choose from a list of all
  public CRAN mirror URLs.

  \code{\link{monograph_group}}, \code{\link{RBioinf_group}} and
  \code{\link{biocases_group}} return package names associated 
  with Bioconductor publications.
  
  \code{\link{all_group}} returns the names of all Bioconductor
  software packages.

}

\examples{
\dontrun{
## Change default Bioconductor and CRAN mirrors
chooseBioCmirror()
chooseCRANmirror()


## If you don't have the BiocInstaller package installed, you can 
## quickly install and load it as follows:
source("http://bioconductor.org/biocLite.R")


## The most recent version of the BiocInstaller package is now loaded.
## No need to load it with library().
biocLite() # installs default packages


## Now install a CRAN package:
biocLite("survival")

## install a Bioconductor package, but don't update all installed
## packages as well:
biocLite("GenomicRanges", suppressUpdates=TRUE)

## Install default packages, but do not update any package whose name
## starts with "org." or "BSgenome."
biocLite(suppressUpdates=c("^org\\.", "^BSgenome\\."))

## install a package from source:
biocLite("IRanges", type="source")

## don't install any packages, but update installed packages
biocLite(character(0), ask=FALSE)

## install all Bioconductor software packages
biocLite(all_group())

}
## Show the Bioconductor and CRAN repositories that will be used to
## install/update packages.
biocinstallRepos()
}
\keyword{environment}
