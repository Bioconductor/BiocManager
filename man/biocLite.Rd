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

  \item{ask}{\code{logical(1)} indicating whether to prompt user before
    installed packages are updated, or the character string 'graphics',
    which brings up a widget for choosing which packages to update.
    Passed to \code{\link{update.packages}}.}

  \item{...}{Additional arguments passed to \code{\link{install.packages}}.}

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

## install a BiocInstaller package, but don't update all installed
## packages as well:
biocLite("GenomicRanges", suppressUpdates=TRUE)

## Install default packages, but do not update any package whose name
## starts with "org." or "BSgenome."
biocLite(suppressUpdates=c("^org\\.", "^BSgenome\\."))

## install a package from source:
biocLite("IRanges", type="source")

}
## Show the Bioconductor and CRAN repositories that will be used to
## install/update packages.
biocinstallRepos()
}
\keyword{environment}
