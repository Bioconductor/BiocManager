\name{useDevel}
\alias{useDevel}
\title{

  Get the 'devel' version of the BiocInstaller package.

}
\description{

  Downloads the 'devel' version of the BiocInstaller package
  so that all subsequent invocations of \code{\link{biocLite}}
  and \code{\link{biocinstallRepos}} use the devel repositories.
  
  Displays the URLs of the repositories used by \code{\link{biocLite}}
  to install Bioconductor and CRAN packages.

}
\usage{
useDevel(devel=TRUE)
}
\arguments{
\item{devel}{Whether to look in the devel (TRUE) or release
   (FALSE) repositories in subsequent invocations of
   \code{\link{biocLite}} and \code{\link{biocinstallRepos}}.}  
}
\value{
  Invisible NULL.
}
\seealso{
  \code{\link{biocinstallRepos}} returns the Bioconductor and CRAN
  repositories used by \code{biocLite}.

  \code{\link{biocLite}} Installs/updates Bioconductor/CRAN packages.

  \code{\link{install.packages}} installs the packages themselves.

  \code{\link{chooseBioCmirror}} lets you choose from a list of all
  public Bioconductor mirror URLs.

  \code{\link{chooseCRANmirror}} lets you choose from a list of all
  public CRAN mirror URLs.
}
\examples{
\dontrun{
useDevel()
}
}
\keyword{environment}
