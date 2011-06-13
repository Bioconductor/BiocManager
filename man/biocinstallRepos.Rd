\name{biocinstallRepos}
\alias{biocinstallRepos}
\title{

  Display current Bioconductor and CRAN repositories.

}
\description{

  Displays the URLs of the repositories used by \code{\link{biocLite}}
  to install Bioconductor and CRAN packages.

}
\usage{
biocinstallRepos()
}
\value{
  Named \code{character()} of repositories.
}
\seealso{
  \code{\link{biocLite}} Installs/updates Bioconductor/CRAN packages.

  \code{\link{install.packages}} installs the packages themselves.

  \code{\link{chooseBioCmirror}} lets you choose from a list of all
  public Bioconductor mirror URLs.

  \code{\link{chooseCRANmirror}} lets you choose from a list of all
  public CRAN mirror URLs.
}
\examples{
biocinstallRepos()

## Choose mirrors
\dontrun{
chooseCRANmirror()
chooseBioCmirror()
}
}
\keyword{environment}
