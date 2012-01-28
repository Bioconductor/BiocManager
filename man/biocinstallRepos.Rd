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
biocinstallRepos(siteRepos=character())
}
\arguments{
\item{siteRepos}{\code{character()} representing an additional
  repository in which to look for packages to install. This 
  repository will be prepended to the default repositories
  returned by the function.}

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
