\name{biocVersion}
\alias{biocVersion}
\title{Bioconductor version}
\description{

  This function reports the Bioconductor version in use, as determined
  by the BiocInstaller package.

}
\usage{
biocVersion()
}

\value{
  \code{package_version} representing the Bioconductor version in use.
}
\seealso{

  \code{\link{biocLite}} Installs/updates Bioconductor/CRAN packages.

  \code{\link{BiocUpgrade}} Upgrading to new versions.
}
\examples{
biocVersion()
}
\keyword{environment}
