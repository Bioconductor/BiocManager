\name{BiocUpgrade}
\alias{BiocUpgrade}
\title{

  Upgrade Bioconductor to the latest version available for this version
  of R

}
\description{

  Downloads the latest version of the BiocInstaller package,
  and upgrades all currently installed packages to the latest
  repositories \strong{for this version of R}.

  To upgrade, use: \preformatted{
    ## 'http' if 'https' unavailable
    source("https://bioconductor.org/biocLite.R")
    biocLite("BiocUpgrade")
  }

}

\seealso{
  \code{\link{biocLite}} Installs/updates Bioconductor/CRAN packages.

  \code{\link{chooseBioCmirror}} lets you choose from a list of all
  public Bioconductor mirror URLs.

  \code{\link{chooseCRANmirror}} lets you choose from a list of all
  public CRAN mirror URLs.

  \code{\link{biocinstallRepos}} returns the Bioconductor and CRAN
  repositories used by \code{biocLite}.

  \code{\link{install.packages}} installs the packages themselves.
}
\examples{
\dontrun{
source("https://bioconductor.org/biocLite.R")
biocLite("BiocUpgrade")
}
}
\keyword{environment}
