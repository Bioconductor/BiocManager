\name{Package Groups}
\alias{biocases_group}
\alias{RBioinf_group}
\alias{monograph_group}
\alias{all_group}
\title{

  Convenience functions to return package names associated with Bioconductor publications.

}
\description{

  Returns character vectors of packages associated with Bioconductor publications,
  which can then be passed to \code{\link{biocLite}()}

}
\usage{
monograph_group()
RBioinf_group()
biocases_group()
all_group()
}
\value{
  \code{character()} of package names.
}
\seealso{
  \code{\link{biocLite}} Installs/updates Bioconductor/CRAN packages.

  \code{\link{biocinstallRepos}} returns the Bioconductor and CRAN
  repositories used by \code{biocLite}.

  \code{\link{install.packages}} installs the packages themselves.

  \code{\link{chooseBioCmirror}} lets you choose from a list of all
  public Bioconductor mirror URLs.

  \code{\link{chooseCRANmirror}} lets you choose from a list of all
  public CRAN mirror URLs.
}
\examples{

## Get the names of packages used in the book
## "Bioconductor Case Studies":
biocases_group()

## Get the names of packages used in the book
## "R Programming for Bioinformatics":
RBioinf_group()

## Get the names of packages used in the monograph
## "Bioinformatics and Computational Biology Solutions
## Using R and Bioconductor":
monograph_group()

## Get the names of all Bioconductor software packages
all_group()
}
\keyword{environment}
