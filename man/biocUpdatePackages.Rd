\name{biocUpdatePackages}
\alias{biocUpdatePackages}

\title{

  Update previously installed Bioconductor or CRAN packages and their
  dependencies.

}
\description{

  Update previously installed Bioconductor and CRAN packages and their
  dependencies. Use \code{\link{biocLite}} to install new packages or to
  update all out-of-date packages. Upgrading to a new Bioconductor
  release requires additional steps; see
  \url{https://bioconductor.org/install}.

}
\usage{
biocUpdatePackages(pkgs, dependencies = NA, repos=biocinstallRepos(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{pkgs}{\code{character()} of package names to install or update.}

  \item{dependencies}{\code{character()} describing out-of-date
    dependencies that are also updated. Defaults to \code{c("Depends",
    "Imports", "LinkingTo")} but can be a subset of \code{c("Depends",
    "Imports", "LinkingTo", "Suggests", "Enhances")}.}

  \item{repos}{\code{character()} of named repositories in which to look
    for package updates, in the style of \code{biocinstallRepos()}.}

  \item{\dots}{Additional arguments, passed to
    \code{\link{update.packages}}. For example, \code{ask=FALSE} to
    avoid prompts to update individual packages.}

}

\value{\sQuote{NULL}, invisibly.}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{\code{\link{biocLite}}}

\examples{
\dontrun{
biocUpdatePackages("GenomicRanges", ask=FALSE)
}
}

\keyword{environment}
