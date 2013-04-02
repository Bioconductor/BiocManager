\name{biocValid}

\alias{biocValid}

\title{Validate installed package versions against biocLite versions.}

\description{

  Check that installed packages are consistent (neither out-of-date nor
  too new) with the version of R and Bioconductor in use, using
  \code{biocLite} for validation.

}

\usage{
biocValid(pkgs = installed.packages(lib.loc, priority = priority),
    lib.loc = NULL, priority = "NA", type = getOption("pkgType"),
    filters = NULL, silent = FALSE, ..., fix=FALSE)
}

\arguments{
  \item{pkgs}{A character list of package names for checking, or a
    matrix as returned by \code{\link{installed.packages}}.}

  \item{lib.loc}{The library location(s) of packages to be validated;
    see \code{\link{installed.packages}}.}

  \item{priority}{check validity of all, \dQuote{base}, or
    \dQuote{recommended} packages; see
    \code{\link{installed.packages}}.}

  \item{type}{The type of available package (e.g., binary, source) to
    check validity against; see \code{\link{available.packages}}.}

  \item{filters}{Filter available packages to check validity against; see
    \code{\link{available.packages}}.}

  \item{silent}{Report how packages are invalid (\code{silent=FALSE},
    default) and abort execution, or return a logical(1)
    (\code{silent=TRUE}) indicating the overall validity of installed
    packages.}

  \item{\dots}{Additional arguments, passed to \code{\link{biocLite}}
    when \code{fix=TRUE}.}

  \item{fix}{When \code{TRUE}, invoke \code{biocLite} to reinstall
    (update or downgrade, as appropriate) invalid packages.}
    
}

\details{
  
  This function compares the version of installed packages to the
  version of packages associated with the version of R and Bioconductor
  appropriate for the BiocInstaller package currently in use.

  Packages are reported as \sQuote{out-of-date} if a more recent version
  is available at the repositories specified by
  \code{biocinstallRepos()}. Usually, \code{biocLite()} is sufficient to
  update packages to their most recent version.

  Packages are reported as \sQuote{too new} if the installed version is
  more recent than the most recent available in the
  \code{biocinstallRepos()} repositories. It is possible to down-grade
  by re-installing a too new package \dQuote{PkgA} with
  \code{biocLite("PkgA")}. It is important for the user to understand
  how their installation became too new, and to avoid this in the
  future.

}

\value{
  \code{logical(1)} indicating overall validity of installed packages.
}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{\code{\link{biocLite}} to update installed packages.}

\examples{try(biocValid())}

\keyword{ environment }
