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
  
  Should only be used with a release (or patched) version of R,
  freshly installed.

}
\usage{
useDevel(devel=TRUE)
}
\arguments{
\item{devel}{Whether to look in the devel (TRUE) or release
   (FALSE) repositories in subsequent invocations of
   \code{\link{biocLite}} and \code{\link{biocinstallRepos}}.}  
}
\details{

  With R going to a yearly release schedule and Bioconductor keeping its
  twice-yearly release schedule, the same version of R (3.2) can be used
  with two different versions of Bioconductor (3.1, release, and 3.2, devel).
  The version number of the BiocInstaller package is what is used to determine
  whether to download packages from the BioC 3.1 or 3.2 repositories.
  In keeping with Bioconductor versioning conventions, if the middle number
  (y in x.y.z) is even, the package is part of a release version; if odd, it's
  part of a devel version.
  
  By default, when BiocInstaller is first installed on R-3.2, it will be
  set up to download BioC 3.1 packages. 
  
  If you want to change this, you can run the \code{\link{useDevel}}
  function. With argument \code{TRUE} (the default), it will download
  the devel version of BiocInstaller and subsequently all packages
  downloaded with \code{\link{biocLite}} will be from the BioC 3.2
  (devel) repository. You should run \code{\link{useDevel}} only once.
  
  It is possible to keep BioC 3.1 and 3.2 libraries separate, within
  the same installation of R.
  
  The trick is to use the \code{R_LIBS_USER} environment variable.
  First, create two separate directories for your BioC release
  and devel packages. Suggested directory names are as follows:
  
  Linux:
  
  ~/R/x86_64-unknown-linux-gnu-library/3.2-bioc-release
  
  ~/R/x86_64-unknown-linux-gnu-library/3.2-bioc-devel

  Mac OS:
  
  ~/Library/R/3.2-bioc-release/library
  
  ~/Library/R/3.2-bioc-devel/library

  Windows:
  
  C:\\Users\\YOUR_USER_NAME\\Documents\\R\\win-library\\3.2-bioc-release
  
  C:\\Users\\YOUR_USER_NAME\\Documents\\R\\win-library\\3.2-bioc-devel
  
  (change YOUR_USER_NAME to your user name)
  

  You can then invoke "R for bioc-devel" or "R for bioc-release" from
  the command line as follows:

  Linux:
  
  R_LIBS_USER=~/R/x86_64-unknown-linux-gnu-library/3.2-bioc-release R
  
  R_LIBS_USER=~/R/x86_64-unknown-linux-gnu-library/3.2-bioc-devel R
  
  
  Mac OS X:
  
  R_LIBS_USER=~~/Library/R/3.2-bioc-release/library R
  R_LIBS_USER=~~/Library/R/3.2-bioc-devel/library R

  Windows:
  
  cmd /C "set R_LIBS_USER=C:\\Users\\YOUR_USER_NAME\\Documents\\R\\win-library\\3.2-bioc-release &&  R"
  
  cmd /C "set R_LIBS_USER=C:\\Users\\YOUR_USER_NAME\\Documents\\R\\win-library\\3.2-bioc-devel &&  R"
  
  (Note: this assumes that R.exe is in your PATH.)


  If you launch R in this way and then invoke \code{\link{.libPaths}},
  you'll see that the first item is your special release or
  devel directory. Packages will be installed to that directory and that
  is the first place that \code{\link{library}} will look for them. 
  \code{\link{biocLite}}, \code{\link{install.packages}},
  \code{\link{update.packages}} and friends all respect this
  setting.


  On Linux and Mac OS X, you can create a bash alias to save typing. Add the
  following to your ~/bash_profile:
  

  Linux
  
  alias Rdevel='R_LIBS_USER=~/R/x86_64-unknown-linux-gnu-library/3.2-bioc-devel R'

  alias Rrelease='R_LIBS_USER=~/R/x86_64-unknown-linux-gnu-library/3.2-bioc-release R'
  
  Mac OS X
  
  alias Rdevel='R_LIBS_USER=~/Library/R/3.2-bioc-devel/library R'
  alias Rrelease='R_LIBS_USER=~/Library/R/3.2-bioc-release/library R'

  
  You can then invoke these from the command line as
  
  Rdevel
  
  ...and...
  
  Rrelease


  On Windows, you can create two shortcuts, one for devel and one for
  release. Go to My Computer and navigate to a directory that is in your
  PATH. Then right-click and choose New->Shortcut.
  
  in the "type the location of the item" box, put:
  
  cmd /C "set R_LIBS_USER=C:\\Users\\YOUR_USER_NAME\\Documents\\R\\win-library\\3.2-bioc-release &&  R"
  
  ...for release and
  
  cmd /C "set R_LIBS_USER=C:\\Users\\YOUR_USER_NAME\\Documents\\R\\win-library\\3.0-bioc-devel &&  R"

  ...for devel.

  (again, it's assumed R.exe is in your PATH)

  Click "Next".

  In the "Type a name for this shortcut" box, type

  Rdevel

  or

  Rrelease
  
  You can invoke these from the command line as
  
  Rdevel.lnk
  
  ...and...
  
  Rrelease.lnk
  
  (You must type in the .lnk extension.)
  
  Because \code{R_LIBS_USER} is an environment variable, its value should be
  inherited by any subprocesses started by R, so they should do the
  right thing as well.
  
  

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
