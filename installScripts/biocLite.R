## Mirrors: uncomment the following and change to your favorite CRAN mirror
## if you don't want to use the default (cran.fhcrc.org, Seattle, USA).
## options("repos" = "http://cran.fhcrc.org")

## Mirrors: uncomment the following and change to your favorite Bioconductor
## mirror, if you don't want to use the default (www.bioconductor.org,
## Seattle, USA)
## options("BioC_mirror" = "http://www.bioconductor.org")

local({
    vers <- getRversion()
    biocVers <-
        tryCatch(tools:::.BioC_version_associated_with_R_version,
                 error=function(...) numeric_version(0.0))
    if (vers > "2.13" && biocVers > "2.8") {
        if (!suppressWarnings(require("BiocInstaller", quietly=TRUE))) {
            a <- NULL
            p <- file.path(Sys.getenv("HOME"), ".R", "repositories")
            if (file.exists(p)) {
                a <- tools:::.read_repositories(p)
                if (!"BioCsoft" %in% rownames(a)) 
                    a <- NULL
            }
            if (is.null(a)) {
                p <- file.path(R.home("etc"), "repositories")
                a <- tools:::.read_repositories(p)
            }
            utils::install.packages("BiocInstaller", repos=a["BioCsoft", "URL"])
            if (!suppressWarnings(require("BiocInstaller", quietly=TRUE))) {
                stop("use 'install.packages(\"BiocInstaller\")' before\n",
                      "  'source(\"http://bioconductor.org/biocLite.R\")'")
            }
        }
    } else {
        source("http://bioconductor.org/getBioC.R")
        biocLite <<-
            function(pkgs, groupName="lite", ...)
            {
                if (missing(pkgs))
                    biocinstall(groupName=groupName, ...)
                else
                    biocinstall(pkgs=pkgs, groupName=groupName, ...)
            }
    }
})
