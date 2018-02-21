source("http://bioconductor.org/biocLite.R")

workflowInstall <- function(pkg, ...)
{
    repos <- c(biocinstallRepos(),
               sprintf("https://bioconductor.org/packages/%s/workflows",
                       BiocInstaller::biocVersion()))

    install.packages(pkg, repos=repos, ...)
}
