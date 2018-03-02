workflowInstall <- function(pkg, ...)
{
    repos <- c(biocinstallRepos(),
               sprintf("https://bioconductor.org/packages/%s/workflows",
                       Bioconductor::biocVersion()))

    install.packages(pkg, repos=repos, ...)
}
