workflowInstall <- function(pkg, ...)
{
    repos <- c(installRepos(),
               sprintf("https://bioconductor.org/packages/%s/workflows",
                       Bioconductor::biocVersion()))

    install.packages(pkg, repos=repos, ...)
}
