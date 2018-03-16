workflowInstall <- function(pkg, ...)
{
    repos <- c(installRepos(),
               sprintf("https://bioconductor.org/packages/%s/workflows",
                       BiocVersion::version()))

    install.packages(pkg, repos=repos, ...)
}
