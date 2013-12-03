biocUpdatePackages <-
    function(pkgs, dependencies = NA, repos = biocinstallRepos(), ...)
{
    if (identical(dependencies, NA))
        dependencies <- c("Depends", "Imports", "LinkingTo")
    avail <- available.packages(contriburl=contrib.url(repos))
    deps <- avail[pkgs, dependencies, drop=FALSE]
    deps <- unlist(apply(deps, 1, utils:::.clean_up_dependencies))
    deps <- unique(c(pkgs, deps))
    deps <- deps[deps %in% rownames(avail)]
    update.packages(oldPkgs=deps, repos=repos, ...)
} 
