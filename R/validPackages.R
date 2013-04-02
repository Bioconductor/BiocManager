.tooNewPkgs <-
    function(instPkgs, availPkgs)
{
    idx <- rownames(availPkgs) %in% rownames(instPkgs)
    vers <- availPkgs[idx, "Version"]
    idx <- package_version(vers) <
        package_version(instPkgs[names(vers), "Version"])
    tooNew <- names(vers)[idx]
    instPkgs[tooNew, c("Version", "LibPath"), drop=FALSE]
}

validPackages <-
    function(pkgs = installed.packages(lib.loc, priority=priority),
             lib.loc=NULL, priority="NA", type=getOption("pkgType"),
             filters=NULL, silent=FALSE)
{
    if (!is.matrix(pkgs)) {
        if (is.character(pkgs))
            pkgs <- installed.packages(pkgs, lib.loc=lib.loc)
        else
            .stop("'pkgs' must be a character vector of package names,
                   or a matrix like that returned by 'installed.packages()'")
    }
    repos <- biocinstallRepos()
    contribUrl <- contrib.url(repos, type=type)

    availPkgs <- available.packages(contribUrl, type=type, filters=filters)
    oldPkgs <- old.packages(lib.loc, repos=biocinstallRepos(),
        instPkgs=pkgs, available=availPkgs, checkBuilt=TRUE,
        type=type)
    tooNewPkgs <- .tooNewPkgs(pkgs, availPkgs)

    result <- structure(list(oldPkgs=oldPkgs, tooNewPkgs = tooNewPkgs),
                        class="validPackages")
    valid <- (NROW(oldPkgs) == 0) && (NROW(tooNewPkgs) == 0)
    if (!valid && !silent) {
        print(result)
        .stop("some packages are not valid")
    }
    valid
}

print.validPackages <-
    function(x, ...)
{
    if (NROW(x$oldPkgs)) {
        cat("\n* Out-of-date packages\n")
        print(x$oldPkgs)
        cat("\nupdate with biocLite()\n")
    }

    if (NROW(x$tooNewPkgs)) {
        cat("\n* Packages too new for Bioconductor version ",
            sQuote(as.character(biocVersion())), "\n\n", sep="")
        print(x$tooNewPkgs)
        pkgs <- paste(dQuote(rownames(x$tooNewPkgs)), collapse=", ")
        msg <- .msg(ifelse(length(x$tooNewPkgs) == 1L, "biocLite(%s)",
                           "biocLite(c(%s))"), pkgs)
        cat("\ndowngrade with ", msg, "\n", sep="")
    }

    cat("\n* sessionInfo()\n\n")
    print(sessionInfo())
    cat("\n")
}
