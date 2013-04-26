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

biocValid <-
    function(pkgs = installed.packages(lib.loc, priority=priority),
             lib.loc=NULL, priority="NA", type=getOption("pkgType"),
             filters=NULL, silent=FALSE, ..., fix=FALSE)
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

    valid <- (NROW(oldPkgs) == 0) && (NROW(tooNewPkgs) == 0)
    if (valid)
        return(valid)

    if (!silent) {
        result <- structure(list(oldPkgs=oldPkgs, tooNewPkgs = tooNewPkgs),
                            class="biocValid")
        print(result)
    }
    if (fix) {
        pkgs <- c(rownames(oldPkgs), rownames(tooNewPkgs))
        biocLite(pkgs, lib.loc=lib.loc, ...)
        .warning("updated or downgraded package(s) %s",
                 paste(.sQuote(pkgs), collapse=" "))
    } else {
        msg <- character()
        if (NROW(oldPkgs))
            msg <-
                c(msg, sprintf("%d package(s) out of date", NROW(oldPkgs)))
        if (NROW(tooNewPkgs))
            msg <-
                c(msg, sprintf("%d package(s) too new", NROW(tooNewPkgs)))
        .stop(paste(msg, collapse="; "))
    }

    invisible(valid)
}

print.biocValid <-
    function(x, ...)
{
    cat("\n* sessionInfo()\n\n")
    print(sessionInfo())
    cat("\n")
    if (NROW(x$oldPkgs)) {
        cat("* Out-of-date packages\n")
        print(x$oldPkgs)
        cat("\nupdate with biocLite()\n\n")
    }

    if (NROW(x$tooNewPkgs)) {
        cat("* Packages too new for Bioconductor version ",
            .sQuote(as.character(biocVersion())), "\n\n", sep="")
        print(x$tooNewPkgs)
        pkgs <- paste(.dQuote(rownames(x$tooNewPkgs)), collapse=", ")
        msg <- .msg(ifelse(NROW(x$tooNewPkgs) == 1L, "biocLite(%s)",
                           "biocLite(c(%s))"), pkgs)
        cat("\ndowngrade with ", msg, "\n\n", sep="")
    }
}
