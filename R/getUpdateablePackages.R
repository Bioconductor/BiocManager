filterPackagesToUpdate <-
    function(regexes, pkgs)
{
    regex = paste(regexes, collapse="|")
    hits <- grep(regex, pkgs[,"Package"], invert=TRUE)
    pkgs[hits, , drop=FALSE]
}

getUpdatablePackages <-
    function(pkgs)
{
    all <- file.access(pkgs[,"LibPath"], 2)
    nonUpdateable <- pkgs[all == -1, "Package"]
    updateable <- pkgs[all == 0, , drop=FALSE]
    if (length(nonUpdateable))
    {
        pkgList <- paste(nonUpdateable, collapse="', '")
        fmt <- "installed directory not writeable, cannot offer to update old packages '%s'"
        .warning(fmt, pkgList, call.=FALSE)
    }
    updateable
}
