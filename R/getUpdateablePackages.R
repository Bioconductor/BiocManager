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
        txt <- "package update failed, installed directory not writeable: '%s'"
        .warning(txt, paste(nonUpdateable, collapse="' '"), call.=FALSE)
    }
    updateable
}
