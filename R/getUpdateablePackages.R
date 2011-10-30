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
        txt <- sprintf("'%s' cannot be updated, installed directory '%s'
                        not writeable",
                       nonUpdateable, pkgs[nonUpdateable, "LibPath"])
        .warning(txt, call.=FALSE)
    }
    updateable
}
