filterPackagesToUpdate <-
    function(regexes, pkgs)
{
    regex = paste(regexes, collapse="|")
    hits <- grep(regex, pkgs[,"Package"], invert=TRUE)
    pkgs[hits, , drop=FALSE]
}

