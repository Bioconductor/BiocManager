.onAttach <-
    function(libname, pkgname)
{
    .message("Bioconductor version %s (package version %s), ?install for help",
             version(), packageVersion("Bioconductor"))
     Rversion <- getRversion()
     if (IS_END_OF_LIFE) {
         if (IS_UPGRADEABLE)
             .message('
                 A newer version of Bioconductor is available for
                 this version of R, see vignettes(package="Bioconductor")
                 for help
             ')
         else if (Rversion > R_VERSION_MAX)
             .warning("Bioconductor version %s is too old for R version %s;
                 see https://bioconductor.org/install/#troubleshoot-biocinstaller",
                 BIOC_VERSION, Rversion)
         else
             .message("A new version of Bioconductor is available after
                       installing the most recent version of R; see
                       http://bioconductor.org/install")
     } else if (Rversion > R_VERSION_MAX) {
         if (Rversion >= NEXT_R_DEVEL_VERSION)
             .message("Bioconductor does not yet support R version %s", Rversion)
         else
             .warning("Bioconductor version %s is too old for R version %s;
                 see https://bioconductor.org/install/#troubleshoot-biocinstaller",
                 BIOC_VERSION, Rversion)
     }

}
