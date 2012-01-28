monograph_group <- function() 
{
    warning("'arrayMagic' package is no longer available")
    c("affycomp", "affydata", "affypdnn", "affyPLM", "ALL", "ALLMLL",
      "AmpAffyExample", "annaffy", "AnnBuilder", "annotate",
      "arrayQuality", "beta7", "Biobase", "bioDist", "Biostrings",
      "cMAP", "CoCiteStats", "convert", "e1071", "edd", "estrogen",
      "exactRankTests", "facsDorit", "factDesign", "gbm", "gcrma",
      "geneplotter", "golubEsets", "GOstats", "gpls", "graph",
      "hexbin", "hgu133a", "hgu133atagcdf", "hgu133acdf",
      "hgu133bcdf", "hgu2beta7", "hgu95av2", "hgu95av2cdf",
      "hgu95av2probe", "hopach", "hsahomology", "hu6800cdf",
      "hu6800probe", "humanLLMappings", "ipred", "KEGG", "KEGGSOAP",
      "kidpack", "limma", "locfit", "LogitBoost", "matchprobes",
      "mclust", "mlbench", "MLInterfaces", "multtest", "pamr",
      "prada", "PROcess", "ProData", "randomForest", "rat2302",
      "RbcBook1", "RBGL", "RColorBrewer", "RCurl", "Rgraphviz",
      "rrcov", "simpleaffy", "sma", "SpikeInSubset", "SSOAP",
      "statmod", "vsn", "XML", "xtable", "YEAST", "yeastExpData")
}

lite_group <- function()
{
    c("affy", "affydata", "affyPLM", "annaffy", "annotate", "Biobase",
      "biomaRt", "Biostrings", "DynDoc", "gcrma", "genefilter",
      "geneplotter", "hgu95av2.db", "limma", "marray", "multtest",
      "vsn", "xtable", "affyQCReport")
}

graph_group <- function()
{
    c("graph", "Rgraphviz", "RBGL")
}

all_group <- function()
{
    contribUrl <- paste(biocinstallRepos()['BioCsoft'], "src/contrib",
                        sep="/")
    pkglist = available.packages(contribUrl)
    pkgs = rownames(pkglist)
}

RBioinf_group <- function()
{
    c(lite_group(), graph_group(), "RBioinf", "BiocCaseStudies",
      "XML", "RCurl", "biomaRt", "GEOquery", "KEGG", "KEGGSOAP",
      "hgu95av2", "hgu95av2probe", "hgu95av2cdf", "human.db0",
      "BSgenome.Hsapiens.UCSC.hg18")
}

biocases_group <- function()
{
    c(lite_group(), graph_group(), "ALL", "apComplex", "bioDist",
      "BiocCaseStudies", "biocGraph", "biomaRt", "CCl4", "CLL",
      "Category", "class", "convert", "GO.db", "GOstats", "GSEABase",
      "hgu133a.db", "hgu95av2cdf", "hgu95av2probe", "hopach",
      "KEGG.db", "kohonen", "lattice", "latticeExtra", "MASS",
      "matchprobes", "MLInterfaces", "org.Hs.eg.db", "ppiStats",
      "randomForest", "RColorBrewer", "Rintact", "sma", "weaver",
      "yeastExpData")
}
