
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cytoBatchNorm

<!-- badges: start -->
<!-- badges: end -->

The goal of cytoBatchNorm is to normalize cytometry data using a control
sample that is labeled and measured in each batch.

## Installation

cytoBatchNorm is not available on [CRAN](https://CRAN.R-project.org).

### Simplest way to install

``` r
# Install BiocConductor manager if not already done
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
# Install cytoBatchNorm
BiocManager::install(c("i-cyto/cytoBatchNorm"))
```

### Alternative installation

First, be sure to install
[flowCore](https://www.bioconductor.org/packages/release/bioc/html/flowCore.html)
from [Bioconductor](https://www.bioconductor.org).

``` r
# Only once per R and BiocConductor installation
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!require("flowCore", quietly = TRUE)) BiocManager::install(c("flowCore"))
```

Then, you can install the latest version of cytoBatchNorm from
[GitHub](https://github.com/) with:

``` r
# the devtools package must be present
# if not, install devtools with the following command
# install.packages("devtools")
devtools::install_github("i-cyto/cytoBatchNorm")
```

## Usages

### Graphical User Interface

For an interactive use, type the following commands:

``` r
# load the library, once per R session
library(cytoBatchNorm)
# execute the following command for each dataset or set of parameters
cytoBatchNormGUI(roots = c(PRJ="/path/to/results", FCS="/path/to/FCS_files"))
# you have to set the paths to results and FCS files
# if you don't know how to retrieve these 2 information, then for each path,
# use the command to select any file in the target path:
# gsub("\\", "/", dirname(file.choose()), fixed = TRUE)
```

### Writing scripts

For using the API, look at vignettes.
