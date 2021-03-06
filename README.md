
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/amcdavid/Genesee/workflows/R-CMD-check/badge.svg)](https://github.com/amcdavid/Genesee/actions)
<!-- badges: end -->

# Genesee

<!-- badges: start -->
<!-- badges: end -->

The goal of Genesee is to set up assay-specific analysis templates for
various transcriptomic modalities, including scRNAseq, scVDJ and bulk
RNAseq.

## Installation

You can install it from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("amcdavid/Genesee")
```

You will also need to install GeneseeSC and GeneseeBulk, which contain
the templates for this to be useful.

## Example

Initialize a project titled ‘title’ at
‘path/to/projects/investigator/title’ and copy down scRNA template
files.

``` r
library(Genesee)
library(GeneseeSC)
genesee_skeleton('path/to/projects', 'investigator', 'title', author = 'Andrew', project_type = 'scRNA')
```
