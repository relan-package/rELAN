
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rELAN

<!-- badges: start -->
<!-- badges: end -->

## Overview

rELAN provides a tool to import ELAN files (.eaf), which are generated
by the annotation software ELAN, directly into R as `data.frame`s.

## Installation

You can install rELAN with:

``` r
# install.packages("devtools")
devtools::install_github("relan-package/rELAN")
```

## Usage

Depending on your working directory, the first argument is the file
name, or path and file name, as a string. To calculate the duration and
time slots of non-alignable annotations set
`distribute_duration_among_children = TRUE`. `wide_format = TRUE`
returns a wide `data.frame` where each tier has its own column and rows
are merged, so `ANNOTATION_VALUE`s replace `NA`s. This will also have
fewer data than the original extracted `data.frame`.

``` r
library(rELAN)

exctract_annotations("ELAN_files/frog_story.eaf")

extract_annotations("ELAN_files/pear_story.eaf", distribute_duration_among_children = TRUE,
                    wide_format = TRUE)
```

## Why rELAN / more Information

So far, one of the most common ways to import the annotation data into
R, was by a two step process. First, you needed to use ELAN’s function
to export the ELAN file, which is written in XML, as a tab-delimited
text, for instance. This exported file could then be imported into R as
a `data.frame`. Thus, using rELAN has three advantages:

1.  The import is a single step, which is more economical in general.
2.  If you need to add, change, or delete annotations, you only need to
    modify the ELAN file and import it into R again instead of changing
    the ELAN file, the tab-delimited file, and importing it into R.
3.  Importing with rELAN delivers you every information of the ELAN file
    concerning the annotations. The tab-delimited text file only
    contains limited data relating to the annotations.

The default of `extract_annotations()` produces a long `data.frame` with
all data relating to the annotations. However, you can get a
`data.frame` with fewer data, where each tier has its own column, by
using the argument `wide_format = TRUE`.
