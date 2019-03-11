# RNWIS

[![Travis-CI Build Status](https://travis-ci.org/jfisher-usgs/RNWIS.svg?branch=master)](https://travis-ci.org/jfisher-usgs/RNWIS)

## Deprecated

Development of this package has halted.
If you are interested in taking over maintainer status for the package, please email the author.

## Overview

The [R](https://www.r-project.org/) package **RNWIS** provides access to water-resources data stored on the
**N**ational **W**ater **I**nformation **S**ystem ([NWIS](https://waterdata.usgs.gov/nwis)).
Importing data requires read-only permissions for you local NWIS server; therefore,
the usefulness of this package may be limited to **U**.**S**. **G**eological **S**urvey (USGS) employees.
A **g**raphical **u**ser **i**nterface (GUI) is provided for data selection.

## Install

If R is not already installed on your computer, download and install the latest binary distribution from
[CRAN](https://cran.r-project.org/ "The Comprehensive R Archive Network").
Windows users should set R to operate as an **s**ingle-**d**ocument **i**nterface (SDI) application during installation
by choosing to customize the start-up options and specifying the SDI interface (not the default).

You can install the stable version of **RNWIS** from [GitHub](https://jfisher-usgs.github.io/R/),
and its dependencies from CRAN, using the following commands:

```r
repos <- c("https://jfisher-usgs.github.io/R", "https://cloud.r-project.org/")
install.packages("RNWIS", repos = repos)
```

Or use [**devtools**](https://CRAN.R-project.org/package=devtools) to install the development version.

```r
devtools::install_github("jfisher-usgs/RNWIS")
```

Guidance for setting up an ODBC connection to NWIS is provided on this
[website](https://github.com/USGS-R/WQ-Review/blob/master/README.md).

## Run

R must be run in 32-bit mode to use the ODBC driver.
Load **RNWIS** in the current R session and activate the main GUI

```r
library(RNWIS)
```

## Bugs

Please consider reporting bugs and asking questions on the [Issues page](https://github.com/jfisher-usgs/RNWIS/issues).

## Disclaimer

This software has been approved for release by the U.S. Geological Survey
(USGS). Although the software has been subjected to rigorous review, the USGS
reserves the right to update the software as needed pursuant to further analysis
and review. No warranty, expressed or implied, is made by the USGS or the U.S.
Government as to the functionality of the software and related material nor
shall the fact of release constitute any such warranty. Furthermore, the
software is released on condition that neither the USGS nor the U.S. Government
shall be held liable for any damages resulting from its authorized or
unauthorized use.
