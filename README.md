# RNWIS

[![Travis-CI Build Status](https://travis-ci.org/jfisher-usgs/RNWIS.svg?branch=master)](https://travis-ci.org/jfisher-usgs/RNWIS)

## Overview


The [R](http://www.r-project.org/) package **RNWIS** provides access to water-resources data stored on the
**N**ational **W**ater **I**nformation **S**ystem ([NWIS](http://waterdata.usgs.gov/nwis)).
A graphical user interface (GUI) is provided and requires R operate as an SDI application,
using multiple top-level windows for the console, graphics, and pager.

## Install

If R is not already installed on your computer, download and install the latest binary distribution from
[CRAN](http://cran.r-project.org/ "The Comprehensive R Archive Network").
Windows users should set R to operate as an SDI application during installation
by choosing to customize the start-up options and specifying the SDI interface (not the default).

You can install the stable version of **ObsNetwork** from [GitHub](https://jfisher-usgs.github.io/R/),
and its dependencies from CRAN, using the following commands:

```r
repos <- c("https://jfisher-usgs.github.io/R", getOption("repos"))
install.packages("RNWIS", repos = repos)
```

Or use **devtools** to install the development version.

```r
devtools::install_github("jfisher-usgs/RNWIS")
```

Contact your IT specialist for help with setting up the Oracle ODBC driver (only available in 32-bit).

## Run

Load **RNWIS** in the current 32-bit R session and activate the main GUI

```r
library(RNWIS)
```

## Bugs

Please consider reporting bugs and asking questions on the [Issues page](https://github.com/jfisher-usgs/RNWIS/issues).

## Disclaimer

This software is in the public domain because it contains materials that originally came from the USGS,
an agency of the United States Department of Interior.
For more information, see the
[official USGS copyright policy](https://www2.usgs.gov/visual-id/credit_usgs.html "official USGS copyright policy").

Although this software program has been used by the USGS, no warranty, expressed or implied,
is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and
related program material nor shall the fact of distribution constitute any such warranty,
and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

[![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)
