RNWIS: National Water Information System: R Interface
=====================================================

This [R](http://www.r-project.org/ "R") package provides access to
water-resources data stored on the National Water Information System
([NWIS](http://waterdata.usgs.gov/nwis "NWIS")).
A graphical user interface (GUI) is provided and
requires R operate as an SDI application, using multiple
top-level windows for the console, graphics, and pager.

The set of standards used for coding **RNWIS** is documented in
[Google's R Style Guide](http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html "Google's R Style Guide").

Install
-------

If R is not already installed on your
computer, download and install the latest binary distribution from
[CRAN](http://cran.r-project.org/ "The Comprehensive R Archive Network").
Windows users should set R to operate as an SDI application during installation
by choosing to customize the startup options and specifying the SDI interface
(not the default).

Open an R session and install the required packages from CRAN:

    > install.packages(c('sp', 'RODBC', 'rgeos', 'rgdal', 'brew', 'Rook'))

Install the **RNWIS** package:

    > install.packages('devtools')
    > library(devtools)
    > install_github('RNWIS', username='jfisher-usgs', ref='v0.1.7')

Contact your IT specialist for help with setting up the 
[Oracle](http://bwtst.usgs.gov/database/oracle/index.html "Oracle") 
ODBC drivers.

Run
---

Load **RNWIS** in the current 32-bit R session:

    > library(RNWIS)

Activate the main GUI:

    > OpenRNWIS()
