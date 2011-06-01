RNWIS: National Water Information System: R Interface
=====================================================

Description
-----------

This [R](http://www.r-project.org/ "R") package provides access to
water-resources data stored on the National Water Information System (NWIS).

The set of standards used for coding **RNWIS** is documented in
[Google's R Style Guide](http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html "Google's R Style Guide").


Installation
------------

If R is not already installed on your
computer, download and install the latest binary distribution from
[CRAN](http://cran.r-project.org/ "The Comprehensive R Archive Network").
Windows users should set R to operate as an SDI application during installation
by choosing to customize the startup options and specifying the SDI interface
(not the default).

Install required R packages from CRAN using a simple call to
`install.packages()`:

    > install.packages(c('tcltk', 'sp', 'RODBC', 'gpclib', 'rgdal'))

Install the **RNWIS** package:

    > install.packages('RNWIS', repos='ftp://ftpext.usgs.gov/pub/wr/id/scoville/Fisher/RNWIS')

The following instructions are provided for accessing NWIS using **RNWIS**.
The site administrator must
[install the Ingres II client](http://bwtst.usgs.gov/database/ingres/ "Ingres")
on the user's computer. The user must be added to the *nwis_select*
Ingres access group (see section 1.5 of the
[NWIS Security System Documentation](http://nwis.usgs.gov/nwisdocs4_2/nwis_security.pdf "NWIS Security")).
**RNWIS** must be installed on the user's computer.
And the user must be provided with the following database connection
information:

+   The hostname of the NWIS server, e.g. the hostname for
    the Idaho district NWIS server is *sun2didbse.wr.usgs.gov*
+   The name of the NWIS database in Ingres ("nwisxx" where "xx"
    is the state postal code, e.g. for the Idaho district the database is named
    "nwisid").
+   The name of the data source name (DSN) that contains the
    connection information to NWIS. An Open Database Connectivity (ODBC) data
    source allows the user to connect to an NWIS database using the
    *nwis_select* Ingres access group. The site administrator enters the data
    source information using the *ODBC Data Source Administrator*.
    **RNWIS** will prompt the user for a data source to connect to. To connect to
    NWIS, the user would select the data source name for the
    NWIS connection.

Running
-------

Load **RNWIS** in the current R session:

    > library(RNWIS)

Activate the main GUI:

    > OpenRNWIS()
