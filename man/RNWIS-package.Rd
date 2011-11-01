\name{RNWIS-package}

\alias{RNWIS-package}

\docType{package}

\title{
National Water Information System: R Interface
}

\description{
This package provides access to water-resources data.
It retrieves data from the National Water Information System
(\href{http://waterdata.usgs.gov/nwis}{NWIS})
through an Open Database Connectivity (\acronym{ODBC}) connection.
}

\details{
  \tabular{ll}{
    Package: \tab RNWIS\cr
    Type: \tab Package\cr
    Version: \tab 0.1-5\cr
    Date: \tab 2011-11-01\cr
    License: \tab GPL (>= 2)
  }
}

\note{
The \pkg{RNWIS} \acronym{GUI} requires \R operate as an \acronym{SDI}
application, using multiple top-level windows for the console, graphics, and
pager.

The following user instructions are provided for accessing \acronym{NWIS}
using \pkg{RNWIS}. The site administrator must
\href{http://bwtst.usgs.gov/database/ingres/}{install the Ingres II client}
(only available in 32-bit)
on the user's computer. The user must be added to the \emph{nwis_select}
Ingres access group (see section 1.5 of the
\href{http://nwis.usgs.gov/nwisdocs4_2/nwis_security.pdf}{NWIS Security System Documentation}).
\pkg{RNWIS} must be installed on the user's computer.
And the user must be provided with the following database connection
information:
\itemize{
\item The hostname of the \acronym{NWIS} server, e.g. the hostname for
the Idaho district \acronym{NWIS} server is \emph{sun2didbse.wr.usgs.gov}
\item The name of the \acronym{NWIS} database in Ingres ("\emph{nwisxx}"
where "\emph{xx}" is the state postal code, e.g. for the Idaho district the
database is named "\emph{nwisid}").
\item The name of the data source name (\acronym{DSN}) that contains the
connection information to \acronym{NWIS}. An \acronym{ODBC} data source
allows the user to connect to an \acronym{NWIS} database using the
\emph{nwis_select} Ingres access group. The site administrator enters the data
source information using the \emph{ODBC Data Source Administrator}.
\pkg{RNWIS} will prompt the user for a data source to connect to. To connect to
\acronym{NWIS}, the user would select the data source name for the
\acronym{NWIS} connection.
}

The set of standards used for coding \pkg{RNWIS} is documented in
\href{http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html}{Google's R Style Guide}.
}

\author{
Jason C. Fisher

Maintainer: <\email{jfisher@usgs.gov}>
}

\examples{
\donttest{library(RNWIS)
OpenRNWIS()
}
}

\keyword{package}

