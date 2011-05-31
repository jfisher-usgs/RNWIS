\name{QueryDatabase}

\alias{QueryDatabase}

\title{Query the National Water Information System Database}

\description{
Acquire data from a single database table using a site selection criteria to
constrain the number of sites selected.
}

\usage{
QueryDatabase(con, sqtable, sqvars = "*",
          site.no.var = NULL, site.no = NULL,
          site.tp.cd.var = NULL, site.tp.cd = NULL,
          agency.cd.var = NULL, agency.cd = NULL,
          lat.var = NULL, lat.lim = c(NA, NA),
          lng.var = NULL, lng.lim = c(NA, NA),
          alt.var = NULL, alt.lim = c(NA, NA),
          d.t.var = NULL, d.t.lim = c(NA, NA))
}

\arguments{
\item{con}{connection; a connection to a ODBC database.}
\item{sqtable}{character; name of the table from which data is to be
  retrieved.}
\item{sqvars}{character; vector of column names in queried table
  to be included in the final query results; its default is an asterisk
  \file{*} and specifies that all columns will be returned.}
\item{site.no.var}{character; column name of table which shows the site
  identification number.}
\item{site.no}{numeric; vector of site identification numbers.}
\item{site.tp.cd.var}{character; column name of queried table which shows the
  site type.}
\item{site.tp.cd}{character; something.}
\item{agency.cd.var}{character; column name of queried table which shows the
  agency code.}
\item{agency.cd}{character; something.}
\item{lat.var}{character; column name of queried table which shows the
  latitude.}
\item{lat.lim}{numeric; vector with the minimum and maximum latitude values.}
\item{lng.var}{character; column name of queried table which shows the
  longitude.}
\item{lng.lim}{numeric; something.}
\item{alt.var}{character; column name of queried table which shows the
  altitude.}
\item{alt.lim}{numeric; something.}
\item{d.t.var}{character; column name of queried table which shows a
  date and time variable.}
\item{d.t.lim}{POSIXt; something.}
}

\details{
Something.
}

\value{
Something.
}

\author{Fisher, J.C.}

\seealso{
\code{odbcConnect}, \code{sqlQuery}
}

\examples{
\donttest{con <- odbcConnect("NWIS")
QueryDatabase(con)
}
}

\keyword{IO}
