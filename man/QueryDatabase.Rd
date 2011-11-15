\name{QueryDatabase}

\alias{QueryDatabase}

\title{Query the National Water Information System Database}

\description{
Acquire data from a single database table using a site selection criteria to
constrain the number of sites selected.
}

\usage{
QueryDatabase(channel, sqtable, sqvars = "*",
          site.no.var = NULL, site.no = NULL,
          site.tp.cd.var = NULL, site.tp.cd = NULL,
          agency.cd.var = NULL, agency.cd = NULL,
          lat.var = NULL, lat.lim = c(NA, NA),
          lng.var = NULL, lng.lim = c(NA, NA),
          alt.var = NULL, alt.lim = c(NA, NA),
          d.t.var = NULL, d.t.lim = c(NA, NA))
}

\arguments{
\item{channel}{RODBC; a connection to a ODBC database.}
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
\item{site.tp.cd}{character; vector of site type codes.}
\item{agency.cd.var}{character; column name of queried table which shows the
  agency code.}
\item{agency.cd}{character; vector of agency codes.}
\item{lat.var}{character; column name of queried table which shows the
  latitude.}
\item{lat.lim}{numeric; vector of minimum and maximum latitude values (WGS84).}
\item{lng.var}{character; column name of queried table which shows the
  longitude.}
\item{lng.lim}{numeric; vector of minimum and maximum longitude values (WGS84).}
\item{alt.var}{character; column name of queried table which shows the
  altitude.}
\item{alt.lim}{numeric; vector of minimum and maximum altitude values
  (NGVD29 or NAVD 88).}
\item{d.t.var}{character; column name of queried table which shows a
  date and time variable.}
\item{d.t.lim}{POSIXt; vector of minimum and maximum date values.}
}

\value{
On success, a data frame (possibly with 0 rows) or character string. On error,
a character vector of error message(s).
}

\author{J.C. Fisher}

\seealso{
\code{\link{sqlQuery}}
}

\examples{
\dontrun{con <- odbcConnect("NWIS Idaho", uid = "", pwd = "")}
\donttest{
d <- QueryDatabase(con, sqtable = "sitefile_01",
                   sqvars = c("site_no", "alt_va"),
                   site.no.var = "site_no",
                   site.no = c(432700112470801, 435038112453401))

d <- QueryDatabase(con, sqtable = "sitefile_01",
                   sqvars = c("site_no", "dec_lat_va", "dec_long_va"),
                   site.tp.cd.var = "site_tp_cd", site.tp.cd = "GW",
                   agency.cd.var = "agency_cd", agency.cd = c("USGS", "USEPA"),
                   lng.var = "dec_long_va", lng.lim = c(-114.00, -112.00),
                   lat.var = "dec_lat_va", lat.lim = c(43.00, 44.00))

d <- QueryDatabase(con, sqtable = "gw_lev_01",
                   sqvars = c("site_no", "lev_dt", "lev_va"),
                   site.no.var = "site_no", site.no = 432700112470801)

close(con)
}
}

\keyword{IO}
