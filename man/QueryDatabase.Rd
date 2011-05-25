\name{QueryDatabase}

\alias{QueryDatabase}

\title{Query the National Water Information System Database}

\description{
Something
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
\item{con}{connection; something.}
\item{sqtable}{character; something.}
\item{sqvars}{character; something.}
\item{site.no.var}{character; something.}
\item{site.no}{numeric; something.}
\item{site.tp.cd.var}{character; something.}
\item{site.tp.cd}{character; something.}
\item{agency.cd.var}{character; something.}
\item{agency.cd}{character; something.}
\item{lat.var}{character; something.}
\item{lat.lim}{numeric; something.}
\item{lng.var}{character; something.}
\item{lng.lim}{numeric; something.}
\item{alt.var}{character; something.}
\item{alt.lim}{numeric; something.}
\item{d.t.var}{character; something.}
\item{d.t.lim}{character, POSIXt; something.}
}

\details{
Something.
}

\author{Fisher, J.C.}

\seealso{
\code{odbcConnect}, \code{sqlQuery}
}

\examples{
print("something")
}

\keyword{IO}
