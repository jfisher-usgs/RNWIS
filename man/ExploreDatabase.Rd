\name{ExploreDatabase}

\alias{ExploreDatabase}

\title{Explore Database Connection}

\description{
A \acronym{GUI} for exploring the contents of database.
}

\usage{
ExploreDatabase(channel, parent = NULL)
}

\arguments{
\item{channel}{RODBC; connection to ODBC database.}
\item{parent}{tkwin; parent window (optional).}
}

\details{
Shows database table names and the column structure for each table.
}

\author{J.C. Fisher}

\seealso{
\code{\link{sqlTables}}, \code{\link{sqlColumns}}, \code{\link{sqlPrimaryKeys}}
}

\examples{
\dontrun{channel <- odbcConnect("NWIS Idaho", uid = "", pwd = "")}
\donttest{ExploreDatabase(channel)}
\donttest{close(channel)}
}

\keyword{misc}
