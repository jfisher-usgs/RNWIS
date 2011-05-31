\name{ExploreDatabase}

\alias{ExploreDatabase}

\title{Explore Database Connection}

\description{
A \acronym{GUI} for exploring the contents of database.
}

\usage{
ExploreDatabase(con, parent = NULL)
}

\arguments{
\item{con}{RODBC; connection to ODBC database.}
\item{parent}{tkwin; parent window (optional).}
}

\details{
Shows database table names and column structure for each table.
}

\author{Fisher, J.C.}

\seealso{
\code{sqlTables}, \code{sqlColumns}, \code{sqlPrimaryKeys}
}

\examples{
\donttest{con <- odbcConnect("NWIS")
ExploreDatabase(con)}
}

\keyword{misc}
