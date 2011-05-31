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
\item{con}{RODBC; a connection to a ODBC database.}
\item{parent}{tkwin; the parent window (optional).}
}

\details{
Shows the database table names and column structure for each table.
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
