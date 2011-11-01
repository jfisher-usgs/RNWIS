\name{MapSites}

\alias{MapSites}

\title{Map Site Locations in Google Maps}

\description{
Add site markers and polygon objects to
\href{http://maps.google.com/}{Google Maps}.
}

\usage{
MapSites(sites, polygons = NULL, map.id = "map")
}

\arguments{
\item{sites}{data.frame; site information, see \sQuote{Details}.}
\item{polygons}{gpc.poly; polygon information.}
\item{map.id}{character; unique identifier in map \acronym{URL}.}
}

\details{
The \code{sites} data table has components of
\code{lat}, \code{lng}, \code{alt}, \code{site}, \code{name}, \code{agency},
and \code{type}.
Where \code{lat} and \code{lng} are the latitude and longitude
based on the \href{http://en.wikipedia.org/wiki/WGS84}{WGS84} datum;
\code{alt} is the altitude of the site referenced to the specified
vertical datum (\href{http://en.wikipedia.org/wiki/NGVD29}{NGVD29} or
\href{http://en.wikipedia.org/wiki/NAVD88}{NAVD 88});
\code{site} is the unique site identification number;
\code{name} is the name of the site;
\code{agency} is the code for the agency reporting the data;
and \code{type} is the hydrologic setting of the site.
}

\value{
Writes a \href{http://www.json.org/}{JSON} data file (\file{.json})
to a temporary directory.
An Rhttpd object is created and started in the internal web server.
}

\author{Fisher, J.C.}

\seealso{
\code{\link{tempdir}}, \code{\link{Rhttpd-class}}
}

\examples{
\donttest{s <- data.frame(lat = c(43.5757402325, 43.5803046219, 43.8642317971),
                lng = c(-112.934719019, -112.876178537, -112.742880208),
                site = c(433433112560201, 433449112523101, 435152112443101),
                name = c("CPP 1", "NPR Test", "ANP 6"),
                agency = "USGS", type = "GW", stringsAsFactors = FALSE)
MapSites(sites = s, map.id = "map01")

f <- system.file("extdata/ExamplePolygon.ply", package = "RNWIS")
p <- read.polyfile(f, nohole = FALSE)
MapSites(sites = s, polygons = p, map.id = "map02")
}
}

\keyword{aplot}
