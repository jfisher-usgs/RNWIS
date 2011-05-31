\name{MapSites}

\alias{MapSites}

\title{Map Site Locations in Google Maps}

\description{
Add site markers and polygon objects to Google Maps.
}

\usage{
MapSites(sites, polygons = NULL, map.id = NULL)
}

\arguments{
\item{sites}{data.frame; site location information, see \sQuote{Details}.}
\item{polygons}{gpc.poly; polygon information.}
\item{map.id}{character; the initial part of the temporary file names.}
}

\details{
The \code{sites} data table has components of
\code{lat}, \code{lng}, \code{alt}, \code{site}, \code{name}, \code{agency},
and \code{type}.
Where \code{lat} and \code{lng} are the latitude and longitude
based on the WGS84 datum;
\code{alt} is the altitude of the site referenced to the specified
vertical datum (NGVD29 or NAVD88);
\code{site} is the unique site identification number;
\code{name} is the name of the site;
\code{agency} is the code for the agency reporting the data;
and \code{type} is the hydrologic setting of the site.
}

\value{
Writes three files to a temporary directory. Base names are identical with
extensions indicating file type. An \acronym{HTML} file (\file{.html}) is
loaded into the default web browser and calls on MarkerClusterer,
a javascript library (\file{.js}) that creates and manages per-zoom-level
clusters for large amounts of markers, and a \acronym{JSON} data file
(\file{.json}) dynamically created from site and polygon information.
}

\author{Fisher, J.C.}

\seealso{
\code{\link{browseURL}}, \code{\link{tempfile}}
}

\examples{
\donttest{s <- data.frame(lat = c(43.5757402325, 43.5803046219, 43.8642317971),
                lng = c(-112.934719019, -112.876178537, -112.742880208),
                site = c(433433112560201, 433449112523101, 435152112443101),
                name = c("CPP 1", "NPR Test", "ANP 6"),
                agency = "USGS", type = "GW", stringsAsFactors = FALSE)
MapSites(sites = s, map.id = "Example1")

f <- system.file("extdata/ExamplePolygon.ply", package="RNWIS")
p <- read.polyfile(f, nohole=FALSE)
MapSites(sites = s, polygons = p, map.id = "Example2")
}
}

\keyword{aplot}
