#' Map Site Locations in Google Maps
#'
#' Add site markers and polygon objects to \href{http://maps.google.com/}{Google Maps}.
#'
#' @param sites data.frame.
#'   Site information, see 'Details'.
#' @param polygons gpc.poly.
#'   Polygon information
#' @param map.id character.
#'   Unique identifier in map URL
#'
#' @details The \code{sites} data table has components of \code{lat}, \code{lng},
#'   \code{alt}, \code{site}, \code{name}, \code{agency}, and \code{type}.
#'   Where \code{lat} and \code{lng} are the latitude and longitude based on the
#'   \href{http://en.wikipedia.org/wiki/WGS84}{WGS84} datum;
#'   \code{alt} is the altitude of the site referenced to the specified vertical datum
#'   (\href{http://en.wikipedia.org/wiki/NGVD29}{NGVD29} or \href{http://en.wikipedia.org/wiki/NAVD88}{NAVD 88});
#'   \code{site} is the unique site identification number;
#'   \code{name} is the name of the site;
#'   \code{agency} is the code for the agency reporting the data; and
#'   \code{type} is the hydrologic setting of the site.
#'
#' @return Writes a \href{http://www.json.org/}{JSON} data file (\file{.json}) to a temporary directory.
#'   An Rhttpd object is created and started in the internal web server.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{Rhttpd-class}}
#'
#' @keywords aplot
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   s <- data.frame(lat    = c(43.5757402325, 43.5803046219, 43.8642317971),
#'                   lng    = c(-112.934719019, -112.876178537, -112.742880208),
#'                   site   = c(433433112560201, 433449112523101, 435152112443101),
#'                   name   = c("CPP 1", "NPR Test", "ANP 6"),
#'                   agency = "USGS", type = "GW", stringsAsFactors = FALSE)
#'   MapSites(sites = s, map.id = "map01")
#'
#'   f <- system.file("extdata/ExamplePolygon.ply", package = "RNWIS")
#'   p <- rgeos::read.polyfile(f, nohole = FALSE)
#'   MapSites(sites = s, polygons = p, map.id = "map02")
#' }
#'

MapSites <- function(sites, polygons=NULL, map.id="map") {

  # build JavaScript Object Notation (JSON) data table
  BuildJSONTable <- function(d) {
    idxs <- which(sapply(1:ncol(d), function(i) mode(d[, i])) == "character")
    col.names <- names(d)
    for (i in seq(along=col.names)) {
      na.rows <- is.na(d[, i])
      if (i %in% idxs) {
        d[, i] <- paste0("\"", col.names[i], "\": \"", d[, i], "\"")
      } else {
        d[, i] <- paste0("\"", col.names[i], "\": ", d[, i])
        d[na.rows, i] <- paste0("\"", col.names[i], "\": \"NA\"")
      }
    }
    s <- apply(d, 1, function(i) paste(i, collapse=", "))
    s <- paste0("{", s, "}")
    s <- paste(s, collapse=",\n")
    s
  }

  # check if polygon winding direction is clockwise
  ClockWise <- function(x, y) {
    num <- length(x)
    if (num < 3) return()
    count <- 0
    m <- c(2:num, 1)
    n <- c(3:num, 1:2)
    for (i in 1:num) {
      j <- m[i]
      k <- n[i]
      z <- (x[j] - x[i]) * (y[k] - y[j]) - (y[j] - y[i]) * (x[k] - x[j])
      if (z < 0) {
        count <- count - 1
      } else if (z > 0) {
        count <- count + 1
      }
    }
    if (count > 0) {
      return(FALSE)
    } else if (count < 0) {
      return(TRUE)
    } else {
      stop("incomputable winding")
    }
  }


  # construct character string based on JSON format
  is.sites <- !is.null(sites)
  is.polygons <- !is.null(polygons) && inherits(polygons, "gpc.poly")

  s <- "var data = {"
  if (is.sites) {
    sites <- sites[!is.na(sites[, "lng"]) & !is.na(sites[, "lat"]), ]
    s <- c(s, "\"sites\": [", BuildJSONTable(sites))
    s <- c(s, if (is.polygons) "]," else "]")
  }
  if (is.polygons) {
    poly.pts <- rgeos::get.pts(polygons)
    n <- length(poly.pts)
    s <- c(s, "\"polygons\": [")
    for (i in seq(along=poly.pts)) {
      lng <- poly.pts[[i]]$x
      lat <- poly.pts[[i]]$y

      # Browser graphics engines require clockwise outer polygon(s) and
      # counterclockwise inner polygon(s) for proper rendering of inner
      # hole; this is not documented in the Google Maps API (wtf!).
      # All polygons will be placed in a giant polygon that is bigger than
      # North America so everything outside the user defined polygon is tinted.

      is.clockwise <- ClockWise(lng, lat)
      is.hole <- poly.pts[[i]]$hole
      if ((is.hole & !is.clockwise) | (!is.hole & is.clockwise)) {
        lng <- rev(lng)
        lat <- rev(lat)
      }
      d <- as.data.frame(cbind(lat=lat, lng=lng))
      s <- c(s, "[", BuildJSONTable(d))
      s <- c(s, if (i < n) "]," else "]")
    }
    s <- c(s, "]")
  }
  s <- c(s, "}")

  # Write JSON file to temporary directory
  # TODO: Give JSON file a unique name; see ./inst/map/config.R for difficulty
  #       with implementing this. Required for proper browser refresh with
  #       older maps.

  temp.dir <- file.path(tempdir(), "json")
  dir.create(temp.dir, showWarnings=FALSE)
  f.json <- file.path(temp.dir, "data.json")

  con <- file(description=f.json, open="w")
  cat(s, file=con, sep="\n", append=FALSE)
  close(con)

  # give r application to internal web server
  if ("package:RNWIS" %in% search())
    map.path <- system.file("map", package="RNWIS")
  else
    map.path <- file.path(getwd(), "inst", "map")

  try(tools::startDynamicHelp(start=FALSE), silent=TRUE)

  server <- Rook::Rhttpd$new()
  server$add(app=file.path(map.path, "config.R"), name=map.id)

  server$start(quiet=TRUE)
  server$browse(map.id)

  return(server)
}
