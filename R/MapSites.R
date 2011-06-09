MapSites <- function(sites, polygons=NULL, map.id="map") {
  # Write site and polygon data to JSON file and open in Google Maps.

  # Additional functions (subroutines)

  # Build JavaScript Object Notation (JSON) data table

  BuildJSONTable <- function(d) {
    idxs <- which(sapply(1:ncol(d), function(i) mode(d[, i])) == "character")
    col.names <- names(d)
    for (i in seq(along=col.names)) {
      na.rows <- is.na(d[, i])
      if (i %in% idxs) {
        d[, i] <- paste("\"", col.names[i], "\": \"", d[, i], "\"", sep="")
      } else {
        d[, i] <- paste("\"", col.names[i], "\": ", d[, i], sep="")
        d[na.rows, i] <- paste("\"", col.names[i], "\": \"NA\"", sep="")
      }
    }
    s <- apply(d, 1, function(i) paste(i, collapse=", "))
    s <- paste("{", s, "}", sep="")
    s <- paste(s, collapse=",\n")
    s
  }

  # Check if polygon winding direction is clockwise

  ClockWise <- function(x, y) {
    num <- length(x)
    if (num < 3)
      return()
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


  # Main program

  # Construct character string based on JSON format

  is.sites <- !is.null(sites)
  is.polygons <- !is.null(polygons) && inherits(polygons, "gpc.poly")

  s <- "var data = {"
  if (is.sites) {
    sites <- sites[!is.na(sites[, "lng"]) & !is.na(sites[, "lat"]), ]
    s <- c(s, "\"sites\": [", BuildJSONTable(sites))
    s <- c(s, if (is.polygons) "]," else "]")
  }
  if (is.polygons) {
    poly.pts <- get.pts(polygons)
    n <- length(poly.pts)
    s <- c(s, "\"polygons\": [")
    for (i in seq(along=poly.pts)) {
      lng <- poly.pts[[i]]$x
      lat <- poly.pts[[i]]$y

      # Browser graphics engines require clockwise outer polygon(s) and
      # counterclockwise inner polygon(s) for proper rendering of inner
      # hole; this is not documented in the Google Maps API (wtf!).

      is.clockwise <- ClockWise(lng, lat)
      is.hole <- poly.pts[[i]]$hole
      if ((is.hole & is.clockwise) | (!is.hole & !is.clockwise)) {
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

  temp.dir <- file.path(tempdir(), "json")
  dir.create(temp.dir, showWarnings=FALSE)
  f.json <- file.path(temp.dir, "data.json")

  con <- file(description=f.json, open="w")
  cat(s, file=con, sep="\n", append=FALSE)
  close(con)

  # Give R application to web server

  if ("package:RNWIS" %in% search())
    map.path <- system.file("map", package="RNWIS")
  else
    map.path <- file.path(getwd(), "inst", "map")

  server <- Rhttpd$new()
  server$add(app=file.path(map.path, "config.R"), name=map.id)

  server$start(quiet=TRUE)
  server$browse(map.id)
}
