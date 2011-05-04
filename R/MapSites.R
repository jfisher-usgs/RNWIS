MapSites <- function(data, lat.var, lng.var, site.var, name.var, agency.var,
                     map.id=NULL) {

  # Remove records with NA coordinate values

  data <- data[!is.na(data[, lng.var]) & !is.na(data[, lat.var]), ]

  # Consolidate data

  data <- paste("{",
                "\"lat\": ", data[, lat.var], ", ",
                "\"lng\": ", data[, lng.var], ", ",
                "\"site\": \"", data[, site.var], "\", ",
                "\"name\": \"", data[, name.var], "\", ",
                "\"agency\": \"", data[, agency.var], "\"",
                "},", sep="")
  n <- length(data)
  data[n] <- substr(data[n], 1, nchar(data[n]) - 1)

  # Map ID

  if (is.null(map.id))
    map.id <- "MapID"

  # Write temporary files

  f <- tempfile(pattern=map.id, tmpdir=tempdir())
  f.html <- paste(f, ".html", sep="")
  f.json <- paste(f, ".json", sep="")
  f.js <- paste(f, ".js", sep="")

  is.pkg <- "package:RNWIS" %in% search()
  if (is.pkg) {
    path <- system.file(package="RNWIS")
  } else {
    path <- paste(getwd(), "/inst", sep="")
  }

  obj <- readLines(paste(path, "/MapSites.html", sep=""))
  write(obj, file=f.html, sep="\n")

  obj <- readLines(paste(path, "/markerclusterer.js", sep=""))
  write(obj, file=f.js, sep="\n")

  con <- file(description=f.json, open="w")

  # Beginning of html

  s <- "var data = {"
  s <- c(s, paste("\"count\": ", length(data), ",", sep=""))
  s <- c(s, "\"sites\": [")
  cat(s, file=con, sep="\n", append=FALSE)

  # Site information

  increment <- 1000
  imin <- seq(1, n, by=increment)
  imax <- imin + increment - 1
  if (imax[length(imax)] > n)
    imax[length(imax)] <- n
  apply(cbind(imin, imax), 1,
        function(i) cat(data[i[1]:i[2]], file=con, sep="\n", append=TRUE))

  # Ending of html

  s <- "]}\n"
  cat(s, file=con, append=TRUE)

  # Close file connection

  close(con)

  # Open html file in web browser

  browseURL(paste("file:\\\\", f.html, sep=""), browser=getOption("browser"))
}
