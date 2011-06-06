QueryDatabase <- function(channel, sqtable, sqvars="*",
                          site.no.var=NULL, site.no=NULL,
                          site.tp.cd.var=NULL, site.tp.cd=NULL,
                          agency.cd.var=NULL, agency.cd=NULL,
                          lat.var=NULL, lat.lim=c(NA, NA),
                          lng.var=NULL, lng.lim=c(NA, NA),
                          alt.var=NULL, alt.lim=c(NA, NA),
                          d.t.var=NULL, d.t.lim=c(NA, NA)) {
  # Construct and submit SQL query to an ODBC database.

  # Additional functions (subroutines)

  # Trim leading and trailing white space

  trim <- function(x) {
    sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
  }


  # Main program

  if (inherits(channel, "RODBC"))
    channel <- odbcReConnect(channel)
  else
    channel <- odbcConnect(channel, uid="", pwd="")
  if (channel < 0)
    stop("error occurred when opening connection to ODBC database")
  on.exit(close(channel))

  cond <- c()

  # Variables
  vars <- paste(sqvars, collapse=", ")

  # Site type
  if (!is.null(site.tp.cd.var)) {
    if (!is.null(site.tp.cd)) {
      hold <- paste(site.tp.cd.var, "=\'", site.tp.cd, "\'", sep="")
      hold <- paste("(", paste(hold, collapse=" OR "), ")", sep="")
      cond <- c(cond, hold)
    }
  }

  # Agency
  if (!is.null(agency.cd.var)) {
    if (!is.null(agency.cd)) {
      hold <- paste(agency.cd.var, "=\'", agency.cd, "\'", sep="")
      hold <- paste("(", paste(hold, collapse=" OR "), ")", sep="")
      cond <- c(cond, hold)
    }
  }

  if (is.null(site.no.var)) { # spatial domain
    # Latitude limits
    if (!is.null(lat.var)) {
      if (!is.na(lat.lim[1]))
        cond <- c(cond, paste(lat.var, ">=\'", lat.lim[1], "\'", sep=""))
      if (!is.na(lat.lim[2]))
        cond <- c(cond, paste(lat.var, "<=\'", lat.lim[2], "\'", sep=""))
    }
    # Longitude limits
    if (!is.null(lng.var)) {
      if (!is.na(lng.lim[1]))
        cond <- c(cond, paste(lng.var, ">=\'", lng.lim[1], "\'", sep=""))
      if (!is.na(lng.lim[2]))
        cond <- c(cond, paste(lng.var, "<=\'", lng.lim[2], "\'", sep=""))
    }
    # Altitude limits
    if (!is.null(alt.var)) {
      if (!is.na(alt.lim[1]))
        cond <- c(cond, paste(alt.var, ">=\'", alt.lim[1], "\'", sep=""))
      if (!is.na(alt.lim[2]))
        cond <- c(cond, paste(alt.var, "<=\'", alt.lim[2], "\'", sep=""))
    }
  } else { # site numbers
    if (is.null(site.no))
      return()
    site.no <- na.omit(as.numeric(site.no))
    if (length(site.no) == 0)
      stop("invalid site number(s)")
    hold <- paste(site.no.var, " = \'", site.no, "\'", sep="")
    hold <- paste("(", paste(hold, collapse=" OR "), ")", sep="")
    cond <- c(cond, hold)
  }

  # Date limits
  if (!is.null(d.t.var)) {
    if (inherits(d.t.lim, "POSIXt"))
      d.t.lim <- format(d.t.lim, format="%Y-%m-%d %H:%M:%S")
    if (!is.na(d.t.lim[1]))
      cond <- c(cond, paste(d.t.var, ">=\'", d.t.lim[1], "\'", sep=""))
    if (!is.na(d.t.lim[2]))
      cond <- c(cond, paste(d.t.var, "<=\'", d.t.lim[2], "\'", sep=""))
  }

  # Construct query string
  conds <- ""
  if (length(cond) > 0)
    conds <- paste("WHERE (", paste(cond, collapse=" AND "), ")", sep="")
  query <- paste("SELECT", vars, "FROM", sqtable, conds)

  # Query database
  d <- sqlQuery(channel, query, stringsAsFactors=FALSE)

  # Remove leading and trailing white spaces
  for (i in seq(along=names(d))) {
    if (inherits(d[, i], "character"))
      d[, i] <- trim(d[, i])
  }

  d
}
