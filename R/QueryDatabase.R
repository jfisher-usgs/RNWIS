QueryDatabase <- function(con, sqtable, sqvars="*",
                      site.no.var=NULL, site.no=NA,
                      site.tp.cd.var=NULL, site.tp.cd=NA,
                      lat.var=NULL, lat.lim=c(NA, NA),
                      lng.var=NULL, lng.lim=c(NA, NA),
                      d.t.var=NULL, d.t.lim=c(NA, NA)) {

  trim <- function(x) {
      sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
  }


  # Open connection to database

  if (!inherits(con, "RODBC")) {
    require("RODBC")
    con <- odbcConnect(con, uid="", pwd="")
    on.exit(close(con))
  }

  # Construct query

  vars <- paste(sqvars, collapse=", ")

  cond <- c()

  if (!is.null(site.tp.cd.var)) {
    if (!is.na(site.tp.cd))
      cond <- c(cond, paste(site.tp.cd.var, " = \'", site.tp.cd, "\'", sep=""))
  }

  if (is.null(site.no.var)) {
    if (!is.null(lat.var)) {
      if (!is.na(lat.lim[1]))
        cond <- c(cond, paste(lat.var, " >= \'", lat.lim[1], "\'", sep=""))
      if (!is.na(lat.lim[2]))
        cond <- c(cond, paste(lat.var, " <= \'", lat.lim[2], "\'", sep=""))
    }
    if (!is.null(lng.var)) {
      if (!is.na(lng.lim[1]))
        cond <- c(cond, paste(lng.var, " >= \'", lng.lim[1], "\'", sep=""))
      if (!is.na(lng.lim[2]))
        cond <- c(cond, paste(lng.var, " <= \'", lng.lim[2], "\'", sep=""))
    }
  } else {
    site.no <- na.omit(as.numeric(site.no))
    if (length(site.no) == 0)
      stop("invalid site number(s)")
    cond <- c(cond, paste(site.no.var, " = \'", site.no, "\'", sep=""))
  }

  if (!is.null(d.t.var)) {
    if (inherits(d.t.lim, "POSIXt"))
      d.t.lim <- format(d.t.lim, format="%Y-%m-%d %H:%M:%S")
    if (!is.na(d.t.lim[1]))
      cond <- c(cond, paste(d.t.var, " >= \'", d.t.lim[1], "\'", sep=""))
    if (!is.na(d.t.lim[2]))
      cond <- c(cond, paste(d.t.var, " <= \'", d.t.lim[2], "\'", sep=""))
  }

  conds <- ""
  if (length(cond) > 0)
    conds <- paste("WHERE (", paste(cond, collapse=" AND "), ")", sep="")

  query <- paste("SELECT", vars, "FROM", sqtable, conds)

  # Query database

  d <- sqlQuery(con, query, stringsAsFactors=FALSE)

  # Remove leading and trailing white spaces

  for (i in seq(along=names(d))) {
    if (inherits(d[, i], "character"))
      d[, i] <- trim(d[, i])
  }

  d
}
