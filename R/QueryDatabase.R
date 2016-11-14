#' Query the National Water Information System Database
#'
#' This function acquire data from a single database table using a
#' site selection criteria to constrain the number of sites selected.
#'
#' @param channel RODBC.
#'   A connection to a ODBC database.
#' @param sqtable character.
#'   Name of the table from which data is to be retrieved.
#' @param sqvars character.
#'   Vector of column names in queried table to be included in the final query results;
#'   its default is an asterisk \file{*} and specifies that all columns will be returned.
#' @param site.no.var character.
#'   Column name of table which shows the site identification number.
#' @param site.no numeric.
#'   Vector of site identification numbers
#' @param site.tp.cd.var character.
#'   Column name of queried table which shows the site type.
#' @param site.tp.cd character.
#'   Vector of site type codes
#' @param agency.cd.var character.
#'   Column name of queried table which shows the agency code.
#' @param agency.cd character.
#'   Vector of agency codes
#' @param lat.var character.
#'   Column name of queried table which shows the latitude.
#' @param lat.lim numeric.
#'   Vector of minimum and maximum latitude values (WGS84).
#' @param lng.var character.
#'   Column name of queried table which shows the longitude.
#' @param lng.lim numeric.
#'   Vector of minimum and maximum longitude values (WGS84).
#' @param alt.var character.
#'   Column name of queried table which shows the altitude.
#' @param alt.lim numeric.
#'   Vector of minimum and maximum altitude values (NGVD29 or NAVD 88).
#' @param d.t.var character.
#'   Column name of queried table which shows a date and time variable.
#' @param d.t.lim POSIXt.
#'   Vector of minimum and maximum date values
#'
#' @return On success, returns an object of class \code{data.frame} or \code{character}.
#'   On failure, returns a vector of error message(s).
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{sqlQuery}}
#'
#' @keywords IO
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   con <- RODBC::odbcConnect("NWIS Idaho", uid = "", pwd = "")
#'
#'   d <- QueryDatabase(con, sqtable = "sitefile_01",
#'                      sqvars = c("site_no", "alt_va"),
#'                      site.no.var = "site_no",
#'                      site.no = c(432700112470801, 435038112453401))
#'
#'   d <- QueryDatabase(con, sqtable = "sitefile_01",
#'                      sqvars = c("site_no", "dec_lat_va", "dec_long_va"),
#'                      site.tp.cd.var = "site_tp_cd", site.tp.cd = "GW",
#'                      agency.cd.var = "agency_cd", agency.cd = c("USGS", "USEPA"),
#'                      lng.var = "dec_long_va", lng.lim = c(-114.00, -112.00),
#'                      lat.var = "dec_lat_va", lat.lim = c(43.00, 44.00))
#'
#'   d <- QueryDatabase(con, sqtable = "gw_lev_01",
#'                      sqvars = c("site_no", "lev_dt", "lev_va"),
#'                      site.no.var = "site_no", site.no = 432700112470801)
#'
#'   close(con)
#' }
#'

QueryDatabase <- function(channel, sqtable, sqvars="*",
                          site.no.var=NULL, site.no=NULL,
                          site.tp.cd.var=NULL, site.tp.cd=NULL,
                          agency.cd.var=NULL, agency.cd=NULL,
                          lat.var=NULL, lat.lim=c(NA, NA),
                          lng.var=NULL, lng.lim=c(NA, NA),
                          alt.var=NULL, alt.lim=c(NA, NA),
                          d.t.var=NULL, d.t.lim=c(NA, NA)) {

  if (inherits(channel, "RODBC"))
    channel <- RODBC::odbcReConnect(channel)
  else
    channel <- RODBC::odbcConnect(channel, uid="", pwd="", readOnlyOptimize=TRUE)
  if (channel < 0)
    stop("error occurred when opening connection to ODBC database")
  on.exit(close(channel))

  cond <- c()

  # variables
  vars <- paste(sqvars, collapse=", ")

  # site type
  if (!is.null(site.tp.cd.var) && !is.null(site.tp.cd)) {
    hold <- paste0(site.tp.cd.var, "=\'", site.tp.cd, "\'")
    hold <- paste0("(", paste(hold, collapse=" OR "), ")")
    cond <- c(cond, hold)
  }

  # agency
  if (!is.null(agency.cd.var) && !is.null(agency.cd)) {
    hold <- paste0(agency.cd.var, "=\'", agency.cd, "\'")
    hold <- paste0("(", paste(hold, collapse=" OR "), ")")
    cond <- c(cond, hold)
  }

  if (is.null(site.no.var)) {

    # latitude limits
    if (!is.null(lat.var)) {
      if (!is.na(lat.lim[1]))
        cond <- c(cond, paste0(lat.var, ">=\'", lat.lim[1], "\'"))
      if (!is.na(lat.lim[2]))
        cond <- c(cond, paste0(lat.var, "<=\'", lat.lim[2], "\'"))
    }

    # longitude limits
    if (!is.null(lng.var)) {
      if (!is.na(lng.lim[1]))
        cond <- c(cond, paste0(lng.var, ">=\'", lng.lim[1], "\'"))
      if (!is.na(lng.lim[2]))
        cond <- c(cond, paste0(lng.var, "<=\'", lng.lim[2], "\'"))
    }

    # altitude limits
    if (!is.null(alt.var)) {
      if (!is.na(alt.lim[1]))
        cond <- c(cond, paste0(alt.var, ">=\'", alt.lim[1], "\'"))
      if (!is.na(alt.lim[2]))
        cond <- c(cond, paste0(alt.var, "<=\'", alt.lim[2], "\'"))
    }

  } else {
    if (is.null(site.no)) return()
    site.no <- stats::na.omit(as.numeric(site.no))
    if (length(site.no) == 0) stop("invalid site number(s)")
    hold <- paste0(site.no.var, " = \'", site.no, "\'")
    hold <- paste0("(", paste(hold, collapse=" OR "), ")")
    cond <- c(cond, hold)
  }

  # date limits
  if (!is.null(d.t.var)) {
    if (inherits(d.t.lim, "POSIXt"))
      d.t.lim <- format(d.t.lim, format="%Y-%m-%d %H:%M:%S")
    if (!is.na(d.t.lim[1]))
      cond <- c(cond, paste0(d.t.var, ">=\'", d.t.lim[1], "\'"))
    if (!is.na(d.t.lim[2]))
      cond <- c(cond, paste0(d.t.var, "<=\'", d.t.lim[2], "\'"))
  }

  # construct query string
  conds <- ""
  if (length(cond) > 0)
    conds <- paste0("WHERE (", paste(cond, collapse=" AND "), ")")
  sq.table <- paste(RODBC::odbcGetInfo(channel)[["Server_Name"]], sqtable, sep=".")
  query <- paste("SELECT", vars, "FROM", sq.table, conds)

  # query database
  d <- RODBC::sqlQuery(channel, query, stringsAsFactors=FALSE)

  # remove leading and trailing white spaces
  for (i in seq(along=names(d))) {
    if (inherits(d[, i], "character")) d[, i] <- trimws(d[, i])
  }

  return(d)
}
