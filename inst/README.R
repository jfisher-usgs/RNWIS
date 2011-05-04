

# Ingres is a commercially supported, open-source SQL relational
# database management system that is fully open source.
# To install the Ingres client software,
#   http://dg14dpahrb.er.usgs.gov/ingres/ing2006_w32.html


# Use the ODBC Administrator to update your data source connection information
#   START [menu] -> SETTINGS [menu] -> CONTROL PANEL -> Administrative Tools ->
#   Data Sources (ODBC) ->  System DSN [tab] -> ADD [button] ->
#   Ingres 3.0 [select] -> FINISH [button] ->
#   Data Source: NWIS Idaho
#   Server Vnode: SUN2DIDBSE.WR.USGS.GOV
#   Server Type: INGRES
#   Server Database: nwisid
#   Connection Options, Group: nwis-select


# Connecting to NWIS database in R

# Open R package
require("RODBC")

# Establish path
path <- "D:/WORK/JFisher/Software/RNWIS"

# Open a connection to the NWIS database
con <- odbcConnect("NWIS Idaho", uid="", pwd="")
print(con)
con.summary <- attributes(con)
print(con.summary$class)

# List the tables in the database
nwis.tables <- sqlTables(con, errors=FALSE, as.is=TRUE)[, "TABLE_NAME"]
f <- paste(path, "nwis.tables.txt", sep="/")
write.table(nwis.tables, file=f, sep="\t", quote=FALSE, row.names=FALSE)

# Tables (http://nwis.usgs.gov/dbms):
#
#  sitefile_01: Site information indexed by the site number (site_no)
#                 http://nwis.usgs.gov/dbms/4.9/cm-dd.html
#  gw_lev_01: Groundwater levels indexed by site_no
#               http://nwis.usgs.gov/dbms/4.9/gw-dd.html



# Query: sitefile_01

sitefile.keys <- sqlPrimaryKeys(con, sqtable="sitefile_01")[, "COLUMN_NAME"]
sitefile.cols <- sqlColumns(con, sqtable="sitefile_01")[, "COLUMN_NAME"]

### sel <- sqlQuery(con, "SELECT * FROM sitefile_01") # takes a long time
### f <- paste(path, "sitefile.ex.txt", sep="/")
### write.table(sel[1:3, ], file=f, sep="\t", quote=FALSE, row.names=FALSE)

sel <- sqlQuery(con, "SELECT site_id, site_no, station_nm FROM sitefile_01")
f <- paste(path, "sitefile.id.no.nm.txt", sep="/")
write.table(sel, file=f, sep="\t", quote=FALSE, row.names=FALSE)


sel <- sqlQuery(con, "SELECT agency_cd FROM sitefile_01")

sel <- sqlQuery(con, "SELECT * FROM PARM_ALIAS")


# Query: gw_lev_01

gw_level.keys <- sqlPrimaryKeys(con, sqtable="gw_lev_01")[, "COLUMN_NAME"]
gw_level.cols <- sqlColumns(con, sqtable="gw_lev_01")[, "COLUMN_NAME"]

sel <- sqlQuery(con, "SELECT * FROM gw_lev_01 WHERE site_no = \'434126112550701\'")
f <- paste(path, "gw_level.434126112550701.txt", sep="/")
write.table(sel, file=f, sep="\t", quote=FALSE, row.names=FALSE)




agency.cols <- sqlColumns(con, sqtable="agency")[, "COLUMN_NAME"]
sel <- sqlQuery(con, "SELECT * FROM agency")
f <- paste(path, "agency.txt", sep="/")
write.table(sel, file=f, sep="\t", quote=FALSE, row.names=FALSE)

gw_hole.cols <- sqlColumns(con, sqtable="gw_hole_01")[, c("COLUMN_NAME", "TYPE_NAME")]
sel <- sqlQuery(con, "SELECT * FROM gw_hole_01 WHERE site_no  = \'432714112560702\'")
# interesting, should add zone information to gw_hole for MLMS wells


site_tp.cols <- sqlColumns(con, sqtable="site_tp")[, "COLUMN_NAME"]
sel <- sqlQuery(con, "SELECT * FROM site_tp")
f <- paste(path, "site_tp.txt", sep="/")
write.table(sel, file=f, sep="\t", quote=FALSE, row.names=FALSE)


### This is what you need to connect variable name to PARM table
gw_gwdd.cols <- sqlColumns(con, sqtable="GW_GWDD")[, "COLUMN_NAME"]
sel <- sqlQuery(con, "SELECT * FROM GW_GWDD")
f <- paste(path, "gw_gwdd.txt", sep="/")
write.table(sel, file=f, sep="\t", quote=FALSE, row.names=FALSE)



sqlColumns(con, sqtable="gw_lev_01")[, c("COLUMN_NAME", "TYPE_NAME")]



# Close database connection
odbcCloseAll()
close(con)



sqlColumns(con, sqtable="gw_hole_01")[, "COLUMN_NAME"]


dic <- list()
dic$site_no <- "Site id (C1)"
dic$station_nm <- "Station name (C12)"
dic$dec_lat_va <- "Latitude (C9)"
dic$dec_long_va <- "Longitude (C10)"
dic$coord_datum_cd <- "Lat/Long datum (C36)"
dic$alt_va <- "Altidude (C16)"
dic$alt_acy_va <- "Altitdue accuracy (C18)"
dic$alt_datum_cd <- "Altitdue datum (C22)"
dic$hole_depth_va <- "Hole depth (C27)"
dic$well_depth_va <- "Well depth (C28)"


################################################################################
#                           QueryDatabase                                      #
################################################################################






d <- QueryDatabase(con="NWIS Idaho", sqtable="sitefile_01",
               sqvars=c("site_no", "dec_lat_va", "dec_long_va"))



d <- QueryDatabase(con="NWIS Idaho", sqtable="gw_hole_01",
               site.no="434126112550701")
d <- QueryDatabase(con="NWIS Idaho", sqtable="gw_csng_01",
               site.no="434126112550701")
d <- QueryDatabase(con="NWIS Idaho", sqtable="gw_open_01",
               site.no="434126112550701")



d <- QueryDatabase(con="NWIS Idaho", sqtable="gw_lev_01",
               sqvars=c("site_no", "lev_dt", "lev_va"),
               site.no="434126112550701", tcol="lev_dt",
               tlim=c("1994-03-10 17:00:00", NA))










################################################################################
#                           MapSites                                           #
################################################################################

lat <- c(43.49990645, 43.53794259, 43.56064835, 43.53712928, 43.5343044)
long <- c(-113.0536099, -113.0145484, -112.9721448, -112.9511078, -112.939588)
site_no <- c(433000113031000, 433217113004908, 433338112581601,
             433214112570101, 433204112562001)
site_na <- c("02N 29E 18DBB1", "03N 29E 33CCC1 MIDDLE 2051 QA PORT 775.4",
             "03N 29E 26BCA1 ICPP-MON-A-164B", "3N 29E 36CDC1 CFA 1932",
             "02N 29E 01AAC1 CFA")
d <- as.data.frame(cbind(lat, long, site_no, site_na))
MapSites(d, "lat", "long", "site_no")






###

require(googleVis)
data(Andrew)
M1 <- gvisMap(data=Andrew, locationvar="LatLong" , tipvar="Tip",
              options=list(showTip=TRUE, showLine=FALSE, enableScrollWheel=TRUE,
                           mapType='terrain', useMapTypeControl=TRUE,
                           width="100%", height="100%"))
plot(M1)




####################


d <- QueryDatabase(con="NWIS Idaho", sqtable="sitefile_01",
               sqvars=c("site_no", "dec_lat_va", "dec_long_va"))


### dput(d, file=paste(getwd(), "/d.txt", sep=""))
### d <- dget(file=paste(getwd(), "/d.txt", sep=""))





d <- QueryDatabase(con="NWIS Idaho", sqtable="sitefile_01",
               sqvars=c("dec_lat_va", "dec_long_va", "site_no", "station_nm",
                        "agency_cd", "site_tp_cd"),
               site.no.var=NULL, site.no=NA,
               site.tp.cd.var="site_tp_cd", site.tp.cd="GW",
               lng.var="dec_long_va", lng.lim=c(-114.00, -112.00),
               lat.var="dec_lat_va", lat.lim=c(43.00, 44.00))

MapSites(d, "dec_lat_va", "dec_long_va", "site_no", "station_nm", "agency_cd")








####################


RestoreSession()
ConfigConnection()

