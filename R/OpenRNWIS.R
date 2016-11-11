#' Open Main Graphical User Interface
#'
#' This function activates the main graphcial user interface (GUI) for the \pkg{RNWIS} package.
#'
#' @details All functions within the \pkg{RNWIS} package may be called from this GUI.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{ExploreDatabase}}, \code{\link{MapSites}}, \code{\link{QueryDatabase}}
#'
#' @keywords misc
#'
#' @import tcltk
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   OpenRNWIS()
#' }
#'

OpenRNWIS <- function() {


  CallFileDialogBox <- function(cmd, caption, defaultextension, filters) {
    args <- list(cmd, title=caption, parent=tt)
    if (!is.null(defaultextension))
      args <- c(args, defaultextension=defaultextension)
    if (!is.null(initialdir))
      args <- c(args, initialdir=initialdir)
    if (!is.null(filters)) {
      filters[] <- paste("{", filters, "}", sep="")
      filters <- apply(filters, 1, paste, collapse=" ")
      filters <- paste(paste("{", filters, "}", sep=""), collapse=" ")
      args <- c(args, filetypes=filters)
    }
    file <- tclvalue(do.call(tcl, args))
    if (nzchar(file)) {
      initialdir <<- dirname(file)
      return(file)
    }
  }


  SaveFile <- function(type) {
    if (type == "Data") {
      caption <- "Save As"
      defaultextension <- "txt"
      filters <- rbind(c("Text Files", ".txt"),
                       c("Compressed Text Files", ".gz"),
                       c("ESRI Shapefiles", ".shp"),
                       c("All files", "*"))
    } else if (type == "Query") {
      caption <- "Save Project As"
      defaultextension <- "rda"
      filters <- rbind(c("Project Files", ".rda"),
                       c("All files", "*"))
    }
    CallFileDialogBox("tk_getSaveFile", caption, defaultextension, filters)
  }


  SaveQueryFile <- function(file) {
    if (missing(file) || is.null(file)) {
      file <- SaveFile("Query")
      if (is.null(file)) return()
      save.file <<- file
    }
    obj <- SaveQueryObject()
    save(obj, file=file, ascii=TRUE)
  }


  SaveQueryObject <- function() {
    obj <- list()
    obj$dsn.sel       <- as.integer(tcl(f1.box.1.2, "current"))
    obj$site.type.sel <- as.integer(tcl(f3.box.2.5, "current"))
    obj$agency.sel    <- as.integer(tcl(f3.box.3.5, "current"))
    obj$data.type.sel <- as.integer(tcl(f4.box.4.3, "current"))
    obj$opt           <- tclvalue(opt.var)
    obj$site.no       <- tclvalue(site.no.var)
    obj$site.file     <- tclvalue(site.file.var)
    obj$poly.file     <- tclvalue(poly.file.var)
    obj$lng.min       <- tclvalue(lng.min.var)
    obj$lng.max       <- tclvalue(lng.max.var)
    obj$lat.min       <- tclvalue(lat.min.var)
    obj$lat.max       <- tclvalue(lat.max.var)
    obj$alt.min       <- tclvalue(alt.min.var)
    obj$alt.max       <- tclvalue(alt.max.var)
    obj$date.time     <- tclvalue(date.time.var)
    obj$tmin          <- tclvalue(tmin.var)
    obj$tmax          <- tclvalue(tmax.var)
    obj$retr.vars     <- retr.vars
    obj$save.file     <- save.file
    obj
  }


  # open db connection and query tables/variables
  OpenConnection <- function() {
    idx <- as.integer(tcl(f1.box.1.2, "current"))
    if (idx < 0) return()
    tkconfigure(tt, cursor="watch")

    dsn <- as.character(tclvalue(dsn.var))
    channel <<- RODBC::odbcConnect(dsn, uid="", pwd="", readOnlyOptimize=TRUE)

    tkfocus(force=tt)
    tclServiceMode(FALSE)

    site.vars <<- NULL
    data.vars <<- NULL
    retr.vars <<- NULL

    # clear gui
    tcl("lset", site.var, "")
    tcl("lset", data.var, "")
    tcl("lset", retr.var, "")
    tclvalue(data.type.var) <- ""
    tclvalue(date.time.var) <- ""
    tkconfigure(f4.box.4.3, value="")
    tkconfigure(f6.box.1.2, value="")
    tkconfigure(f1.but.1.3, state="disabled")

    # update gui
    if (channel >= 0L && inherits(channel, "RODBC")) {
      tables <- RODBC::sqlTables(channel, errors=FALSE, as.is=TRUE)[, "TABLE_NAME"]
      if (site.table %in% tables) {

        # establish table types
        types <- names(data.tables)[sapply(data.tables, function(i) i) %in% tables]
        tkconfigure(f4.box.4.3, values=types)
        tcl(f4.box.4.3, "current", 0)

        # update site variables
        tkconfigure(f1.but.1.3, state="normal")
        server.name <- RODBC::odbcGetInfo(channel)[["Server_Name"]]
        sq.table <- paste(server.name, site.table, sep=".")
        sel <- RODBC::sqlColumns(channel, sqtable=sq.table)
        sel <- sel[sel[, "TABLE_NAME"] == site.table, "COLUMN_NAME"]
        site.vars <<- unique(sel)
        for (i in seq(along=site.vars))
          tcl("lappend", site.var, site.vars[i])

        UpdateDataVariables()
      } else {
        channel <<- NULL
      }
    } else {
      channel <<- NULL
    }

    RODBC::odbcCloseAll()

    tkconfigure(tt, cursor="arrow")
    tclServiceMode(TRUE)
  }


  # set state for site options
  SetState <- function() {
    opt <- as.integer(tclvalue(opt.var))

    s <- if (opt == 1L) "normal" else "disabled"
    tkconfigure(f2.ent.2.1, state=s)

    s <- if (opt == 2L) "normal" else "disabled"
    tkconfigure(f2.ent.4.1, state=s)
    tkconfigure(f2.but.4.2, state=s)

    s <- if (opt == 3L) "normal" else "disabled"
    tkconfigure(f3.ent.2.2, state=s)
    tkconfigure(f3.ent.2.3, state=s)
    tkconfigure(f3.ent.2.4, state=s)
    tkconfigure(f3.ent.3.2, state=s)
    tkconfigure(f3.ent.3.3, state=s)
    tkconfigure(f3.ent.3.4, state=s)
    tkconfigure(f3.ent.5.2, state=s)
    tkconfigure(f3.but.5.6, state=s)

    s <- if (opt == 3L) "readonly" else "disabled"
    tkconfigure(f3.box.2.5, state=s)
    tkconfigure(f3.box.3.5, state=s)
  }


  UpdateDataVariables <- function() {
    if (!inherits(channel, "RODBC")) return()

    tclServiceMode(FALSE)

    data.table <- data.tables[[as.character(tclvalue(data.type.var))]]

    channel <- RODBC::odbcReConnect(channel)
    server.name <- RODBC::odbcGetInfo(channel)[["Server_Name"]]
    sq.table <- paste(server.name, data.table, sep=".")
    sel <- RODBC::sqlColumns(channel, sqtable=sq.table)
    RODBC::odbcCloseAll()

    sel <- sel[sel[, "TABLE_NAME"] == data.table, ]
    data.vars <<- sel[, "COLUMN_NAME"]
    data.types <- sel[, "TYPE_NAME"]

    if (!is.null(retr.vars)) {
      is.var <- retr.vars %in% c(site.vars, data.vars)
      retr.vars <<- retr.vars[is.var]
      tcl("lset", retr.var, "")
      for (i in seq(along=retr.vars)) tcl("lappend", retr.var, retr.vars[i])
    }

    tcl("lset", data.var, "")
    for (i in seq(along=data.vars)) tcl("lappend", data.var, data.vars[i])

    dt.vars <- data.vars[data.types == "DATE"]
    if (length(dt.vars) > 0) {
      tkconfigure(f6.box.1.2, values=dt.vars)
      tcl(f6.box.1.2, "current", 0)
    }

    tclServiceMode(TRUE)
  }


  OpenQueryObject <- function(obj) {
    tcl(f1.box.1.2, "current", obj$dsn.sel)
    OpenConnection()
    tclvalue(opt.var) <- obj$opt
    SetState()
    tclvalue(site.no.var)   <- obj$site.no
    tclvalue(site.file.var) <- obj$site.file
    tclvalue(poly.file.var) <- obj$poly.file
    tclvalue(lng.min.var)   <- obj$lng.min
    tclvalue(lng.max.var)   <- obj$lng.max
    tclvalue(lat.min.var)   <- obj$lat.min
    tclvalue(lat.max.var)   <- obj$lat.max
    tclvalue(alt.min.var)   <- obj$alt.min
    tclvalue(alt.max.var)   <- obj$alt.max
    tclvalue(tmin.var)      <- obj$tmin
    tclvalue(tmax.var)      <- obj$tmax
    save.file <<- obj$save.file
    tcl(f3.box.2.5, "current", obj$site.type.sel)
    tcl(f3.box.3.5, "current", obj$agency.sel)
    tcl(f4.box.4.3, "current", obj$data.type.sel)
    UpdateDataVariables()
    tclServiceMode(FALSE)
    tcl("lset", retr.var, "")
    if (!is.null(obj$retr.vars)) {
      is.retr <- obj$retr.vars %in% c(site.vars, data.vars)
      retr.vars <<- obj$retr.vars[is.retr]
      for (i in seq(along=retr.vars)) tcl("lappend", retr.var, retr.vars[i])
    }
    cur.vals <- as.character(tkcget(f6.box.1.2, "-values"))
    if (obj$date.time %in% cur.vals)
      tcl(f6.box.1.2, "set", obj$date.time)
    tclServiceMode(TRUE)
  }


  OpenFile <- function(type, obj) {
    if (type == "Sites") {
      caption <- "Open Site Number File"
      defaultextension <- NULL
      filters <- NULL
    } else if (type == "Polygon") {
      caption <- "Open Polygon File"
      defaultextension <- "ply"
      filters <- rbind(c("Polygon Text Files", ".ply"), c("All files", "*"))
    } else if (type == "Query") {
      caption <- "Open Project File"
      defaultextension <- "rda"
      filters <- rbind(c("Project Files", ".rda"), c("All files", "*"))
    }
    f <- CallFileDialogBox("tk_getOpenFile", caption, defaultextension, filters)
    if (missing(obj)) {
      return(f)
    } else {
      tclvalue(obj) <- f
    }
  }


  OpenQueryFile <- function() {
    file <- OpenFile("Query")
    if (is.null(file)) return()
    obj <- NULL
    load(file)
    OpenQueryObject(obj)
  }


  CloseGUI <- function() {
    tclServiceMode(FALSE)
    if (as.integer(tclvalue(tt.done.var)) != 0) return()
    RODBC::odbcCloseAll()
    tclvalue(tt.done.var) <- 1
    tkdestroy(tt)
    tclServiceMode(TRUE)
  }


  # convert image bits to the image data string format
  BitsToString <- function(bits) {
    n <- length(bits) / 2
    b <- paste(bits, collapse=", ")
    sprintf("#define v_width %d\n#define v_height %d\nstatic unsigned char v_bits[] = { %s }; ",
            n, n, b)
  }


  # add variables to retrieval list
  AddVariables <- function() {
    site.idxs <- as.integer(tkcurselection(f4.lst.2.1))
    data.idxs <- as.integer(tkcurselection(f4.lst.2.3))

    ids <- NULL
    if (length(site.idxs) > 0)
      for (i in site.idxs)
        ids <- c(ids, as.character(tkget(f4.lst.2.1, i, i)))
    if (length(data.idxs) > 0)
      for (i in data.idxs)
        ids <- c(ids, as.character(tkget(f4.lst.2.3, i, i)))

    if (is.null(ids)) return()

    tkselection.clear(f4.lst.2.1, 0, "end")
    tkselection.clear(f4.lst.2.3, 0, "end")

    for (i in ids) {
      if (!i %in% retr.vars) {
        tcl("lappend", retr.var, i)
        retr.vars <<- c(retr.vars, i)
      }
    }
  }


  # remove selected variables from retrieval list
  RemoveVariables <- function() {
    idxs <- as.integer(tkcurselection(f4.lst.2.6))
    if (length(idxs) == 0) return()
    tkselection.clear(f4.lst.2.6, 0, "end")
    retr.vars <<- retr.vars[-(idxs + 1)]
    tcl("lset", retr.var, "")
    for (i in retr.vars) tcl("lappend", retr.var, i)
  }


  # copy variables from retrieval list into Windows clipboard
  CopyVariables <- function() {
    idxs <- as.integer(tkcurselection(f4.lst.2.6))
    s <- ""
    if (length(idxs) != 0) s <- paste(retr.vars[idxs + 1], collapse="\n")
    utils::writeClipboard(s, format=1)
  }


  # paste variables from clipboard into retrieval list
  PasteVariables <- function() {
    s <- utils::readClipboard(format=1, raw=FALSE)
    if (is.null(s)) return()
    is.var <- s %in% c(site.vars, data.vars) & !s %in% retr.vars
    add.vars <- s[is.var]
    for (i in seq(along=add.vars)) {
      retr.vars <<- c(retr.vars, add.vars[i])
      tcl("lappend", retr.var, add.vars[i])
    }
  }


  # arrange variables in listbox
  Arrange <- function(type) {
    sel.idxs <- as.integer(tkcurselection(f4.lst.2.6)) + 1
    if (length(sel.idxs) == 0) return()
    n <- length(retr.vars)
    idxs <- seq_len(n)

    if (type == "top") {
      idxs <- c(sel.idxs, idxs[-sel.idxs])
    } else if (type == "bottom") {
      idxs <- c(idxs[-sel.idxs], sel.idxs)
    } else if (type == "up") {
      for (i in sel.idxs) {
        if (i == 1 || idxs[i - 1] %in% sel.idxs) next
        idxs[c(i - 1, i)] <- c(i, idxs[i - 1])
      }
    } else if (type == "down") {
      for (i in rev(sel.idxs)) {
        if (i == n || idxs[i + 1] %in% sel.idxs) return()
        idxs[c(i, i + 1)] <- c(idxs[i + 1], i)
      }
    }

    retr.vars <<- retr.vars[idxs]
    for (i in 1:n) tclvalue(retr.var) <- tcl("lreplace", tclvalue(retr.var),
                                             i - 1, i - 1, retr.vars[i])
    tkselection.clear(f4.lst.2.6, 0, "end")
    for (i in which(idxs %in% sel.idxs)) tkselection.set(f4.lst.2.6, i - 1)
  }


  CallMapSites <- function(vars) {
    if (!inherits(channel, "RODBC")) {
      ShowErrorMessage("01")
      return()
    }
    sqvars <- c(vars$lat, vars$lng, vars$alt, vars$site, vars$name, vars$agency, vars$type)
    data <- GetSiteInfo(sqvars)
    names(data$sites) <- names(vars)
    tkconfigure(tt, cursor="watch")
    MapSites(data$sites, data$polygons, map.id=sprintf("%02d", map.idx))
    tkconfigure(tt, cursor="arrow")
    map.idx <<- map.idx + 1
  }


  RetrieveData <- function() {
    if (!inherits(channel, "RODBC")) {
      ShowErrorMessage("01")
      return()
    }
    if (is.null(retr.vars) || length(retr.vars) == 0) {
      ShowErrorMessage("02")
      return()
    }
    tkconfigure(tt, cursor="watch")

    # determine site variables in retrieval list
    is.site <- retr.vars %in% site.vars

    # construct site table and identify site numbers
    sqvars <-  unique(c(vars$site, retr.vars[is.site]))
    d.site <- GetSiteInfo(sqvars)
    if (is.null(d.site)) {
      ShowErrorMessage("04")
      return()
    }
    d.site <- d.site$sites
    site.no <- d.site[, vars$site]

   # date and time range
    d.t.lim <- c(NA, NA)
    d.t.var <- as.character(tclvalue(date.time.var))
    if (d.t.var != "")
      d.t.lim <- c(ConvertEntryToDate(tmin.var), ConvertEntryToDate(tmax.var))
    else
      d.t.var <- NULL

    # construct data table
    d.data <- NULL
    sqvars <- unique(c(vars$site, retr.vars[!is.site]))
    data.table <- data.tables[[as.character(tclvalue(data.type.var))]]

    if (length(sqvars) > 1) {

      # maintain a reasonable number of site numbers for each query;
      # an error results from a query size that's too large
      n <- length(site.no)
      inc <- seq(0, n, by=200)
      if (n != inc[length(inc)]) inc <- c(inc, n)

      # query database
      channel <- RODBC::odbcReConnect(channel)
      for (i in seq(along=inc[-1])) {
        site.numbers <- site.no[inc[i]:inc[i + 1]]
        sel <- QueryDatabase(channel,
                             sqtable=data.table,
                             sqvars=sqvars,
                             site.no.var=vars$site,
                             site.no=site.numbers,
                             d.t.var=d.t.var,
                             d.t.lim=d.t.lim)
        if (inherits(sel, "character")) {
          ShowErrorMessage("06", d.data[1])
          stop()
        }
        d.data <- rbind(d.data, sel)
      }
      RODBC::odbcCloseAll()

      if (nrow(d.data) == 0L) d.data <- NULL
    }

    # merge site and data tables
    if (is.null(d.data))
      d <- d.site
    else
      d <- merge(d.site, d.data, by=vars$site)

    # save data to file
    f <- SaveFile("Data")
    retr.vars <- retr.vars[retr.vars %in% names(d)]

    if (!is.null(f)) {
      if (grepl(".shp$", tolower(f))) {
        col.names <- unique(c(vars$lng, vars$lat, retr.vars))
        d <- d[, col.names]

        # columns of class POSIXct are not permitted, convert to character strings
        col.class <- sapply(names(d), function(i) class(d[, i])[1])
        dt.idxs <- which(col.class == "POSIXct")
        if (length(dt.idxs) > 0)
          d[, dt.idxs] <- as.character(format(d[, dt.idxs]))

        # column names are finicky for shapefiles, rules are convoluted, 8-bit names and no periods
        col.names <- gsub("\\.", "", make.names(substr(col.names, 1, 7), unique=TRUE))

        colnames(d) <- col.names
        sp::coordinates(d) <- col.names[1:2]
        sp::proj4string(d) <- sp::CRS("+init=epsg:3857")

        rgdal::writeOGR(obj=d, dsn=dirname(f), driver="ESRI Shapefile",
                        layer=sub(paste(".shp$", sep=""), "", basename(f)),
                        verbose=TRUE)
      } else {
        d <- d[, retr.vars]

        if (grepl(".gz$", tolower(f)))
          con <- gzfile(description=f, open="w")
        else
          con <- file(description=f, open="w")

        utils::write.table(d, file=con, sep="\t", quote=FALSE, row.names=FALSE)
        close(con)
      }
    }
    tkconfigure(tt, cursor="arrow")
  }


  # convert tk entry to date-time value
  ConvertEntryToDate <- function(tk.ent) {
    string <- as.character(tclvalue(tk.ent))
    if (string == "") return(NA)
    fmt <- "%Y-%m-%d"
    if (length(strsplit(string, " ")[[1]]) > 1) fmt <- paste(fmt, "%H:%M")
    date <- as.POSIXct(string, format=fmt)
    if (is.na(date)) ShowErrorMessage("05", string)
    date
  }


  # convert site strings to vector of site numbers
  CleanSiteStrings <- function(s) {
    str.split <- unlist(strsplit(s, "[[:punct:][:space:]]"))
    int.split <- suppressWarnings(as.numeric(str.split))
    as.character(int.split[!is.na(int.split)])
  }

  # retrieve site data and polygon domain
  GetSiteInfo <- function(sqvars) {
    opt <- as.integer(tclvalue(opt.var))
    poly.obj <- NULL

    sqvars <- unique(c(vars$lat, vars$lng, sqvars))

    # reconnect to db
    channel <- RODBC::odbcReConnect(channel)

    # base query on site numbers
    if (opt == 1L | opt == 2L) {

      if (opt == 1L) { # get site number(s) in entry box
        site.no <- as.character(tclvalue(site.no.var))
        if (site.no == "") {
          ShowErrorMessage("03")
          stop()
        }
        site.no <- CleanSiteStrings(site.no)
      } else { # read site number(s) from file
        site.file <- as.character(tclvalue(site.file.var))
        if (site.file == "" || file.access(site.file, mode=0) < 0) {
          ShowErrorMessage("03")
          return()
        }
        scanned.strings <- scan(file=site.file, what="character",
                                comment.char="#", quiet=TRUE)
        site.no <- CleanSiteStrings(paste(scanned.strings, collapse=","))
      }

      if (length(site.no) == 0L) {
        ShowErrorMessage("03")
        return()
      }

      # query database
      sel <- QueryDatabase(channel, sqtable=site.table, sqvars=sqvars,
                           site.no.var=vars$site, site.no=site.no)

    # base query on site attributes
    } else if (opt == 3L) {

      # spatial limits
      lat.min <- suppressWarnings(as.numeric(tclvalue(lat.min.var)))
      lat.max <- suppressWarnings(as.numeric(tclvalue(lat.max.var)))
      lng.min <- suppressWarnings(as.numeric(tclvalue(lng.min.var)))
      lng.max <- suppressWarnings(as.numeric(tclvalue(lng.max.var)))
      alt.min <- suppressWarnings(as.numeric(tclvalue(alt.min.var)))
      alt.max <- suppressWarnings(as.numeric(tclvalue(alt.max.var)))

      # account for spatial limits imposed by polygon bounding box
      poly.obj <- NULL
      poly.file <- as.character(tclvalue(poly.file.var))
      if (poly.file != "" && file.access(poly.file, mode=0) == 0) {
        poly.obj <- rgeos::read.polyfile(poly.file, nohole=FALSE)
        if (inherits(poly.obj, "gpc.poly")) {
          poly.bbox <- rgeos::get.bbox(poly.obj)
          lat.min <- min(lat.min, poly.bbox$y[1], na.rm=TRUE)
          lat.max <- max(lat.max, poly.bbox$y[2], na.rm=TRUE)
          lng.min <- min(lng.min, poly.bbox$x[1], na.rm=TRUE)
          lng.max <- max(lng.max, poly.bbox$x[2], na.rm=TRUE)
        } else {
          poly.obj <- NULL
        }
      }

      # site types
      site.type <- as.character(tclvalue(site.type.var))
      if (site.type %in% names(site.types))
        site.type.codes <- site.types[[site.type]]
      else
        site.type.codes <- NULL

      # agency
      agency <- as.character(tclvalue(agency.var))
      if (agency %in% names(agencies))
        agency.codes <- agencies[[agency]]
      else
        agency.codes <- NULL

      # query database
      sel <- QueryDatabase(channel,
                           sqtable=site.table,
                           sqvars=sqvars,
                           site.tp.cd.var=vars$type,
                           site.tp.cd=site.type.codes,
                           agency.cd.var=vars$agency,
                           agency.cd=agency.codes,
                           lng.var=vars$lng,
                           lng.lim=c(lng.min, lng.max),
                           lat.var=vars$lat,
                           lat.lim=c(lat.min, lat.max),
                           alt.var=vars$alt,
                           alt.lim=c(alt.min, alt.max))
      if (inherits(sel, "character")) {
        ShowErrorMessage("06", sel[1])
        stop()
      }

      # convert decimal degree latitude and longitude from NAD83 to WGS84
      xy.names <- c(vars$lng, vars$lat)
      xy <- sel[, xy.names]
      sp::coordinates(xy) <- xy.names
      sp::proj4string(xy) <- sp::CRS("+proj=longlat +datum=NAD83")
      sel[, xy.names] <- as.data.frame(sp::spTransform(xy, sp::CRS("+init=epsg:3857")))

      # only permit sites inside polygon spatial domain
      if (!is.null(poly.obj)) {
        poly.pts <- rgeos::get.pts(poly.obj)
        for (i in seq(along=poly.pts)) {
          x <- sel[, vars$lng]
          y <- sel[, vars$lat]
          poly.x <- poly.pts[[i]]$x
          poly.y <- poly.pts[[i]]$y
          is.inside <- sp::point.in.polygon(point.x=x, point.y=y,
                                            pol.x=poly.x, pol.y=poly.y)
          if (poly.pts[[i]]$hole)
            sel <- sel[is.inside != 1, ]
          else
            sel <- sel[is.inside != 0, ]
        }
      }
    }

    # close database connection
    RODBC::odbcCloseAll()

    # catergorize site types
    if ("site_tp_cd" %in% names(sel)) {
      tp <- sel[, "site_tp_cd"]
      sel[, "site_tp_cd"] <- NA
      site.category <- list(groundwater="GW", "surface water"="SW",
                            spring="SP", atmospheric="AT")
      for (i in names(site.types))
        sel[tp %in% site.types[[i]], "site_tp_cd"] <- site.category[[i]]
    }

    # return selection
    if (inherits(sel, "data.frame") && nrow(sel) > 0) {
      return(list(sites=sel[, sqvars], polygons=poly.obj))
    } else {
      ShowErrorMessage("04")
      stop(sel)
    }
  }


  # show error message dialog box
  ShowErrorMessage <- function(code, detail=NULL) {
    err <- err[[code]]
    arg <- list("tk_messageBox", parent=tt, default="ok",
                icon=err[1], type=err[2], title=err[3], message=err[4])
    if (!is.null(detail)) arg <- c(arg, detail=detail)
    ans <- tclvalue(do.call(tcl, arg))
    tkconfigure(tt, cursor="arrow")
    tclServiceMode(TRUE)
    as.character(ans)
  }


  AboutPackage <- function() {
    if ("package:RNWIS" %in% search())
      path <- system.file("DESCRIPTION", package="RNWIS")
    else
      path <- paste(getwd(), "/DESCRIPTION", sep="")
    msg <- paste(readLines(path, n=-1L), collapse="\n")
    tkmessageBox(icon="info", message=msg, title="About", parent=tt)
  }


  # open html help for r functions
  OpenHTMLHelp <- function() {
    if (!("RNWIS" %in% rownames(utils::installed.packages()))) return()
    utils::help(package="RNWIS", help_type="html")
    invisible()
  }


  # main program

  # variables specific to nwis:
  #   The decimal degree latitude and longitude (attributes dec_lat_va and
  #   dec_long_va), are stored to a common datum (NAD83). The degrees, minutes,
  #   and seconds version of latitude and longitude (attributes
  #   lat_va and long_va) are stored in the datum that the user entered the
  #   values in. If the lat_va and long_va are retrieved it is recommended
  #   that the attribute coordinate datum (coord_datum_cd) be retrieved as well.
  #   There is no guarantee that all sites have been entered in a consistent datum.
  #   Altitudes (alt_va) are stored in the NGVD29 or NAVD88 datus; RNWIS
  #   does not convert altitudes to the Google Maps WGS84 EGM96 vertical
  #   datum (this may be possible with future versions of PROJ.4).

  site.table <- "SITEFILE_01"

  data.tables <- list("groundwater levels"="GW_LEV_01",
                      "hole construction"="GW_HOLE_01",
                      "casing construction"="GW_CSNG_01",
                      "openings construction"="GW_OPEN_01")

  site.types <- list(groundwater=c("GW", "GW-CR", "GW-EX", "GW-HZ", "GW-IW", "GW-MW", "GW-TH"),
                     "surface water"=c("ST", "ST-CA", "ST-DCH", "ST-TS", "LK"),
                     spring="SP", atmospheric="AT")

  agencies <- list(USGS="USGS", EPA="USEPA")

  vars <- list(lat="DEC_LAT_VA", lng="DEC_LONG_VA", alt="ALT_VA", site="SITE_NO",
               name="STATION_NM", agency="AGENCY_CD", type="SITE_TP_CD")

  # error and warning messages: [icon, type, title, message]
  err <- list()
  err[["01"]] <- c("warning", "ok", "Missing Database Connection",
                   "Select a registerd ODBC data source")
  err[["02"]] <- c("warning", "ok", "Missing Retrieval Variables",
                   "Add variables to retrieval list.")
  err[["03"]] <- c("error", "ok", "Missing Site Numbers",
                   "Site numbers missing or improperly formatted.")
  err[["04"]] <- c("error", "ok", "Empty Database Query",
                   "Query of site table returned no data.")
  err[["05"]] <- c("warning", "ok", "Date and Time Conversion",
                   "Character string could not be converted to date object.")
  err[["06"]] <- c("error", "ok", "Query Error",
                   "Query resulted in error.")
  err[["07"]] <- c("error", "ok", "Empty Database Query",
                   "Query of data table returned no data.")

  # initialize top-level variables
  channel    <- NULL
  site.vars  <- NULL
  data.vars  <- NULL
  retr.vars  <- NULL
  initialdir <- NULL
  save.file  <- NULL
  map.idx    <- 1L

  # assign variables linked to tk widgets
  dsn.var       <- tclVar()

  opt.var       <- tclVar(1)
  site.no.var   <- tclVar()
  site.file.var <- tclVar()
  poly.file.var <- tclVar()
  site.type.var <- tclVar()
  agency.var    <- tclVar()
  lng.min.var   <- tclVar()
  lng.max.var   <- tclVar()
  lat.min.var   <- tclVar()
  lat.max.var   <- tclVar()
  alt.min.var   <- tclVar()
  alt.max.var   <- tclVar()

  data.type.var <- tclVar()
  site.var      <- tclVar()
  data.var      <- tclVar()
  retr.var      <- tclVar()

  date.time.var <- tclVar()
  tmin.var      <- tclVar()
  tmax.var      <- tclVar()

  tt.done.var   <- tclVar(0)

  # create image bitmaps for buttons, based on arrows.tcl
  # by Keith Vetter, http://wiki.tcl.tk/8554
  bits <- c("0x00", "0x00", "0x06", "0x03", "0x8e", "0x03", "0xdc", "0x01",
            "0xf8", "0x00", "0x70", "0x00", "0xf8", "0x00", "0xdc", "0x01",
            "0x8e", "0x03", "0x06", "0x03", "0x00", "0x00")
  img.del <- tkimage.create("bitmap", data=as.tclObj(BitsToString(bits)))

  bits <- c("0x00", "0x00", "0x20", "0x00", "0x60", "0x00", "0xe0", "0x00",
            "0xfc", "0x01", "0xfc", "0x03", "0xfc", "0x01", "0xe0", "0x00",
            "0x60", "0x00", "0x20", "0x00", "0x00", "0x00")
  img.right <- tkimage.create("bitmap", data=as.tclObj(BitsToString(bits)))

  bits <- c("0x00", "0x00", "0x20", "0x00", "0x70", "0x00", "0xf8", "0x00",
            "0xfc", "0x01", "0xfe", "0x03", "0x70", "0x00", "0x70", "0x00",
            "0x70", "0x00", "0x00", "0x00", "0x00", "0x00")
  img.up <- tkimage.create("bitmap", data=as.tclObj(BitsToString(bits)))

  bits <- c("0x00", "0x00", "0x00", "0x00", "0x70", "0x00", "0x70", "0x00",
            "0x70", "0x00", "0xfe", "0x03", "0xfc", "0x01", "0xf8", "0x00",
            "0x70", "0x00", "0x20", "0x00", "0x00", "0x00")
  img.down <- tkimage.create("bitmap", data=as.tclObj(BitsToString(bits)))

  bits <- c("0x00", "0x00", "0xfe", "0x03", "0xfe", "0x03", "0x20", "0x00",
            "0x70", "0x00", "0xf8", "0x00", "0xfc", "0x01", "0xfe", "0x03",
            "0x70", "0x00", "0x70", "0x00", "0x70", "0x00")
  img.top <- tkimage.create("bitmap", data=as.tclObj(BitsToString(bits)))

  bits <- c("0x70", "0x00", "0x70", "0x00", "0x70", "0x00", "0xfe", "0x03",
            "0xfc", "0x01", "0xf8", "0x00", "0x70", "0x00", "0x20", "0x00",
            "0xfe", "0x03", "0xfe", "0x03", "0x00", "0x00")
  img.bottom <- tkimage.create("bitmap", data=as.tclObj(BitsToString(bits)))

  bits <- c("0x00", "0x00", "0x7e", "0x00", "0x42", "0x00", "0xf2", "0x03",
            "0x12", "0x02", "0x12", "0x02", "0x12", "0x02", "0x1e", "0x02",
            "0x10", "0x02", "0xf0", "0x03", "0x00", "0x00")
  img.copy <- tkimage.create("bitmap", data=as.tclObj(BitsToString(bits)))

  bits <- c("0x10", "0x00", "0xfe", "0x00", "0xba", "0x00", "0x82", "0x00",
            "0xe2", "0x07", "0xe2", "0x07", "0xe2", "0x07", "0xe2", "0x07",
            "0xfe", "0x07", "0xe0", "0x07", "0xe0", "0x07")
  img.paste <- tkimage.create("bitmap", data=as.tclObj(BitsToString(bits)))

  # open gui
  tclServiceMode(FALSE)
  tt <- tktoplevel(padx=0, pady=0)
  tktitle(tt) <- "National Water Information System: R Interface"

  # create pull-down menus
  top.menu <- tkmenu(tt, tearoff=0)

  menu.file <- tkmenu(tt, tearoff=0, relief="flat")
  tkadd(top.menu, "cascade", label="File", menu=menu.file, underline=0)
  tkadd(menu.file, "command", label="Open", accelerator="Ctrl+O",
        command=function() OpenQueryFile())
  tkadd(menu.file, "command", label="Save", accelerator="Ctrl+S",
        command=function() SaveQueryFile(save.file))
  tkadd(menu.file, "command", label="Save as", accelerator="Shift+Ctrl+S",
        command=function() SaveQueryFile())
  tkadd(menu.file, "separator")
  tkadd(menu.file, "command", label="Exit",
        command=CloseGUI)

  menu.help <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Help", menu=menu.help, underline=0)
  tkadd(menu.help, "command", label="R functions (html)",
        command=OpenHTMLHelp)
  tkadd(menu.help, "separator")
  tkadd(menu.help, "command", label="About",
        command=AboutPackage)

  tkconfigure(tt, menu=top.menu)

  # frame 0, map and export buttons
  f0 <- ttkframe(tt, relief="flat")

  f0.but.1 <- ttkbutton(f0, width=15, text="Map Sites",
                        command=function() CallMapSites(vars))
  f0.but.2 <- ttkbutton(f0, width=15, text="Retrieve Data",
                        command=RetrieveData)

  f0.grp.3 <- ttksizegrip(f0)

  tkgrid(f0.but.1, f0.but.2, f0.grp.3)

  tkgrid.configure(f0.but.1, f0.but.2, sticky="e", padx=2, pady=c(12, 10))
  tkgrid.configure(f0.grp.3, sticky="se")

  tkpack(f0, side="bottom", anchor="e")

  # frame 1, odbc db source name
  f1 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3,
                      text="Select a registerd ODBC data source")

  f1.lab.1.1 <- ttklabel(f1, text="Source name")
  f1.box.1.2 <- ttkcombobox(f1, textvariable=dsn.var, state="readonly")
  f1.but.1.3 <- ttkbutton(f1, width=8, text="Explore",
                          command=function() ExploreDatabase(channel, tt))

  tkgrid(f1.lab.1.1, f1.box.1.2, f1.but.1.3, padx=c(0, 2), pady=3, sticky="we")
  tkgrid.configure(f1.lab.1.1, sticky="e")
  tkgrid.configure(f1.but.1.3, padx=0)

  tkgrid.columnconfigure(f1, 1, weight=1, minsize=25)

  tkpack(f1, fill="x", expand=FALSE, ipadx=2, ipady=2, padx=8, pady=8)

  tkconfigure(f1.box.1.2, value=names(RODBC::odbcDataSources()))

  tkconfigure(f1.but.1.3, state="disabled")

  # frame 2 and 3, sites
  txt <- "Select sites based on one of the following options"
  f2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3, text=txt)

  f2.rad.1.1 <- ttkradiobutton(f2, variable=opt.var, value=1,
                               command=SetState, text="Site number(s):")
  f2.rad.3.1 <- ttkradiobutton(f2, variable=opt.var, value=2,
                               command=SetState, text="File of site numbers:")
  f2.rad.5.1 <- ttkradiobutton(f2, variable=opt.var, value=3,
                               command=SetState, text="Site attributes:")

  f2.ent.2.1 <- ttkentry(f2, width=25, textvariable=site.no.var)
  f2.ent.4.1 <- ttkentry(f2, width=25, textvariable=site.file.var)

  f2.but.4.2 <- ttkbutton(f2, width=8, text="Browse",
                          command=function() OpenFile("Sites", site.file.var))

  f3 <- ttkframe(f2, relief="flat")

  f3.lab.1.2 <- ttklabel(f3, justify="center", text="Latitude\n(WGS84)")
  f3.lab.1.3 <- ttklabel(f3, justify="center", text="Longitude\n(WGS84)")
  f3.lab.1.4 <- ttklabel(f3, justify="center", text="Altitude\nin feet")
  f3.lab.1.5 <- ttklabel(f3, justify="center", text="")
  f3.lab.2.1 <- ttklabel(f3, text="Minimum")
  f3.lab.3.1 <- ttklabel(f3, text="Maximum")

  f3.ent.2.3 <- ttkentry(f3, width=15, textvariable=lng.min.var)
  f3.ent.3.3 <- ttkentry(f3, width=15, textvariable=lng.max.var)

  f3.ent.2.2 <- ttkentry(f3, width=15, textvariable=lat.min.var)
  f3.ent.3.2 <- ttkentry(f3, width=15, textvariable=lat.max.var)

  f3.ent.2.4 <- ttkentry(f3, width=15, textvariable=alt.min.var)
  f3.ent.3.4 <- ttkentry(f3, width=15, textvariable=alt.max.var)

  f3.box.2.5 <- ttkcombobox(f3, state="readonly", width=15, textvariable=site.type.var)
  f3.box.3.5 <- ttkcombobox(f3, state="readonly", width=15, textvariable=agency.var)
  tkconfigure(f3.box.2.5, values=c("All types ...", names(site.types)))
  tkconfigure(f3.box.3.5, values=c("All agencies ...", names(agencies)))
  tcl(f3.box.2.5, "current", 0)
  tcl(f3.box.3.5, "current", 0)

  f3.lab.4.1 <- ttklabel(f3, foreground="#414042", text="e.g.")
  f3.lab.4.2 <- ttklabel(f3, foreground="#414042", text="43.510023")
  f3.lab.4.3 <- ttklabel(f3, foreground="#414042", text="-112.980728")
  f3.lab.4.4 <- ttklabel(f3, foreground="#414042", text="4382.3")

  f3.lab.5.1 <- ttklabel(f3, text="Polygon")
  f3.ent.5.2 <- ttkentry(f3, width=25, textvariable=poly.file.var)
  f3.but.5.6 <- ttkbutton(f3, width=8, text="Browse",
                          command=function() OpenFile("Polygon", poly.file.var))

  tkgrid(f2.rad.1.1)
  tkgrid(f2.ent.2.1, columnspan=2, pady=c(0, 4))
  tkgrid(f2.rad.3.1)
  tkgrid(f2.ent.4.1, f2.but.4.2, pady=c(0, 4))
  tkgrid(f2.rad.5.1)

  tkgrid.configure(f2.rad.1.1, f2.rad.3.1, f2.rad.5.1,
                   sticky="w", columnspan=2)

  tkgrid.configure(f2.ent.2.1, sticky="we", padx=c(10, 0))
  tkgrid.configure(f2.ent.4.1, sticky="we", padx=c(10, 2))

  tkgrid(f3, columnspan=2, sticky="we")

  tkgrid("x", f3.lab.1.2, f3.lab.1.3, f3.lab.1.4, f3.lab.1.5, "x", pady=c(0, 1))

  tkgrid(f3.lab.2.1, f3.ent.2.2, f3.ent.2.3, f3.ent.2.4,
         f3.box.2.5, "x", padx=1, pady=c(0, 1), sticky="we")

  tkgrid(f3.lab.3.1, f3.ent.3.2, f3.ent.3.3, f3.ent.3.4,
         f3.box.3.5, "x", padx=1, pady=c(1, 0), sticky="we")

  tkgrid.configure(f3.lab.2.1, f3.lab.3.1, sticky="e", padx=c(10, 0))

  tkgrid(f3.lab.4.1, f3.lab.4.2, f3.lab.4.3, f3.lab.4.4)
  tkgrid.configure(f3.lab.4.1, sticky="e")

  tkgrid(f3.lab.5.1, f3.ent.5.2, "x", "x", "x", f3.but.5.6, pady=c(7, 0))

  tkgrid.configure(f3.lab.1.5, f3.box.2.5, f3.box.3.5, padx=c(10, 0), columnspan=2)

  tkgrid.configure(f3.lab.5.1, padx=c(10, 0), sticky="e")
  tkgrid.configure(f3.ent.5.2, columnspan=4, sticky="we", padx=c(1, 2))

  tkgrid.columnconfigure(f3, 1, weight=1, minsize=15)
  tkgrid.columnconfigure(f3, 2, weight=1, minsize=15)
  tkgrid.columnconfigure(f3, 3, weight=1, minsize=15)
  tkgrid.columnconfigure(f3, 4, weight=1, minsize=15)

  tkgrid.columnconfigure(f2, 0, weight=1, minsize=15)

  tkpack(f2, fill="x", ipadx=2, ipady=2, padx=8, pady=8, anchor="n")

  # frame 4 and 3, variables
  f4 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3,
                          text="Add variables to retrieval list")

  f4.lab.1.1 <- ttklabel(f4, text="Site variables")
  f4.lab.1.3 <- ttklabel(f4, text="Data variables")
  f4.lab.1.6 <- ttklabel(f4, text="Retrieve variables")
  f4.lab.4.1 <- ttklabel(f4, text="Type")

  f4.lst.2.1 <- tklistbox(f4, selectmode="extended", activestyle="none",
                          relief="flat", borderwidth=5, width=15, height=3,
                          exportselection=FALSE, listvariable=site.var,
                          highlightthickness=0)
  f4.lst.2.3 <- tklistbox(f4, selectmode="extended", activestyle="none",
                          relief="flat", borderwidth=5, width=15, height=3,
                          exportselection=FALSE, listvariable=data.var,
                          highlightthickness=0)
  f4.lst.2.6 <- tklistbox(f4, selectmode="extended", activestyle="none",
                          relief="flat", borderwidth=5, width=15, height=3,
                          exportselection=FALSE, listvariable=retr.var,
                          highlightthickness=0)

  f4.ysc.2.2 <- ttkscrollbar(f4, orient="vertical")
  f4.ysc.2.4 <- ttkscrollbar(f4, orient="vertical")
  f4.ysc.2.7 <- ttkscrollbar(f4, orient="vertical")

  tkconfigure(f4.lst.2.1, background="white",
              yscrollcommand=paste(.Tk.ID(f4.ysc.2.2), "set"))
  tkconfigure(f4.lst.2.3, background="white",
              yscrollcommand=paste(.Tk.ID(f4.ysc.2.4), "set"))
  tkconfigure(f4.lst.2.6, background="white",
              yscrollcommand=paste(.Tk.ID(f4.ysc.2.7), "set"))
  tkconfigure(f4.ysc.2.2, command=paste(.Tk.ID(f4.lst.2.1), "yview"))
  tkconfigure(f4.ysc.2.4, command=paste(.Tk.ID(f4.lst.2.3), "yview"))
  tkconfigure(f4.ysc.2.7, command=paste(.Tk.ID(f4.lst.2.6), "yview"))

  f4.but.2.5 <- ttkbutton(f4, width=2, image=img.right, command=AddVariables)

  f4.box.4.3 <- ttkcombobox(f4, state="readonly", textvariable=data.type.var, width=10)

  f5 <- ttkframe(f4, relief="flat")

  f5.but.1.1 <- ttkbutton(f5, width=2, image=img.top,
                          command=function() Arrange("top"))
  f5.but.1.2 <- ttkbutton(f5, width=2, image=img.up,
                          command=function() Arrange("up"))
  f5.but.1.3 <- ttkbutton(f5, width=2, image=img.down,
                          command=function() Arrange("down"))
  f5.but.1.4 <- ttkbutton(f5, width=2, image=img.bottom,
                          command=function() Arrange("bottom"))
  f5.but.1.5 <- ttkbutton(f5, width=2, image=img.copy,
                          command=CopyVariables)
  f5.but.1.6 <- ttkbutton(f5, width=2, image=img.paste,
                          command=PasteVariables)
  f5.but.1.7 <- ttkbutton(f5, width=2, image=img.del,
                          command=RemoveVariables)

  tkgrid(f4.lab.1.1, "x", f4.lab.1.3, "x", "x", f4.lab.1.6, "x", pady=c(0, 1))
  tkgrid(f4.lst.2.1, f4.ysc.2.2, f4.lst.2.3, f4.ysc.2.4,
         f4.but.2.5, f4.lst.2.6, f4.ysc.2.7)
  tkgrid(f4.lab.4.1, "x", f4.box.4.3, "x", "x", f5, "x")
  tkgrid.configure(f5, columnspan=1)
  tkgrid(f5.but.1.1, f5.but.1.2, f5.but.1.3, f5.but.1.4,
         f5.but.1.5, f5.but.1.6, f5.but.1.7, padx=c(0, 3), pady=c(3, 0))
  tkgrid.configure(f5.but.1.7, padx=0)

  tkgrid.configure(f4.lst.2.1, f4.ysc.2.2, f4.lst.2.3,
                   f4.ysc.2.4, f4.lst.2.6, f4.ysc.2.7)

  tkgrid.configure(f4.lst.2.1, f4.lst.2.3, f4.lst.2.6, sticky="nsew")
  tkgrid.configure(f4.ysc.2.2, sticky="ns", padx=c(0, 10))
  tkgrid.configure(f4.ysc.2.4, f4.ysc.2.7, sticky="ns", padx=0)

  tkgrid.configure(f4.but.2.5, padx=10, pady=c(0, 0))

  tkgrid.configure(f4.lab.4.1, sticky="e", padx=c(0, 1), pady=c(4, 0), columnspan=2)
  tkgrid.configure(f4.box.4.3, sticky="we", pady=c(4, 0), padx=0)

  tkgrid.columnconfigure(f4, 0, weight=1, minsize=15)
  tkgrid.columnconfigure(f4, 2, weight=1, minsize=15)
  tkgrid.columnconfigure(f4, 5, weight=1, minsize=15)

  tkgrid.rowconfigure(f4, 1, weight=1)

  tkpack(f4, fill="both", expand=TRUE, ipadx=2, ipady=2, padx=8, pady=8)

  # frame 6, date range
  f6 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3,
                      text="Specify date and time range")

  f6.lab.1.1 <- ttklabel(f6, text="Variable")
  f6.lab.1.3 <- ttklabel(f6, text="From")
  f6.lab.2.3 <- ttklabel(f6, text="To")
  f6.lab.1.5 <- ttklabel(f6, foreground="#414042", text="e.g. 2010-06-27")
  f6.lab.2.5 <- ttklabel(f6, foreground="#414042", text="e.g. 2011-03-13 17:00")

  f6.box.1.2 <- ttkcombobox(f6, state="readonly", width=15, textvariable=date.time.var)

  f6.ent.1.4 <- ttkentry(f6, width=15, textvariable=tmin.var)
  f6.ent.2.4 <- ttkentry(f6, width=15, textvariable=tmax.var)

  tkgrid(f6.lab.1.1, f6.box.1.2, f6.lab.1.3, f6.ent.1.4, f6.lab.1.5, pady=c(0, 2))
  tkgrid("x", "x", f6.lab.2.3, f6.ent.2.4, f6.lab.2.5)

  tkgrid(f6.box.1.2, f6.ent.1.4, f6.ent.2.4, sticky="we")
  tkgrid.configure(f6.lab.1.1, f6.lab.1.3, f6.lab.2.3, sticky="e")
  tkgrid.configure(f6.lab.1.5, f6.lab.2.5, sticky="w")

  tkgrid.configure(f6.lab.1.3, f6.lab.2.3, padx=c(25, 0))
  tkgrid.configure(f6.lab.1.5, f6.lab.2.5, padx=c(5, 0))

  tkgrid.columnconfigure(f6, 1, weight=1, minsize=15)
  tkgrid.columnconfigure(f6, 3, weight=1, minsize=15)
  tkpack(f6, fill="x", ipadx=2, ipady=2, padx=8, pady=8)

  # bind events
  tkbind(tt, "<Control-o>", function() OpenQueryFile())
  tkbind(tt, "<Control-s>", function() SaveQueryFile(save.file))
  tkbind(tt, "<Shift-Control-S>", function() SaveQueryFile())

  tkbind(f1.box.1.2, "<<ComboboxSelected>>", OpenConnection)
  tkbind(f4.box.4.3, "<<ComboboxSelected>>", UpdateDataVariables)

  tkbind(f4.lst.2.6, "<Control-[>", function() Arrange("up"))
  tkbind(f4.lst.2.6, "<Control-]>", function() Arrange("down"))
  tkbind(f4.lst.2.6, "<Shift-Control-{>", function() Arrange("top"))
  tkbind(f4.lst.2.6, "<Shift-Control-}>", function() Arrange("bottom"))

  tkbind(f4.lst.2.1, "<Control-a>", function() tkselection.set(f4.lst.2.1, 0, "end"))
  tkbind(f4.lst.2.3, "<Control-a>", function() tkselection.set(f4.lst.2.3, 0, "end"))
  tkbind(f4.lst.2.6, "<Control-a>", function() tkselection.set(f4.lst.2.6, 0, "end"))

  tkbind(f4.lst.2.6, "<BackSpace>", RemoveVariables)
  tkbind(f4.lst.2.6, "<Delete>",    RemoveVariables)

  tkbind(f4.lst.2.1, "<Control-r>", AddVariables)
  tkbind(f4.lst.2.3, "<Control-r>", AddVariables)

  # gui control
  SetState()
  tkbind(tt, "<Destroy>", CloseGUI)
  tkfocus(tt)
  tclServiceMode(TRUE)
  invisible()
}
