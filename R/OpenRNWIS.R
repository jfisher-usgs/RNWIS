OpenRNWIS <- function() {
  # A GUI for configuring the database connection

  # Additional functions (subroutines)

  # Save query object

  SaveQueryObject <- function() {
    d <- list()

    d[["dsn.sel"]] <- as.integer(tcl(frame1.box.1.2, "current"))
    d[["opt"]] <- tclvalue(opt.var)
    d[["site.no"]] <- tclvalue(site.no.var)
    d[["site.file"]] <- tclvalue(site.file.var)
    d[["site.type.sel"]] <- as.integer(tkcurselection(frame3.lst.2.6))
    d[["poly.file"]] <- tclvalue(poly.file.var)
    d[["lng.min"]] <- tclvalue(lng.min.var)
    d[["lng.max"]] <- tclvalue(lng.max.var)
    d[["lat.min"]] <- tclvalue(lat.min.var)
    d[["lat.max"]] <- tclvalue(lat.max.var)
    d[["alt.min"]] <- tclvalue(alt.min.var)
    d[["alt.max"]] <- tclvalue(alt.max.var)
    d[["data.type.sel"]] <- as.integer(tcl(frame4.box.4.3, "current"))
    d[["date.time"]] <- tclvalue(date.time.var)
    d[["tmin"]] <- tclvalue(tmin.var)
    d[["tmax"]] <- tclvalue(tmax.var)

    d[["retr.vars"]] <- retr.vars
    d[["initialdir"]] <- initialdir
    d[["save.file"]] <- save.file

    d
  }

  # Open query object

  OpenQueryObject <- function(d) {
    tclServiceMode(FALSE)

    tclvalue(opt.var) <- d$opt
    tclvalue(site.no.var) <- d$site.no
    tclvalue(site.file.var) <- d$site.file
    tclvalue(poly.file.var) <- d$poly.file
    tclvalue(lng.min.var) <- d$lng.min
    tclvalue(lng.max.var) <- d$lng.max
    tclvalue(lat.min.var) <- d$lat.min
    tclvalue(lat.max.var) <- d$lat.max
    tclvalue(alt.min.var) <- d$alt.min
    tclvalue(alt.max.var) <- d$alt.max
    tclvalue(tmin.var) <- d$tmin
    tclvalue(tmax.var) <- d$tmax

    SetState()

    tkselection.clear(frame3.lst.2.6, 0, "end")
    for (i in seq(along=d[["site.type.sel"]]))
      tkselection.set(frame3.lst.2.6, d[["site.type.sel"]][i])

    tcl(frame1.box.1.2, "current", d$dsn.sel)
    OpenConnection()

    tcl(frame4.box.4.3, "current", d$data.type.sel)
    UpdateDataVariables()

    tcl("lset", retr.var, "")
    if (!is.null(d$retr.vars)) {
      is.retr <- d$retr.vars %in% c(site.vars, data.vars)
      retr.vars <<- d$retr.vars[is.retr]
      for (i in seq(along=retr.vars))
        tcl("lappend", retr.var, retr.vars[i])
    }

    cur.vals <- as.character(tkcget(frame6.box.1.2, "-values"))
    if (d$date.time %in% cur.vals)
      tcl(frame6.box.1.2, "set", d$date.time)

    tclServiceMode(TRUE)
  }

  # Save query object to file

  SaveQueryFile <- function(f) {
    if (missing(f) || is.null(f)) {
      f <- SaveFile("Query")
      if (is.null(f))
        return()
      save.file <<- f
    }
    d <- SaveQueryObject()
    save(d, file=f)
  }

  # Open query object file

  OpenQueryFile <- function() {
    f <- OpenFile("Query")
    if (is.null(f))
      return()
    load(file=f)
    OpenQueryObject(d)
  }

  # Close GUI

  CloseGUI <- function() {
    tclServiceMode(FALSE)
    if (as.integer(tclvalue(tt.done.var)) != 0)
      return()
    if (!is.null(con))
      close(con)
    tclvalue(tt.done.var) <- 1
    tkdestroy(tt)
    tclServiceMode(TRUE)
  }

  # Open database connection and query tables/variables

  OpenConnection <- function() {
    idx <- as.integer(tcl(frame1.box.1.2, "current"))
    if (idx < 0)
      return()

    tkconfigure(tt, cursor="watch")

    odbcCloseAll()
    dsn <- as.character(tclvalue(dsn.var))
    con <<- odbcConnect(dsn, uid="", pwd="")
    tkfocus(force=tt)
    tclServiceMode(FALSE)

    site.vars <<- NULL
    data.vars <<- NULL
    retr.vars <<- NULL

    # Clear GUI
    tcl("lset", site.var, "")
    tcl("lset", data.var, "")
    tcl("lset", retr.var, "")
    tclvalue(data.type.var) <- ""
    tclvalue(date.time.var) <- ""
    tkconfigure(frame4.box.4.3, value="")
    tkconfigure(frame6.box.1.2, value="")
    tkconfigure(frame1.but.1.3, state="disabled")

    # Update GUI
    if (inherits(con, "RODBC") && con > 0) {
      tables <- sqlTables(con, errors=FALSE, as.is=TRUE)[, "TABLE_NAME"]
      if (site.table %in% tables) {

        # Establish table types
        types <- names(data.tables)[sapply(data.tables, function(i) i)
                                    %in% tables]
        tkconfigure(frame4.box.4.3, values=types)
        tcl(frame4.box.4.3, "current", 0)

        # Update site variables
        tkconfigure(frame1.but.1.3, state="normal")

        sel <- sqlColumns(con, sqtable=site.table)
        sel <- sel[sel[, "TABLE_NAME"] == site.table, "COLUMN_NAME"]
        site.vars <<- unique(sel)
        for (i in seq(along=site.vars))
          tcl("lappend", site.var, site.vars[i])

        # Update data variables
        UpdateDataVariables()
      } else {
        close(con)
        con <<- NULL
      }
    } else {
      con <<- NULL
    }

    tkconfigure(tt, cursor="arrow")
    tclServiceMode(TRUE)
  }

  # Convert image bits to the image data string format

  BitsToString <- function(bits) {
    n <- length(bits) / 2
    paste("#define v_width ", n, "\n#define v_height ", n, "\n",
          "static unsigned char v_bits[] = { ", paste(bits, collapse=", "),
          " }; ", sep="")
  }

  # Set state for site options

  SetState <- function() {
    opt <- as.integer(tclvalue(opt.var))

    s <- if (opt == 1L) "normal" else "disabled"
    tkconfigure(frame2.ent.2.1, state=s)

    s <- if (opt == 2L) "normal" else "disabled"
    tkconfigure(frame2.ent.4.1, state=s)
    tkconfigure(frame2.but.4.2, state=s)

    s <- if (opt == 3L) "normal" else "disabled"
    tkconfigure(frame3.ent.2.2, state=s)
    tkconfigure(frame3.ent.2.3, state=s)
    tkconfigure(frame3.ent.2.4, state=s)
    tkconfigure(frame3.ent.3.2, state=s)
    tkconfigure(frame3.ent.3.3, state=s)
    tkconfigure(frame3.ent.3.4, state=s)
    tkconfigure(frame3.lst.2.6, state=s)
    tkconfigure(frame3.ent.5.2, state=s)
    tkconfigure(frame3.but.5.5, state=s)
  }

  # Add variables to retrieval list

  AddVariables <- function() {
    site.idxs <- as.integer(tkcurselection(frame4.lst.2.1))
    data.idxs <- as.integer(tkcurselection(frame4.lst.2.3))

    ids <- NULL
    if (length(site.idxs) > 0)
      for (i in site.idxs)
        ids <- c(ids, as.character(tkget(frame4.lst.2.1, i, i)))
    if (length(data.idxs) > 0)
      for (i in data.idxs)
        ids <- c(ids, as.character(tkget(frame4.lst.2.3, i, i)))

    if (is.null(ids))
      return()

    tkselection.clear(frame4.lst.2.1, 0, "end")
    tkselection.clear(frame4.lst.2.3, 0, "end")

    for (i in ids) {
      if (!i %in% retr.vars) {
        tcl("lappend", retr.var, i)
        retr.vars <<- c(retr.vars, i)
      }
    }
  }

  # Remove variables from retrieval list

  RemoveVariables <- function() {
    idxs <- as.integer(tkcurselection(frame4.lst.2.6))
    if (length(idxs) == 0)
      return()
    tkselection.clear(frame4.lst.2.6, 0, "end")
    retr.vars <<- retr.vars[-(idxs + 1)]
    tcl("lset", retr.var, "")
    for (i in retr.vars)
      tcl("lappend", retr.var, i)
  }

  # Update data variables

  UpdateDataVariables <- function() {
    if (is.null(con))
      return()

    tclServiceMode(FALSE)

    data.table <- data.tables[[as.character(tclvalue(data.type.var))]]

    sel <- sqlColumns(con, sqtable=data.table)
    sel <- sel[sel[, "TABLE_NAME"] == data.table, ]

    data.vars <<- sel[, "COLUMN_NAME"]
    data.types <- sel[, "TYPE_NAME"]

    if (!is.null(retr.vars)) {
      is.var <- retr.vars %in% c(site.vars, data.vars)
      retr.vars <<- retr.vars[is.var]
      tcl("lset", retr.var, "")
      for (i in seq(along=retr.vars))
        tcl("lappend", retr.var, retr.vars[i])
    }

    tcl("lset", data.var, "")
    for (i in seq(along=data.vars))
      tcl("lappend", data.var, data.vars[i])

    dt.vars <- data.vars[data.types == "DATE"]
    if (length(dt.vars) > 0) {
      tkconfigure(frame6.box.1.2, values=dt.vars)
      tcl(frame6.box.1.2, "current", 0)
    }

    tclServiceMode(TRUE)
  }

  # Arrange variables in listbox

  Arrange <- function(type) {
    sel.idxs <- as.integer(tkcurselection(frame4.lst.2.6)) + 1
    if (length(sel.idxs) == 0)
      return()
    n <- length(retr.vars)
    idxs <- 1:n

    if (type == "top") {
      idxs <- c(sel.idxs, idxs[-sel.idxs])
    } else if (type == "bottom") {
      idxs <- c(idxs[-sel.idxs], sel.idxs)
    } else if (type == "up") {
      for (i in sel.idxs) {
        if (i == 1L || idxs[i - 1L] %in% sel.idxs)
          next
        idxs[c(i - 1L, i)] <- c(i, idxs[i - 1L])
      }
    } else if (type == "down") {
      for (i in rev(sel.idxs)) {
        if (i == n || idxs[i + 1L] %in% sel.idxs)
          return()
        idxs[c(i, i + 1L)] <- c(idxs[i + 1L], i)
      }
    }

    retr.vars <<- retr.vars[idxs]
    for (i in 1:n)
      tclvalue(retr.var) <- tcl("lreplace", tclvalue(retr.var),
                                i - 1, i - 1, retr.vars[i])
    tkselection.clear(frame4.lst.2.6, 0, "end")
    for (i in which(idxs %in% sel.idxs))
      tkselection.set(frame4.lst.2.6, i - 1)
  }

  # Open file

  OpenFile <- function(type, obj) {
    if (type == "Sites") {
      caption <- "Open Site Number File"
      defaultextension <- NULL
      filters <- NULL
    } else if (type == "Polygon") {
      caption <- "Open Polygon File"
      defaultextension <- "ply"
      filters <- matrix(c("Polygon Text Files", ".ply", "All files", "*"),
                        2, 2, byrow=TRUE)
    } else if (type == "Query") {
      caption <- "Open RNWIS Project File"
      defaultextension <- "rda"
      filters <- matrix(c("RNWIS Project Files", ".rda", "All files", "*"),
                        2, 2, byrow=TRUE)
    }
    f <- CallFileDialogBox("tk_getOpenFile", caption, defaultextension, filters)
    if (missing(obj)) {
      return(f)
    } else {
      tclvalue(obj) <- f
    }
  }

   # Save file as

  SaveFile <- function(type) {
    if (type == "Data") {
      defaultextension <- "txt"
      filters <- matrix(c("Text Files", ".txt", "ESRI Shapefiles", ".shp",
                          "All files", "*"), 3, 2, byrow=TRUE)
    } else if (type == "Query") {
      defaultextension <- "rda"
      filters <- matrix(c("RNWIS Project Files", ".rda", "All files", "*"),
                        2, 2, byrow=TRUE)
    }
    CallFileDialogBox("tk_getSaveFile", caption="Save As",
                      defaultextension, filters)
  }

  # Call file dialog box

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
    f <- tclvalue(do.call(tcl, args))
    if (nzchar(f)) {
      initialdir <<- dirname(f)
      return(f)
    }
  }

  # Map sites

  CallMapSites <- function() {
    if (is.null(con)) {
      ShowErrorMessage('01')
      return()
    }
    sqvars <- c(vars[['lat']], vars[['lng']], vars[['alt']], vars[['site']],
                vars[['name']], vars[['agency']], vars[['type']])
    data <- GetSiteInfo(sqvars)
    names(data$sites) <- names(vars)
    MapSites(data$sites, data$polygons)
  }

  # Retrieve data

  RetrieveData <- function() {
    if (is.null(con)) {
      ShowErrorMessage('01')
      return()
    }
    if (is.null(retr.vars) || length(retr.vars) == 0L) {
      ShowErrorMessage('02')
      return()
    }
    tkconfigure(tt, cursor="watch")

    # Determine site variables in retrieval list
    is.site <- retr.vars %in% site.vars

    # Construct site table and identify site numbers
    sqvars <-  unique(c(vars[['site']], retr.vars[is.site]))
    d.site <- GetSiteInfo(sqvars)
    if (is.null(d.site)) {
      ShowErrorMessage('04')
      return()
    }
    d.site <- d.site$sites
    site.no <- d.site[, vars[['site']]]

   # Date and time range
    d.t.lim <- c(NA, NA)
    d.t.var <- as.character(tclvalue(date.time.var))
    if (d.t.var != "")
      d.t.lim <- c(ConvertEntryToDate(tmin.var), ConvertEntryToDate(tmax.var))
    else
      d.t.var <- NULL

    # Construct data table
    d.data <- NULL
    sqvars <- retr.vars[!is.site]
    data.table <- data.tables[[as.character(tclvalue(data.type.var))]]
    if (length(sqvars) > 0) {
      d.data <- QueryDatabase(con=con,
                              sqtable=data.table,
                              sqvars=unique(c(vars[['site']], sqvars)),
                              site.no.var=vars[['site']],
                              site.no=site.no,
                              d.t.var=d.t.var,
                              d.t.lim=d.t.lim
                             )
      if (inherits(d.data, "character")) {
        ShowErrorMessage('06', d.data[1])
        stop()
      }
      if (nrow(d.data) == 0L)
        d.data <- NULL
    }

    # Merge site and data tables
    if (is.null(d.data))
      d <- d.site
    else
      d <- merge(d.site, d.data, by=vars[['site']])

    # Save data to file
    f <- SaveFile("Data")
    if (!is.null(f)) {
      if (tolower(substr(f, nchar(f) - 2, nchar(f))) == "shp") {
        col.names <- unique(c(vars[['lng']], vars[['lat']], retr.vars))
        d <- d[, col.names]

        # Columns of class POSIXct are not permitted, convert to
        # character strings
        col.class <- sapply(names(d), function(i) class(d[, i])[1])
        dt.idxs <- which(col.class == "POSIXct")
        if (length(dt.idxs) > 0)
          d[, dt.idxs] <- as.character(format(d[, dt.idxs]))

        # Column names are finicky for shapefiles, rules are convoluted,
        # 8-bit names and no periods
        col.names <- gsub("\\.", "", make.names(substr(col.names, 1, 7),
                          unique=TRUE))

        colnames(d) <- col.names
        coordinates(d) <- col.names[1:2]
        s <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        proj4string(d) <- CRS(s)

        writeOGR(obj=d, dsn=dirname(f), driver="ESRI Shapefile",
                 layer=sub(paste(".shp$", sep=""), "", basename(f)),
                 verbose=TRUE)
      } else {
        d <- d[, retr.vars]
        write.table(d, file=f, sep="\t", quote=FALSE, row.names=FALSE)
      }
    }
    tkconfigure(tt, cursor="arrow")
  }

  # Convert Tk entry to date-time value

  ConvertEntryToDate <- function(tk.ent) {
    string <- as.character(tclvalue(tk.ent))
    if (string == "")
      return(NA)
    fmt <- "%Y-%m-%d"
    if (length(strsplit(string, " ")[[1]]) > 1)
      fmt <- paste(fmt, "%H:%M")
    date <- as.POSIXct(string, format=fmt)
    if (is.na(date))
      ShowErrorMessage('05', string)
    date
  }

  # Process site strings, returns vector of site numbers

  CleanSiteStrings <- function(s) {
    str.split <- unlist(strsplit(s, '[[:punct:][:space:]]'))
    int.split <- suppressWarnings(as.numeric(str.split))
    as.character(int.split[!is.na(int.split)])
  }

  # Retrieve site data and polygon domain

  GetSiteInfo <- function(sqvars) {
    opt <- as.integer(tclvalue(opt.var))
    poly.obj <- NULL

    sqvars <- unique(c(vars[['lat']], vars[['lng']], sqvars))

    # Site numbers
    if (opt == 1L | opt == 2L) {

      # Get site number(s) in entry box
      if (opt == 1L) {
        site.no <- as.character(tclvalue(site.no.var))
        if (site.no == "") {
          ShowErrorMessage('03')
          stop()
        }
        site.no <- CleanSiteStrings(site.no)

      # Read site numbers in file
      } else {
        site.file <- as.character(tclvalue(site.file.var))
        if (site.file == "" || file.access(site.file, mode=0) < 0) {
          ShowErrorMessage('03')
          return()
        }
        scanned.strings <- scan(file=site.file, what="character",
                                comment.char="#", quiet=TRUE)
        site.no <- CleanSiteStrings(paste(scanned.strings, collapse=","))
      }

      if (length(site.no) == 0L) {
        ShowErrorMessage('03')
        return()
      }

      # Query database
      sel <- QueryDatabase(con=con, sqtable=site.table, sqvars=sqvars,
                           site.no.var=vars[['site']],
                           site.no=site.no)

    # Site attributes
    } else if (opt == 3L) {

      # Spatial limits
      lat.min <- suppressWarnings(as.numeric(tclvalue(lat.min.var)))
      lat.max <- suppressWarnings(as.numeric(tclvalue(lat.max.var)))
      lng.min <- suppressWarnings(as.numeric(tclvalue(lng.min.var)))
      lng.max <- suppressWarnings(as.numeric(tclvalue(lng.max.var)))
      alt.min <- suppressWarnings(as.numeric(tclvalue(alt.min.var)))
      alt.max <- suppressWarnings(as.numeric(tclvalue(alt.max.var)))

      # Polygon box domain
      poly.obj <- NULL
      poly.file <- as.character(tclvalue(poly.file.var))
      if (poly.file != "" && file.access(poly.file, mode=0) == 0) {
        poly.obj <- read.polyfile(poly.file, nohole=FALSE)
        if (inherits(poly.obj, "gpc.poly")) {
          poly.bbox <- get.bbox(poly.obj)
          lat.min <- min(lat.min, poly.bbox$y[1], na.rm=TRUE)
          lat.max <- max(lat.max, poly.bbox$y[2], na.rm=TRUE)
          lng.min <- min(lng.min, poly.bbox$x[1], na.rm=TRUE)
          lng.max <- max(lng.max, poly.bbox$x[2], na.rm=TRUE)
        } else {
          poly.obj <- NULL
        }
      }

      # Site types
      site.type.codes <- NULL
      idxs <- as.integer(tkcurselection(frame3.lst.2.6))
      if (length(idxs) > 0) {
        for (i in idxs) {
          site.type <- as.character(tkget(frame3.lst.2.6, i, i))
          if (site.type %in% names(site.types)) {
            site.type.codes <- c(site.type.codes, site.types[[site.type]])
          } else {
            site.type.codes <- NULL
            break
          }
        }
      }

      # Query database
      sel <- QueryDatabase(con=con,
                           sqtable=site.table,
                           sqvars=sqvars,
                           site.tp.cd.var=vars[['type']],
                           site.tp.cd=site.type.codes,
                           lng.var=vars[['lng']],
                           lng.lim=c(lng.min, lng.max),
                           lat.var=vars[['lat']],
                           lat.lim=c(lat.min, lat.max),
                           alt.var=vars[['alt']],
                           alt.lim=c(alt.min, alt.max))
      if (inherits(sel, "character")) {
        ShowErrorMessage('06', sel[1])
        stop()
      }

      # Sites in polygon domain
      if (!is.null(poly.obj)) {
        poly.pts <- get.pts(poly.obj)
        for (i in seq(along=poly.pts)) {
          x <- sel[, vars[['lng']]]
          y <- sel[, vars[['lat']]]
          poly.x <- poly.pts[[i]]$x
          poly.y <- poly.pts[[i]]$y
          is.inside <- point.in.polygon(point.x=x, point.y=y,
                                        pol.x=poly.x, pol.y=poly.y)
          if (poly.pts[[i]]$hole)
            sel <- sel[is.inside != 1, ]
          else
            sel <- sel[is.inside != 0, ]
        }
      }
    }

    # Return selection
    if (inherits(sel, "data.frame") && nrow(sel) > 0) {
      return(list(sites=sel[, sqvars], polygons=poly.obj))
    } else {
      ShowErrorMessage('04')
      stop(sel)
    }
  }

  # Show error message dialog box

  ShowErrorMessage <- function(code, detail=NULL) {
    err <- err[[code]]
    arg <- list("tk_messageBox", parent=tt, default="ok",
                icon=err[1], type=err[2], title=err[3], message=err[4])
    if (!is.null(detail))
      arg <- c(arg, detail=detail)
    ans <- tclvalue(do.call(tcl, arg))
    tkconfigure(tt, cursor="arrow")
    tclServiceMode(TRUE)
    as.character(ans)
  }


  # Main program

  # Variables specific to NWIS

  site.table <- "sitefile_01"

  data.tables <- list('Groundwater levels' = "gw_lev_01",
                      'Hole construction' = "gw_hole_01",
                      'Casing construction' = "gw_csng_01",
                      'Openings construction' = "gw_open_01")

  site.types <- list('Well' = c("GW", "GW-CR", "GW-EX", "GW-HZ", "GW-IW",
                                "GW-MW", "GW-TH"),
                     'Other subsurface' = c("SB", "SB-CV", "SB-GWD", "SB-TSM",
                                            "SB-UZ"),
                     'Stream' = c("ST", "ST-CA", "ST-DCH", "ST-TS"),
                     'Lake' = "LK",
                     'Spring' = "SP")

  # NWIS is using the WGS84 datum for the "dec_lat_va" and "dec_long_va"
  # optional variables; Google Maps also uses this dataum. NWIS required
  # variables "lat_va" and "long_va" (not called in this program) are
  # either using the NAD27 or NAD83 datum ("coord_datum_cd").

  vars <- list('lat' = "dec_lat_va",
               'lng' = "dec_long_va",
               'alt' = "alt_va",
               'site' = "site_no",
               'name' = "station_nm",
               'agency' = "agency_cd",
               'type' = "site_tp_cd")

  # Load required R packages

  for (i in c("tcltk", "sp", "RODBC", "gpclib", "rgdal"))
    suppressPackageStartupMessages(require(i, character.only=TRUE))

  # Error and warning messages: [icon, type, title, message]

  err <- list()
  err[['01']] <- c("warning", "ok",
                   "Missing Database Connection",
                   "Select a registerd ODBC data source")
  err[['02']] <- c("warning", "ok",
                   "Missing Retrieval Variables",
                   "Add variables to retrieval list.")
  err[['03']] <- c("error", "ok",
                   "Missing Site Numbers",
                   "Site numbers missing or improperly formatted.")
  err[['04']] <- c("error", "ok",
                   "Empty Database Query",
                   "Query of site table returned no data.")
  err[['05']] <- c("warning", "ok",
                   "Date and Time Conversion",
                   "Character string could not be converted to date object.")
  err[['06']] <- c("error", "ok",
                   "Query Error",
                   "Query resulted in error.")

  # Initialize top-level variables

  con <- NULL
  site.vars <- NULL
  data.vars <- NULL
  retr.vars <- NULL
  initialdir <- NULL
  save.file <- NULL

  # Assign variables linked to Tk widgets

  dsn.var <- tclVar()

  opt.var <- tclVar(1)
  site.no.var <- tclVar()
  site.file.var <- tclVar()
  poly.file.var <- tclVar()
  site.type.var <- tclVar()
  for (i in c("All ...", names(site.types)))
    tcl("lappend", site.type.var, i)
  lng.min.var <- tclVar()
  lng.max.var <- tclVar()
  lat.min.var <- tclVar()
  lat.max.var <- tclVar()
  alt.min.var <- tclVar()
  alt.max.var <- tclVar()

  data.type.var <- tclVar()
  site.var <- tclVar()
  data.var <- tclVar()
  retr.var <- tclVar()

  date.time.var <- tclVar()
  tmin.var <- tclVar()
  tmax.var <- tclVar()

  tt.done.var <- tclVar(0)

  # Create image bitmaps for buttons,
  # based on arrows.tcl by Keith Vetter, http://wiki.tcl.tk/8554

  bits <- c('0x00', '0x00', '0x06', '0x03', '0x8e', '0x03', '0xdc', '0x01',
            '0xf8', '0x00', '0x70', '0x00', '0xf8', '0x00', '0xdc', '0x01',
            '0x8e', '0x03', '0x06', '0x03', '0x00', '0x00')
  cross.del <- tkimage.create("bitmap", data=as.tclObj(BitsToString(bits)))

  bits <- c('0x00', '0x00', '0x20', '0x00', '0x60', '0x00', '0xe0', '0x00',
            '0xfc', '0x01', '0xfc', '0x03', '0xfc', '0x01', '0xe0', '0x00',
            '0x60', '0x00', '0x20', '0x00', '0x00', '0x00')
  arrow.right <- tkimage.create("bitmap", data=as.tclObj(BitsToString(bits)))

  bits <- c('0x00', '0x00', '0x20', '0x00', '0x70', '0x00', '0xf8', '0x00',
            '0xfc', '0x01', '0xfe', '0x03', '0x70', '0x00', '0x70', '0x00',
            '0x70', '0x00', '0x00', '0x00', '0x00', '0x00')
  arrow.up <- tkimage.create("bitmap", data=as.tclObj(BitsToString(bits)))

  bits <- c('0x00', '0x00', '0x00', '0x00', '0x70', '0x00', '0x70', '0x00',
            '0x70', '0x00', '0xfe', '0x03', '0xfc', '0x01', '0xf8', '0x00',
            '0x70', '0x00', '0x20', '0x00', '0x00', '0x00')
  arrow.down <- tkimage.create("bitmap", data=as.tclObj(BitsToString(bits)))

  bits <- c('0x00', '0x00', '0xfe', '0x03', '0xfe', '0x03', '0x20', '0x00',
            '0x70', '0x00', '0xf8', '0x00', '0xfc', '0x01', '0xfe', '0x03',
            '0x70', '0x00', '0x70', '0x00', '0x70', '0x00')
  arrow.top <- tkimage.create("bitmap", data=as.tclObj(BitsToString(bits)))

  bits <- c('0x70', '0x00', '0x70', '0x00', '0x70', '0x00', '0xfe', '0x03',
            '0xfc', '0x01', '0xf8', '0x00', '0x70', '0x00', '0x20', '0x00',
            '0xfe', '0x03', '0xfe', '0x03', '0x00', '0x00')
  arrow.bottom <- tkimage.create("bitmap", data=as.tclObj(BitsToString(bits)))

  # Open GUI

  tclServiceMode(FALSE)
  tt <- tktoplevel(padx=0, pady=0)
  tktitle(tt) <- "National Water Information System: R Interface"

  # Create menus

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

  if (!"RNWIS" %in% .packages()) {
    if ("RSurvey" %in% .packages(all.available=TRUE)) {
      suppressPackageStartupMessages(require("RSurvey"))
      if (!is.null(Data("win.loc")))
        tkwm.geometry(tt, Data("win.loc"))
      tkadd(menu.file, "separator")
      tkadd(menu.file, "command", label="Restore R session",
            command=function() {
              geo <- unlist(strsplit(as.character(tkwm.geometry(tt)), "\\+"))
              win.loc <- paste("+", as.integer(geo[2]),
                               "+", as.integer(geo[3]), sep="")
              Data("win.loc", win.loc)
              CloseGUI()
              RestoreSession(paste(getwd(), "R", sep="/"), fun.call="OpenRNWIS")
            })
    }
  }

  tkconfigure(tt, menu=top.menu)

  # Frame 0, map and export buttons

  frame0 <- ttkframe(tt, relief="flat")

  frame0.but.1 <- ttkbutton(frame0, width=15, text="Map Sites",
                            command=CallMapSites)
  frame0.but.2 <- ttkbutton(frame0, width=15, text="Retrieve Data",
                            command=RetrieveData)

  frame0.grp.3 <- ttksizegrip(frame0)

  tkgrid(frame0.but.1, frame0.but.2, frame0.grp.3)

  tkgrid.configure(frame0.but.1, frame0.but.2, sticky="e",
                   padx=2, pady=c(12, 10))
  tkgrid.configure(frame0.grp.3, sticky="se")

  tkpack(frame0, side="bottom", anchor="e")

  # Frame 1, ODBC source name selection

  frame1 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3,
                          text="Select a registerd ODBC data source")

  frame1.lab.1.1 <- ttklabel(frame1, text="Source name")
  frame1.box.1.2 <- ttkcombobox(frame1, textvariable=dsn.var, state="readonly")
  frame1.but.1.3 <- ttkbutton(frame1, width=8, text="Explore",
                              command=function() ExploreDatabase(con, tt))

  tkgrid(frame1.lab.1.1, frame1.box.1.2, frame1.but.1.3,
         padx=c(0, 2), pady=3, sticky="we")
  tkgrid.configure(frame1.lab.1.1, sticky="e")

  tkgrid.columnconfigure(frame1, 1, weight=1, minsize=25)

  tkpack(frame1, fill="x", expand=FALSE, padx=15, pady=15)

  tkconfigure(frame1.box.1.2, value=names(odbcDataSources()))

  tkconfigure(frame1.but.1.3, state="disabled")

  # Frame 2 and 3, select sites

  txt <- "Select sites based on one of the following options"
  frame2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3, text=txt)

  frame2.rad.1.1 <- ttkradiobutton(frame2, variable=opt.var, value=1,
                                   command=SetState, text='Site number(s):')
  frame2.rad.3.1 <- ttkradiobutton(frame2, variable=opt.var, value=2,
                                   command=SetState,
                                   text='File of site numbers:')
  frame2.rad.5.1 <- ttkradiobutton(frame2, variable=opt.var, value=3,
                                   command=SetState, text='Site attributes:')

  frame2.ent.2.1 <- ttkentry(frame2, width=25, textvariable=site.no.var)
  frame2.ent.4.1 <- ttkentry(frame2, width=25, textvariable=site.file.var)

  frame2.but.4.2 <- ttkbutton(frame2, width=8, text="Browse",
                              command=function() OpenFile("Sites",
                                                               site.file.var))

  frame3 <- ttkframe(frame2, relief="flat")

  frame3.lab.1.2 <- ttklabel(frame3, justify="center",
                             text="Latitude\n(WGS84)")
  frame3.lab.1.3 <- ttklabel(frame3, justify="center",
                             text="Longitude\n(WGS84)")
  frame3.lab.1.4 <- ttklabel(frame3, justify="center",
                             text="Altitude\n(WGS84 EGM96)")
  frame3.lab.1.6 <- ttklabel(frame3, justify="center",
                             text="Select type(s)")
  frame3.lab.2.1 <- ttklabel(frame3, text="Minimum")
  frame3.lab.3.1 <- ttklabel(frame3, text="Maximum")

  frame3.ent.2.3 <- ttkentry(frame3, width=15, textvariable=lng.min.var)
  frame3.ent.3.3 <- ttkentry(frame3, width=15, textvariable=lng.max.var)

  frame3.ent.2.2 <- ttkentry(frame3, width=15, textvariable=lat.min.var)
  frame3.ent.3.2 <- ttkentry(frame3, width=15, textvariable=lat.max.var)

  frame3.ent.2.4 <- ttkentry(frame3, width=15, textvariable=alt.min.var)
  frame3.ent.3.4 <- ttkentry(frame3, width=15, textvariable=alt.max.var)

  frame3.lst.2.6 <- tklistbox(frame3, selectmode="extended", activestyle="none",
                              relief="flat", borderwidth=5, width=15, height=6,
                              exportselection=FALSE, listvariable=site.type.var,
                              highlightthickness=0)
  tkselection.set(frame3.lst.2.6, 0)

  frame3.lab.4.1 <- ttklabel(frame3, foreground="#414042", text="e.g.")
  frame3.lab.4.2 <- ttklabel(frame3, foreground="#414042", text="43.510023")
  frame3.lab.4.3 <- ttklabel(frame3, foreground="#414042", text="-112.980728")
  frame3.lab.4.4 <- ttklabel(frame3, foreground="#414042", text="4382.3")

  frame3.lab.5.1 <- ttklabel(frame3, text="Polygon file")
  frame3.ent.5.2 <- ttkentry(frame3, width=25, textvariable=poly.file.var)
  frame3.but.5.5 <- ttkbutton(frame3, width=8, text="Browse",
                              command=function() OpenFile("Polygon",
                                                          poly.file.var))

  tkgrid(frame2.rad.1.1)
  tkgrid(frame2.ent.2.1, columnspan=2, pady=c(0, 4))
  tkgrid(frame2.rad.3.1)
  tkgrid(frame2.ent.4.1, frame2.but.4.2, pady=c(0, 4))
  tkgrid(frame2.rad.5.1)

  tkgrid.configure(frame2.rad.1.1, frame2.rad.3.1, frame2.rad.5.1,
                   sticky="w", columnspan=2)

  tkgrid.configure(frame2.ent.2.1, frame2.ent.4.1, sticky="we", padx=c(10, 2))

  tkgrid(frame3, columnspan=2, sticky="we")

  tkgrid("x", frame3.lab.1.2, frame3.lab.1.3, frame3.lab.1.4, "x",
         frame3.lab.1.6, pady=c(0, 1))
  tkgrid.configure(frame3.lab.1.6, padx=c(10, 0), sticky="s")

  tkgrid(frame3.lab.2.1, frame3.ent.2.2, frame3.ent.2.3, frame3.ent.2.4,
         "x", frame3.lst.2.6, padx=1, pady=c(0, 1), sticky="we")

  tkgrid(frame3.lab.3.1, frame3.ent.3.2, frame3.ent.3.3, frame3.ent.3.4,
         padx=1, pady=c(1, 0), sticky="we")

  tkgrid.configure(frame3.lab.2.1, frame3.lab.3.1, sticky="e", padx=c(10, 0))

  tkgrid(frame3.lab.4.1, frame3.lab.4.2, frame3.lab.4.3, frame3.lab.4.4)
  tkgrid.configure(frame3.lab.4.1, sticky="e")

  tkgrid.configure(frame3.lab.1.4, frame3.ent.2.4, frame3.ent.3.4,
                   frame3.lab.4.4, columnspan=2)

  tkgrid(frame3.lab.5.1, frame3.ent.5.2, "x", "x", frame3.but.5.5, pady=c(7, 0))
  tkgrid.configure(frame3.lab.5.1, padx=c(10, 0), sticky="e")
  tkgrid.configure(frame3.ent.5.2, columnspan=3, sticky="we", padx=c(1, 2))

  tkgrid.configure(frame3.lst.2.6, rowspan=4, padx=c(10, 0),
                   pady=1, sticky="nsew")

  tkgrid.columnconfigure(frame3, 1, weight=1, minsize=15)
  tkgrid.columnconfigure(frame3, 2, weight=1, minsize=15)
  tkgrid.columnconfigure(frame3, 3, weight=1, minsize=15)

  tkgrid.columnconfigure(frame2, 0, weight=1, minsize=15)

  tkpack(frame2, fill="x", ipadx=2, ipady=2, padx=8, pady=8, anchor="n")

  # Frame 4 and 3, select variables

  frame4 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3,
                          text="Add variables to retrieval list")

  frame4.lab.1.1 <- ttklabel(frame4, text="Site variables")
  frame4.lab.1.3 <- ttklabel(frame4, text="Data variables")
  frame4.lab.1.6 <- ttklabel(frame4, text="Retrieve variables")
  frame4.lab.4.1 <- ttklabel(frame4, text="Type")

  frame4.lst.2.1 <- tklistbox(frame4, selectmode="extended", activestyle="none",
                              relief="flat", borderwidth=5, width=15, height=3,
                              exportselection=FALSE, listvariable=site.var,
                              highlightthickness=0)
  frame4.lst.2.3 <- tklistbox(frame4, selectmode="extended", activestyle="none",
                              relief="flat", borderwidth=5, width=15, height=3,
                              exportselection=FALSE, listvariable=data.var,
                              highlightthickness=0)
  frame4.lst.2.6 <- tklistbox(frame4, selectmode="extended", activestyle="none",
                              relief="flat", borderwidth=5, width=15, height=3,
                              exportselection=FALSE, listvariable=retr.var,
                              highlightthickness=0)

  frame4.ysc.2.2 <- ttkscrollbar(frame4, orient="vertical")
  frame4.ysc.2.4 <- ttkscrollbar(frame4, orient="vertical")
  frame4.ysc.2.7 <- ttkscrollbar(frame4, orient="vertical")

  tkconfigure(frame4.lst.2.1, background="white",
              yscrollcommand=paste(.Tk.ID(frame4.ysc.2.2), "set"))
  tkconfigure(frame4.lst.2.3, background="white",
              yscrollcommand=paste(.Tk.ID(frame4.ysc.2.4), "set"))
  tkconfigure(frame4.lst.2.6, background="white",
              yscrollcommand=paste(.Tk.ID(frame4.ysc.2.7), "set"))
  tkconfigure(frame4.ysc.2.2, command=paste(.Tk.ID(frame4.lst.2.1), "yview"))
  tkconfigure(frame4.ysc.2.4, command=paste(.Tk.ID(frame4.lst.2.3), "yview"))
  tkconfigure(frame4.ysc.2.7, command=paste(.Tk.ID(frame4.lst.2.6), "yview"))

  frame4.but.2.5 <- ttkbutton(frame4, width=2, image=arrow.right,
                              command=AddVariables)

  frame4.box.4.3 <- ttkcombobox(frame4, state="readonly",
                                textvariable=data.type.var, width=10)

  frame5 <- ttkframe(frame4, relief="flat")

  frame5.but.1.1 <- ttkbutton(frame5, width=2, image=arrow.top,
                              command=function() Arrange("top"))
  frame5.but.1.2 <- ttkbutton(frame5, width=2, image=arrow.up,
                              command=function() Arrange("up"))
  frame5.but.1.3 <- ttkbutton(frame5, width=2, image=arrow.down,
                              command=function() Arrange("down"))
  frame5.but.1.4 <- ttkbutton(frame5, width=2, image=arrow.bottom,
                              command=function() Arrange("bottom"))
  frame5.but.1.5 <- ttkbutton(frame5, width=2, image=cross.del,
                              command=RemoveVariables)

  tkgrid(frame4.lab.1.1, "x", frame4.lab.1.3, "x", "x", frame4.lab.1.6, "x",
         pady=c(0, 1))
  tkgrid(frame4.lst.2.1, frame4.ysc.2.2, frame4.lst.2.3, frame4.ysc.2.4,
         frame4.but.2.5, frame4.lst.2.6, frame4.ysc.2.7)
  tkgrid(frame4.lab.4.1, "x", frame4.box.4.3, "x", "x", frame5, "x")
  tkgrid.configure(frame5, columnspan=2, sticky="w")
  tkgrid(frame5.but.1.1, frame5.but.1.2, frame5.but.1.3, frame5.but.1.4,
         frame5.but.1.5, padx=c(0, 4), pady=c(4, 0))

  tkgrid.configure(frame4.lst.2.1, frame4.ysc.2.2, frame4.lst.2.3,
                   frame4.ysc.2.4, frame4.lst.2.6, frame4.ysc.2.7)

  tkgrid.configure(frame4.lst.2.1, frame4.lst.2.3, frame4.lst.2.6,
                   sticky="nsew")
  tkgrid.configure(frame4.ysc.2.2, sticky="ns", padx=c(0, 10))
  tkgrid.configure(frame4.ysc.2.4, frame4.ysc.2.7, sticky="ns", padx=0)

  tkgrid.configure(frame4.but.2.5, padx=10, pady=c(0, 0))

  tkgrid.configure(frame4.lab.4.1, sticky="e", padx=c(0, 1), pady=c(4, 0),
                   columnspan=2)
  tkgrid.configure(frame4.box.4.3, sticky="we", pady=c(4, 0), padx=0)

  tkgrid.columnconfigure(frame4, 0, weight=1, minsize=15)
  tkgrid.columnconfigure(frame4, 2, weight=1, minsize=15)
  tkgrid.columnconfigure(frame4, 5, weight=1, minsize=15)

  tkgrid.rowconfigure(frame4, 1, weight=1)

  tkpack(frame4, fill="both", expand=TRUE, ipadx=2, ipady=2,
         padx=8, pady=8)

  # Frame 6, specify date range

  frame6 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3,
                          text="Specify date and time range")

  frame6.lab.1.1 <- ttklabel(frame6, text="Variable")
  frame6.lab.1.3 <- ttklabel(frame6, text="From")
  frame6.lab.2.3 <- ttklabel(frame6, text="To")
  frame6.lab.1.5 <- ttklabel(frame6, foreground="#414042",
                             text="e.g. 2010-06-27")
  frame6.lab.2.5 <- ttklabel(frame6, foreground="#414042",
                             text="e.g. 2011-03-13 17:00")

  frame6.box.1.2 <- ttkcombobox(frame6, state="readonly", width=15,
                                textvariable=date.time.var)

  frame6.ent.1.4 <- ttkentry(frame6, width=15, textvariable=tmin.var)
  frame6.ent.2.4 <- ttkentry(frame6, width=15, textvariable=tmax.var)

  tkgrid(frame6.lab.1.1, frame6.box.1.2, frame6.lab.1.3, frame6.ent.1.4,
         frame6.lab.1.5, pady=c(0, 2))
  tkgrid("x", "x", frame6.lab.2.3, frame6.ent.2.4, frame6.lab.2.5)

  tkgrid(frame6.box.1.2, frame6.ent.1.4, frame6.ent.2.4, sticky="we")
  tkgrid.configure(frame6.lab.1.1, frame6.lab.1.3, frame6.lab.2.3, sticky="e")
  tkgrid.configure(frame6.lab.1.5, frame6.lab.2.5, sticky="w")

  tkgrid.configure(frame6.lab.1.3, frame6.lab.2.3, padx=c(25, 0))
  tkgrid.configure(frame6.lab.1.5, frame6.lab.2.5, padx=c(5, 0))

  tkgrid.columnconfigure(frame6, 1, weight=1, minsize=15)
  tkgrid.columnconfigure(frame6, 3, weight=1, minsize=15)
  tkpack(frame6, fill="x", ipadx=2, ipady=2, padx=8, pady=8)

  # Bind events

  tkbind(frame1.box.1.2, "<<ComboboxSelected>>", OpenConnection)
  tkbind(frame4.box.4.3, "<<ComboboxSelected>>", UpdateDataVariables)

  tkbind(frame4.lst.2.6, "<Control-[>", function() Arrange("up"))
  tkbind(frame4.lst.2.6, "<Control-]>", function() Arrange("down"))
  tkbind(frame4.lst.2.6, "<Shift-Control-{>", function() Arrange("top"))
  tkbind(frame4.lst.2.6, "<Shift-Control-}>", function() Arrange("bottom"))

  tkbind(frame4.lst.2.1, "<Control-a>",
         function() tkselection.set(frame4.lst.2.1, 0, "end"))
  tkbind(frame4.lst.2.3, "<Control-a>",
         function() tkselection.set(frame4.lst.2.3, 0, "end"))
  tkbind(frame4.lst.2.6, "<Control-a>",
         function() tkselection.set(frame4.lst.2.6, 0, "end"))

  tkbind(frame4.lst.2.6, "<BackSpace>", RemoveVariables)
  tkbind(frame4.lst.2.6, "<Delete>", RemoveVariables)

  tkbind(frame4.lst.2.1, "<Control-r>", AddVariables)
  tkbind(frame4.lst.2.3, "<Control-r>", AddVariables)

  # GUI control

  SetState()
  tkbind(tt, "<Destroy>", CloseGUI)
  tkfocus(tt)
  tclServiceMode(TRUE)
  invisible()
}
