OpenRNWIS <- function() {
  # A GUI for configuring the database connection

  # Additional functions (subroutines)

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
    idx  <- as.integer(tcl(frame1.box.1.2, "current"))
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
        site.vars <<- sqlColumns(con, sqtable=site.table)[, "COLUMN_NAME"]
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
    for (i in idxs) {
      id <- as.character(tkget(frame4.lst.2.6, i, i))
      retr.vars <<- retr.vars[!retr.vars %in% id]
      tclvalue(retr.var) <- tcl("lreplace", tclvalue(retr.var), i, i)
    }
  }

  # Update data variables

  UpdateDataVariables <- function() {
    if (is.null(con))
      return()

    data.table <- data.tables[[as.character(tclvalue(data.type.var))]]
    sqcols <- sqlColumns(con, sqtable=data.table)[, c("COLUMN_NAME",
                                                      "TYPE_NAME")]
    data.vars <<- sqcols[, 1]
    data.types <- sqcols[, 2]

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
  }

  # Arrange variables in listbox

  Arrange <- function(type, lst) {
    sel.idxs <- as.integer(tkcurselection(lst)) + 1
    if (length(sel.idxs) == 0)
      return()
    n <- length(retr.vars)
    idxs <- 1:n
    if (type == "up") {
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
    tkselection.clear(lst, 0, "end")
    for (i in which(idxs %in% sel.idxs))
      tkselection.set(lst, i - 1)
  }

  # Browse for file

  BrowseForFile <- function(type, obj) {
    if (type == "Polygon") {
      caption <- "Select polygon file"
      defaultextension <- "ply"
      filters <- matrix(c("Polygon Text Files", ".ply", "All files", "*"),
                        2, 2, byrow=TRUE)
    } else {
      caption <- "Select site number file"
      filters <- NULL
      defaultextension <- NULL
    }

    args <- list("tk_getOpenFile", title=caption, multiple=FALSE, parent=tt)
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
    if (!nzchar(f))
      return()

    initialdir <<- dirname(f)
    tclvalue(obj) <- f
  }

  # Map sites

  CallMapSites <- function() {
    if (is.null(con))
      return()
    sqvars <- c(vars[['lat']], vars[['lng']], vars[['alt']], vars[['site']],
                vars[['name']], vars[['agency']], vars[['type']])
    data <- GetSiteInfo(sqvars)
    names(data$sites) <- names(vars)
    MapSites(data$sites, data$polygons)
}

  # Retrieve data

  RetrieveData <- function() {
    if (is.null(con))
      return()

    print("notyet")
  }










  # Process site strings

  ProcessSiteStrings <- function(s) {
    str.split <- unlist(strsplit(s, '[[:punct:][:space:]]'))
    int.split <- suppressWarnings(as.numeric(str.split))
    as.character(int.split[!is.na(int.split)])
  }

  # Retrieve site data and polygon domain

  GetSiteInfo <- function(sqvars) {
    opt <- as.integer(tclvalue(opt.var))
    poly.obj <- NULL

    # Site numbers
    if (opt == 1L | opt == 2L) {

      # Get site number(s) in entry box
      if (opt == 1L) {
        site.no <- as.character(tclvalue(site.no.var))
        if (site.no == "")
          return()
        site.no <- ProcessSiteStrings(site.no)

      # Read site numbers in file
      } else {
        site.file <- as.character(tclvalue(site.file.var))
        if (file.access(site.file, mode=0) < 0)
          return()
        scanned.strings <- scan(file=site.file, what="character",
                                comment.char="#", quiet=TRUE)
        site.no <- ProcessSiteStrings(paste(scanned.strings, collapse=","))
      }

      if (length(site.no) == 0L)
        return()

      # Query database
      sel <- QueryDatabase(con=con, sqtable=site.table, sqvars=sqvars,
                           site.no.var=vars[['site']],
                           site.no=site.no)

    # Site attributes
    } else if (opt == 3L) {

      # Spatial limits
      lng.min <- suppressWarnings(as.numeric(tclvalue(lng.min.var)))
      lng.max <- suppressWarnings(as.numeric(tclvalue(lng.max.var)))
      lat.min <- suppressWarnings(as.numeric(tclvalue(lat.min.var)))
      lat.max <- suppressWarnings(as.numeric(tclvalue(lat.max.var)))
      alt.min <- suppressWarnings(as.numeric(tclvalue(alt.min.var)))
      alt.max <- suppressWarnings(as.numeric(tclvalue(alt.max.var)))

      # Polygon box domain
      poly.obj <- NULL
      poly.file <- as.character(tclvalue(poly.file.var))
      if (poly.file != "" && file.access(poly.file, mode=0) == 0) {
        poly.obj <- read.polyfile(poly.file, nohole=FALSE)
        if (inherits(poly.obj, "gpc.poly")) {
          poly.bbox <- get.bbox(poly.obj)
          lng.min <- min(lng.min, poly.bbox$x[1], na.rm=TRUE)
          lng.max <- max(lng.max, poly.bbox$x[2], na.rm=TRUE)
          lat.min <- min(lat.min, poly.bbox$y[1], na.rm=TRUE)
          lat.max <- max(lat.max, poly.bbox$y[2], na.rm=TRUE)
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
        sel <- sel[rowSums(is.na(sel)) == 0, ]
      }
    }

    # Return selection
    if (inherits(sel, "data.frame") && nrow(sel) > 0)
      return(list(sites=sel, polygons=poly.obj))
    else
      stop(sel)
  }


  # Main program

  for (i in c("tcltk", "sp", "RODBC", "gpclib"))
    suppressPackageStartupMessages(require(i, character.only=TRUE))

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

  vars <- list('lat' = "dec_lat_va",
               'lng' = "dec_long_va",
               'alt' = "alt_va",
               'site' = "site_no",
               'name' = "station_nm",
               'agency' = "agency_cd",
               'type' = "site_tp_cd")

  # Initialize top-level variables

  con <- NULL
  site.vars <- NULL
  data.vars <- NULL
  retr.vars <- NULL
  initialdir <- NULL

  # Assign variables linked to Tk widgets

  dsn.var <- tclVar()
  opt.var <- tclVar(1)
  site.no.var <- tclVar()
  site.file.var <- tclVar()
  poly.file.var <- tclVar()
  site.type.var <- tclVar()
  date.time.var <- tclVar()
  for (i in c("All ...", names(site.types)))
    tcl("lappend", site.type.var, i)

  lng.min.var <- tclVar()
  lng.max.var <- tclVar()
  lat.min.var <- tclVar()
  lat.max.var <- tclVar()
  alt.min.var <- tclVar()
  alt.max.var <- tclVar()

  site.var <- tclVar()
  data.var <- tclVar()
  retr.var <- tclVar()
  tmin.var <- tclVar()
  tmax.var <- tclVar()

  data.type.var <- tclVar()

  tt.done.var <- tclVar(0)

  # Create arrow image bitmaps

  bits <- c('0x00', '0x00', '0x20', '0x00', '0x30', '0x00', '0x38', '0x00',
            '0xfc', '0x01', '0xfe', '0x01', '0xfc', '0x01', '0x38', '0x00',
            '0x30', '0x00', '0x20', '0x00', '0x00', '0x00')
  arrow.left <- tkimage.create("bitmap", data=as.tclObj(BitsToString(bits)))

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

  # Open GUI

  tclServiceMode(FALSE)
  tt <- tktoplevel(padx=0, pady=0)
  tktitle(tt) <- "National Water Information System: R Interface"

  # Create menus

  top.menu <- tkmenu(tt, tearoff=0)

  menu.file <- tkmenu(tt, tearoff=0, relief="flat")
  tkadd(top.menu, "cascade", label="File", menu=menu.file, underline=0)
  tkadd(menu.file, "command", label="Open", accelerator="Ctrl+O",
        command=function() print("notyet"))
  tkadd(menu.file, "command", label="Save", accelerator="Ctrl+S",
        command=function() print("notyet"))
  tkadd(menu.file, "command", label="Save as", accelerator="Shift+Ctrl+S",
        command=function() print("notyet"))
  tkadd(menu.file, "separator")
  tkadd(menu.file, "command", label="Exit",
        command=CloseGUI)

  if (!"RNWIS" %in% .packages()) {
    if ("RSurvey" %in% .packages(all.available=TRUE)) {
      suppressPackageStartupMessages(require("RSurvey"))
      tkadd(menu.file, "separator")
      tkadd(menu.file, "command", label="Restore R session",
            command=function() {
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
                          text="Select a registerd ODBC database")

  frame1.lab.1.1 <- ttklabel(frame1, text="NWIS source name")
  frame1.box.1.2 <- ttkcombobox(frame1, textvariable=dsn.var, state="readonly")
  frame1.but.1.3 <- ttkbutton(frame1, width=8, text="Explore",
                              command=function() ExploreDatabase(con, tt))

  tkgrid(frame1.lab.1.1, frame1.box.1.2, frame1.but.1.3,
         padx=c(0, 2), pady=3, sticky="we")
  tkgrid.configure(frame1.lab.1.1, sticky="e")

  tkgrid.columnconfigure(frame1, 1, weight=1, minsize=25)

  tkpack(frame1, fill="x", expand=FALSE, padx=15, pady=15)

  tkbind(frame1.box.1.2, "<<ComboboxSelected>>", OpenConnection)

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
                              command=function() BrowseForFile("Text",
                                                               site.file.var))

  frame3 <- ttkframe(frame2, relief="flat")

  frame3.lab.1.2 <- ttklabel(frame3, text="Longitude")
  frame3.lab.1.3 <- ttklabel(frame3, text="Latitude")
  frame3.lab.1.4 <- ttklabel(frame3, text="Altitude")
  frame3.lab.1.6 <- ttklabel(frame3, text="Select type(s)")
  frame3.lab.2.1 <- ttklabel(frame3, text="Minimum")
  frame3.lab.3.1 <- ttklabel(frame3, text="Maximum")

  width <- 15
  frame3.ent.2.2 <- ttkentry(frame3, width=width, textvariable=lng.min.var)
  frame3.ent.2.3 <- ttkentry(frame3, width=width, textvariable=lat.min.var)
  frame3.ent.2.4 <- ttkentry(frame3, width=width, textvariable=alt.min.var)
  frame3.ent.3.2 <- ttkentry(frame3, width=width, textvariable=lng.max.var)
  frame3.ent.3.3 <- ttkentry(frame3, width=width, textvariable=lat.max.var)
  frame3.ent.3.4 <- ttkentry(frame3, width=width, textvariable=alt.max.var)

  frame3.lst.2.6 <- tklistbox(frame3, selectmode="extended", activestyle="none",
                              relief="flat", borderwidth=5, width=15, height=6,
                              exportselection=FALSE, listvariable=site.type.var,
                              highlightthickness=0)
  tkselection.set(frame3.lst.2.6, 0)

  frame3.lab.4.1 <- ttklabel(frame3, foreground="#414042", text="e.g.")
  frame3.lab.4.2 <- ttklabel(frame3, foreground="#414042", text="-112.980728")
  frame3.lab.4.3 <- ttklabel(frame3, foreground="#414042", text="43.510023")
  frame3.lab.4.4 <- ttklabel(frame3, foreground="#414042", text="4382.3")

  frame3.lab.5.1 <- ttklabel(frame3, text="Polygon domain")
  frame3.ent.5.2 <- ttkentry(frame3, width=25, textvariable=poly.file.var)
  frame3.but.5.5 <- ttkbutton(frame3, width=8, text="Browse",
                              command=function() BrowseForFile("Polygon",
                                                               poly.file.var))

  tkgrid(frame2.rad.1.1)
  tkgrid(frame2.ent.2.1, columnspan=2, pady=c(0, 4))
  tkgrid(frame2.rad.3.1)
  tkgrid(frame2.ent.4.1, frame2.but.4.2, pady=c(0, 4))
  tkgrid(frame2.rad.5.1)

  tkgrid.configure(frame2.rad.1.1, frame2.rad.3.1, frame2.rad.5.1,
                   sticky="w", columnspan=2)

  tkgrid.configure(frame2.ent.2.1, frame2.ent.4.1, sticky="we", padx=c(20, 2))

  tkgrid(frame3, columnspan=2, sticky="we")

  tkgrid("x", frame3.lab.1.2, frame3.lab.1.3, frame3.lab.1.4, "x",
         frame3.lab.1.6, pady=c(0, 1))
  tkgrid.configure(frame3.lab.1.6, padx=c(20, 0))

  tkgrid(frame3.lab.2.1, frame3.ent.2.2, frame3.ent.2.3, frame3.ent.2.4,
         "x", frame3.lst.2.6, padx=1, pady=c(0, 1), sticky="we")

  tkgrid(frame3.lab.3.1, frame3.ent.3.2, frame3.ent.3.3, frame3.ent.3.4,
         padx=1, pady=c(1, 0), sticky="we")

  tkgrid.configure(frame3.lab.2.1, frame3.lab.3.1, sticky="e", padx=c(20, 0))

  tkgrid(frame3.lab.4.1, frame3.lab.4.2, frame3.lab.4.3, frame3.lab.4.4)
  tkgrid.configure(frame3.lab.4.1, sticky="e")

  tkgrid.configure(frame3.lab.1.4, frame3.ent.2.4, frame3.ent.3.4,
                   frame3.lab.4.4, columnspan=2)

  tkgrid(frame3.lab.5.1, frame3.ent.5.2, "x", "x", frame3.but.5.5, pady=c(7, 0))
  tkgrid.configure(frame3.lab.5.1, padx=c(20, 0), sticky="e")
  tkgrid.configure(frame3.ent.5.2, columnspan=3, sticky="we", padx=c(1, 2))

  tkgrid.configure(frame3.lst.2.6, rowspan=4, padx=c(20, 0),
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
                              relief="flat", borderwidth=5, width=15, height=8,
                              exportselection=FALSE, listvariable=site.var,
                              highlightthickness=0)
  frame4.lst.2.3 <- tklistbox(frame4, selectmode="extended", activestyle="none",
                              relief="flat", borderwidth=5, width=15, height=8,
                              exportselection=FALSE, listvariable=data.var,
                              highlightthickness=0)
  frame4.lst.2.6 <- tklistbox(frame4, selectmode="extended", activestyle="none",
                              relief="flat", borderwidth=5, width=15, height=8,
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
  frame4.but.3.5 <- ttkbutton(frame4, width=2, image=arrow.left,
                              command=RemoveVariables)

  frame4.box.4.3 <- ttkcombobox(frame4, state="readonly",
                                textvariable=data.type.var, width=10)

  frame5 <- ttkframe(frame4, relief="flat")
  frame5.but.1.1 <- ttkbutton(frame5, width=2, image=arrow.up,
                              command=function() Arrange("up",
                                                         frame4.lst.2.6))
  frame5.but.1.2 <- ttkbutton(frame5, width=2, image=arrow.down,
                              command=function() Arrange("down",
                                                         frame4.lst.2.6))

  tkgrid(frame4.lab.1.1, "x", frame4.lab.1.3, "x", "x", frame4.lab.1.6, "x",
         pady=c(0, 1))
  tkgrid(frame4.lst.2.1, frame4.ysc.2.2, frame4.lst.2.3, frame4.ysc.2.4,
         frame4.but.2.5, frame4.lst.2.6, frame4.ysc.2.7)
  tkgrid("x", "x", "x", "x", frame4.but.3.5, "x", "x")
  tkgrid(frame4.lab.4.1, "x", frame4.box.4.3, "x", "x", frame5, "x")
  tkgrid.configure(frame5, columnspan=2)
  tkgrid(frame5.but.1.1, frame5.but.1.2, padx=c(0, 4), pady=c(4, 0))


  tkgrid.configure(frame4.lst.2.1, frame4.ysc.2.2, frame4.lst.2.3,
                   frame4.ysc.2.4, frame4.lst.2.6, frame4.ysc.2.7, rowspan=2)

  tkgrid.configure(frame4.lst.2.1, frame4.lst.2.3, frame4.lst.2.6,
                   sticky="nsew")
  tkgrid.configure(frame4.ysc.2.2, frame4.ysc.2.4, sticky="ns", padx=c(0, 20))
  tkgrid.configure(frame4.ysc.2.7, sticky="ns", padx=0)

  tkgrid.configure(frame4.but.2.5, sticky="sw", padx=c(0, 20), pady=c(0, 2))
  tkgrid.configure(frame4.but.3.5, sticky="nw", padx=c(0, 20), pady=c(2, 0))

  tkgrid.configure(frame4.lab.4.1, sticky="e", padx=c(0, 1), pady=c(4, 0),
                   columnspan=2)
  tkgrid.configure(frame4.box.4.3, sticky="we", pady=c(4, 0), padx=0)

  tkgrid.columnconfigure(frame4, 0, weight=1, minsize=15)
  tkgrid.columnconfigure(frame4, 2, weight=1, minsize=15)
  tkgrid.columnconfigure(frame4, 5, weight=1, minsize=15)

  tkgrid.rowconfigure(frame4, 1, weight=1)
  tkgrid.rowconfigure(frame4, 2, weight=1)

  tkpack(frame4, fill="both", expand=TRUE, ipadx=2, ipady=2,
         padx=8, pady=8)

  tkbind(frame4.box.4.3, "<<ComboboxSelected>>", UpdateDataVariables)

  # Frame 6, specify date range

  txt <- "Specify date and time range"
  frame6 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=3, text=txt)


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

  # GUI control

  SetState()
  tkbind(tt, "<Destroy>", CloseGUI)
  tkfocus(tt)
  tclServiceMode(TRUE)
  invisible()
}
