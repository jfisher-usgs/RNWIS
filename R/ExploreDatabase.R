ExploreDatabase <- function(channel, parent=NULL) {
  # GUI for exploring contents of database.

  # Additional functions (subroutines)

  # Retrieve column structure for a database table.

  GetVariables <- function() {
    idx <- as.integer(tkcurselection(frame1.lst.2.1))
    if (length(idx) == 0)
      return()
    table.name <- as.character(tkget(frame1.lst.2.1, idx, idx))

    tkconfigure(tt, cursor="watch")
    tclServiceMode(FALSE)

    # Clear treeview
    tree.children <- tcl(frame2.tre.2.1, "children", "")
    tkdelete(frame2.tre.2.1, tree.children)

    # Query column names
    cols <- sqlColumns(channel, sqtable=table.name)

    cols <- cols[cols[, "TABLE_NAME"] == table.name,
                 c("COLUMN_NAME", "TYPE_NAME", "NUM_PREC_RADIX",
                   "CHAR_OCTET_LENGTH")]

    # Query keys
    keys <- sqlPrimaryKeys(channel, sqtable=table.name)
    which.is.key <- which(cols[, "COLUMN_NAME"] %in% keys[, "COLUMN_NAME"])

    # Tags, color styles in treeview
    tags <- as.list(rep(c("odd", "even"), nrow(cols) / 2 + 1))
    for (i in seq(along=which.is.key))
      tags[[which.is.key[i]]][2] <- "key"

    # Populate treeview
    for (i in seq(along=cols[, 1]))
       tkinsert(frame2.tre.2.1, "", "end", tags=tags[[i]],
                text=cols[i, 1], values=c(cols[i, 2], cols[i, 3], cols[i, 4]))

    tkfocus(frame1.lst.2.1)
    tkconfigure(tt, cursor="arrow")
    tclServiceMode(TRUE)
  }


  # Main program

  if (inherits(channel, "RODBC"))
    channel <- odbcReConnect(channel)
  else
    channel <- odbcConnect(channel, uid="", pwd="", readOnlyOptimize=TRUE)
  if (channel < 0)
    stop("error occurred when opening connection to ODBC database")
  on.exit(close(channel))

  sql.tables <- sqlTables(channel, errors=FALSE, as.is=TRUE)[, "TABLE_NAME"]
  sql.tables <- sort(sql.tables)

  help.url <- "http://nwis.usgs.gov/dbms/" # only available on USGS intranet

  # Assign variables linked to Tk widgets

  tables.var <- tclVar()
  for (i in seq(along=sql.tables))
    tcl("lappend", tables.var, sql.tables[i])
  vars.var <- tclVar()
  tt.done.var <- tclVar(0)

  # Open GUI

  tclServiceMode(FALSE)
  tt <- tktoplevel(padx=0, pady=0)
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    tmp <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(tmp[2]) + 25,
                            "+", as.integer(tmp[3]) + 25, sep=""))
  }
  tktitle(tt) <- "Explore Database"

  # Frame 0, ok and cancel buttons

  frame0 <- ttkframe(tt, relief="flat")

  frame0.but.1 <- ttkbutton(frame0, width=12, text="Help",
                            command=function() browseURL(help.url))
  frame0.but.2 <- ttkbutton(frame0, width=12, text="Close",
                            command=function() tclvalue(tt.done.var) <- 1)
  frame0.grp.3 <- ttksizegrip(frame0)
  tkgrid(frame0.but.1, frame0.but.2, frame0.grp.3)
  tkgrid.configure(frame0.but.1, frame0.but.2, sticky="e",
                   padx=2, pady=c(12, 10))
  tkgrid.configure(frame0.grp.3, sticky="se")
  tkpack(frame0, side="bottom", anchor="e")

  # Paned window

  pw <- ttkpanedwindow(tt, orient="horizontal")

  # Frame 1, tables

  frame1 <- ttkframe(pw, relief="flat", borderwidth=0, padding=0)

  frame1.lab.1.1 <- ttklabel(frame1, text="Select table")
  frame1.lst.2.1 <- tklistbox(frame1, selectmode="browse", activestyle="none",
                relief="flat", borderwidth=5, exportselection=FALSE,
                listvariable=tables.var, highlightthickness=0,
                width=30, height=10)
  frame1.ysc.2.2 <- ttkscrollbar(frame1, orient="vertical")
  tkconfigure(frame1.lst.2.1, background="white",
              yscrollcommand=paste(.Tk.ID(frame1.ysc.2.2), "set"))
  tkconfigure(frame1.ysc.2.2, command=paste(.Tk.ID(frame1.lst.2.1), "yview"))

  tkgrid(frame1.lab.1.1, "x")
  tkgrid(frame1.lst.2.1, frame1.ysc.2.2)

  tkgrid.configure(frame1.lab.1.1, pady=c(0, 3), sticky="w")
  tkgrid.configure(frame1.lst.2.1, sticky="nswe")
  tkgrid.configure(frame1.ysc.2.2, sticky="ns", padx=c(0, 2))

  tkgrid.rowconfigure(frame1, 1, weight=1)
  tkgrid.columnconfigure(frame1, 0, weight=1, minsize=6)

  tkselection.set(frame1.lst.2.1, 0)

  tkbind(frame1.lst.2.1, "<ButtonRelease-1>", GetVariables)

  # Frame 2, variables

  frame2 <- ttkframe(pw, relief="flat", borderwidth=0, padding=0)

  frame2.lab.1.1 <- ttklabel(frame2, text="Table summary")

  columns <- c("type.name", "num.prec.radix", "char.octet.length")

  tcl("ttk::style", "configure", "Custom.Treeview", rowheight=15)
  frame2.tre.2.1 <- ttktreeview(frame2, selectmode="browse", columns=columns)
  tkconfigure(frame2.tre.2.1, style="Custom.Treeview")
  tktag.configure(frame2.tre.2.1, "odd", background="#FFFFFF")
  tktag.configure(frame2.tre.2.1, "even", background="#ECF3F7")
  tktag.configure(frame2.tre.2.1, "key", foreground="#E60000")

  frame2.ysc.2.2 <- ttkscrollbar(frame2, orient="vertical")
  tkconfigure(frame2.tre.2.1,
              yscrollcommand=paste(.Tk.ID(frame2.ysc.2.2), "set"))
  tkconfigure(frame2.ysc.2.2, command=paste(.Tk.ID(frame2.tre.2.1), "yview"))

  tcl(frame2.tre.2.1, "column", "#0", width=100, minwidth=80)
  tcl(frame2.tre.2.1, "column", columns[1], width=100, minwidth=80,
      anchor="center")
  tcl(frame2.tre.2.1, "column", columns[2], width=100, minwidth=80, anchor="e")
  tcl(frame2.tre.2.1, "column", columns[3], width=100, minwidth=80, anchor="e")

  tcl(frame2.tre.2.1, "heading", "#0", text="Name")
  tcl(frame2.tre.2.1, "heading", columns[1], text="Type")
  tcl(frame2.tre.2.1, "heading", columns[2], text="Precision")
  tcl(frame2.tre.2.1, "heading", columns[3], text="Length")

  tkgrid(frame2.lab.1.1, "x")
  tkgrid(frame2.tre.2.1, frame2.ysc.2.2)

  tkgrid.configure(frame2.lab.1.1, pady=c(0, 3), sticky="w")
  tkgrid.configure(frame2.tre.2.1, sticky="nswe")
  tkgrid.configure(frame2.ysc.2.2, sticky="ns", padx=c(0, 2))

  tkgrid.rowconfigure(frame2, 1, weight=1)
  tkgrid.columnconfigure(frame2, 0, weight=1, minsize=6)

  # Final layout

  tkgrid(frame1, frame2, padx=2, pady=2, sticky="nswe")
  tkadd(pw, frame1, weight=0)
  tkadd(pw, frame2, weight=1)
  tkpack(pw, fill="both", expand=TRUE, padx=15, pady=5)

  # GUI control

  GetVariables()

  tkfocus(tt)
  tkgrab(tt)
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tclServiceMode(TRUE)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  invisible()
}
