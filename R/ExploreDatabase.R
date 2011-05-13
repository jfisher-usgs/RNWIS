ExploreDatabase <- function(con, parent=NULL) {
  # A GUI for exploring a database connection

  # Additional functions (subroutines)

  GetVariables <- function() {
    idx <- as.integer(tkcurselection(frame1.lst.2.1))
    if (length(idx) == 0)
      return()
    table.name <- as.character(tkget(frame1.lst.2.1, idx, idx))

    sql.table.cols <- sqlColumns(con, sqtable=table.name)[, "COLUMN_NAME"]

    tcl("lset", vars.var, "")
    for (i in seq(along=sql.table.cols))
      tcl("lappend", vars.var, sql.table.cols[i])

    tkfocus(frame1.lst.2.1)
  }


  # Main program

  sql.tables <- sqlTables(con, errors=FALSE, as.is=TRUE)[, "TABLE_NAME"]
  sql.tables <- sort(sql.tables)

  help.url <- "http://nwis.usgs.gov/dbms/"

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

  frame1.lab.1.1 <- ttklabel(frame1, text="Database tables")

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

  frame2.lab.1.1 <- ttklabel(frame2, text="Table variables")

  frame2.lst.2.1 <- tklistbox(frame2, selectmode="browse", activestyle="none",
                relief="flat", borderwidth=5, exportselection=FALSE,
                listvariable=vars.var, highlightthickness=0,
                width=30, height=10)
  frame2.ysc.2.2 <- ttkscrollbar(frame2, orient="vertical")
  tkconfigure(frame2.lst.2.1, background="white",
              yscrollcommand=paste(.Tk.ID(frame2.ysc.2.2), "set"))
  tkconfigure(frame2.ysc.2.2, command=paste(.Tk.ID(frame2.lst.2.1), "yview"))

  tkgrid(frame2.lab.1.1, "x")
  tkgrid(frame2.lst.2.1, frame2.ysc.2.2)

  tkgrid.configure(frame2.lab.1.1, pady=c(0, 3), sticky="w")
  tkgrid.configure(frame2.lst.2.1, sticky="nswe")
  tkgrid.configure(frame2.ysc.2.2, sticky="ns", padx=c(0, 2))

  tkgrid.rowconfigure(frame2, 1, weight=1)
  tkgrid.columnconfigure(frame2, 0, weight=1, minsize=6)

  # Final layout

  tkgrid(frame1, frame2, padx=2, pady=2, sticky="nswe")

  tkadd(pw, frame1, weight=1)
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
