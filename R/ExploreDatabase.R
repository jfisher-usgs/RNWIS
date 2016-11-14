#' Explore Database Connection
#'
#' A graphical user interface for exploring the contents of a database.
#'
#' @param channel RODBC.
#'   Connection to the ODBC database.
#' @param parent tkwin.
#'    Parent window (optional)
#'
#' @details Shows database table names and the column structure for each table.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{sqlTables}}, \code{\link{sqlColumns}}, \code{\link{sqlPrimaryKeys}}
#'
#' @keywords misc
#'
#' @import tcltk
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   channel <- RODBC::odbcConnect("NWIS Idaho", uid = "", pwd = "")
#'   ExploreDatabase(channel)
#'   close(channel)
#' }
#'

ExploreDatabase <- function(channel, parent=NULL) {

  GetVariables <- function() {
    idx <- as.integer(tkcurselection(f1.lst.2.1))
    if (length(idx) == 0) return()
    table.name <- as.character(tkget(f1.lst.2.1, idx, idx))

    tkconfigure(tt, cursor="watch")
    tclServiceMode(FALSE)

    # clear treeview
    tree.children <- tcl(f2.tre.2.1, "children", "")
    tkdelete(f2.tre.2.1, tree.children)

    # table name on server
    sq.table <- paste(server.name, table.name, sep=".")

    # query column names
    cols <- RODBC::sqlColumns(channel, sqtable=sq.table)
    cols <- cols[cols[, "TABLE_NAME"] == table.name,
                 c("COLUMN_NAME", "TYPE_NAME", "NUM_PREC_RADIX", "CHAR_OCTET_LENGTH")]

    # query keys
    keys <- RODBC::sqlPrimaryKeys(channel, sqtable=sq.table)
    which.is.key <- which(cols[, "COLUMN_NAME"] %in% keys[, "COLUMN_NAME"])

    # tags, color styles in treeview
    tags <- as.list(rep(c("odd", "even"), nrow(cols) / 2 + 1))
    for (i in seq(along=which.is.key)) tags[[which.is.key[i]]][2] <- "key"

    # populate treeview
    for (i in seq(along=cols[, 1]))
       tkinsert(f2.tre.2.1, "", "end", tags=tags[[i]],
                text=cols[i, 1], values=c(cols[i, 2], cols[i, 3], cols[i, 4]))

    tkfocus(f1.lst.2.1)
    tkconfigure(tt, cursor="arrow")
    tclServiceMode(TRUE)
  }


  if (inherits(channel, "RODBC"))
    channel <- RODBC::odbcReConnect(channel)
  else
    channel <- RODBC::odbcConnect(channel, uid="", pwd="", readOnlyOptimize=TRUE)
  if (channel < 0)
    stop("error occurred when opening connection to ODBC database")
  on.exit(close(channel))

  server.name <- RODBC::odbcGetInfo(channel)[["Server_Name"]]

  sql.tables <- RODBC::sqlTables(channel, errors=FALSE, as.is=TRUE)
  sql.tables <- sql.tables[sql.tables[, "TABLE_TYPE"] == "TABLE", ]
  sql.tables <- sort(sql.tables[, "TABLE_NAME"])

  help.url <- "http://nwis.usgs.gov/dbms/"

  # assign variables linked to Tk widgets
  tables.var <- tclVar()
  for (i in seq(along=sql.tables)) tcl("lappend", tables.var, sql.tables[i])
  vars.var <- tclVar()
  tt.done.var <- tclVar(0)

  # open gui
  tclServiceMode(FALSE)
  tt <- tktoplevel(padx=0, pady=0)
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    tmp <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste0("+", as.integer(tmp[2]) + 25,
                             "+", as.integer(tmp[3]) + 25))
  }
  tktitle(tt) <- "Explore Database"

  # frame 0, ok and cancel buttons
  f0 <- ttkframe(tt, relief="flat")
  f0.but.1 <- ttkbutton(f0, width=12, text="Help",
                        command=function() utils::browseURL(help.url))
  f0.but.2 <- ttkbutton(f0, width=12, text="Close",
                        command=function() tclvalue(tt.done.var) <- 1)
  f0.grp.3 <- ttksizegrip(f0)
  tkgrid(f0.but.1, f0.but.2, f0.grp.3)
  tkgrid.configure(f0.but.1, f0.but.2, sticky="e", padx=2, pady=c(12, 10))
  tkgrid.configure(f0.grp.3, sticky="se")
  tkpack(f0, side="bottom", anchor="e")

  # paned window
  pw <- ttkpanedwindow(tt, orient="horizontal")

  # frame 1, tables
  f1 <- ttkframe(pw, relief="flat", borderwidth=0, padding=0)

  f1.lab.1.1 <- ttklabel(f1, text="Select table")
  f1.lst.2.1 <- tklistbox(f1, selectmode="browse", activestyle="none",
                          relief="flat", borderwidth=5, exportselection=FALSE,
                          listvariable=tables.var, highlightthickness=0,
                          width=30, height=10)
  f1.ysc.2.2 <- ttkscrollbar(f1, orient="vertical")
  tkconfigure(f1.lst.2.1, background="white",
              yscrollcommand=paste(.Tk.ID(f1.ysc.2.2), "set"))
  tkconfigure(f1.ysc.2.2, command=paste(.Tk.ID(f1.lst.2.1), "yview"))

  tkgrid(f1.lab.1.1, "x")
  tkgrid(f1.lst.2.1, f1.ysc.2.2)

  tkgrid.configure(f1.lab.1.1, pady=c(0, 3), sticky="w")
  tkgrid.configure(f1.lst.2.1, sticky="nswe")
  tkgrid.configure(f1.ysc.2.2, sticky="ns", padx=c(0, 2))

  tkgrid.rowconfigure(f1, 1, weight=1)
  tkgrid.columnconfigure(f1, 0, weight=1, minsize=6)

  tkselection.set(f1.lst.2.1, 0)

  tkbind(f1.lst.2.1, "<ButtonRelease-1>", GetVariables)

  # frame 2, variables
  f2 <- ttkframe(pw, relief="flat", borderwidth=0, padding=0)

  f2.lab.1.1 <- ttklabel(f2, text="Table summary")

  columns <- c("type.name", "num.prec.radix", "char.octet.length")

  tcl("ttk::style", "configure", "Custom.Treeview", rowheight=15)
  f2.tre.2.1 <- ttktreeview(f2, selectmode="browse", columns=columns)
  tkconfigure(f2.tre.2.1, style="Custom.Treeview")
  tktag.configure(f2.tre.2.1, "odd", background="#FFFFFF")
  tktag.configure(f2.tre.2.1, "even", background="#ECF3F7")
  tktag.configure(f2.tre.2.1, "key", foreground="#E60000")

  f2.ysc.2.2 <- ttkscrollbar(f2, orient="vertical")
  tkconfigure(f2.tre.2.1, yscrollcommand=paste(.Tk.ID(f2.ysc.2.2), "set"))
  tkconfigure(f2.ysc.2.2, command=paste(.Tk.ID(f2.tre.2.1), "yview"))

  tcl(f2.tre.2.1, "column", "#0", width=100, minwidth=80)
  tcl(f2.tre.2.1, "column", columns[1], width=100, minwidth=80, anchor="center")
  tcl(f2.tre.2.1, "column", columns[2], width=100, minwidth=80, anchor="e")
  tcl(f2.tre.2.1, "column", columns[3], width=100, minwidth=80, anchor="e")

  tcl(f2.tre.2.1, "heading", "#0", text="Name")
  tcl(f2.tre.2.1, "heading", columns[1], text="Type")
  tcl(f2.tre.2.1, "heading", columns[2], text="Precision")
  tcl(f2.tre.2.1, "heading", columns[3], text="Length")

  tkgrid(f2.lab.1.1, "x")
  tkgrid(f2.tre.2.1, f2.ysc.2.2)

  tkgrid.configure(f2.lab.1.1, pady=c(0, 3), sticky="w")
  tkgrid.configure(f2.tre.2.1, sticky="nswe")
  tkgrid.configure(f2.ysc.2.2, sticky="ns", padx=c(0, 2))

  tkgrid.rowconfigure(f2, 1, weight=1)
  tkgrid.columnconfigure(f2, 0, weight=1, minsize=6)

  # final layout
  tkgrid(f1, f2, padx=2, pady=2, sticky="nswe")
  tkadd(pw, f1, weight=0)
  tkadd(pw, f2, weight=1)
  tkpack(pw, fill="both", expand=TRUE, padx=15, pady=5)

  # gui control
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
