.onAttach <- function(lib, pkg) {
  if (interactive() && .Machine$sizeof.pointer == 4) {
    ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
    msg <- "USGS Orphan Package:
            https://owi.usgs.gov/R/packages.html#orphan
            Deprecated - Development of this package has halted."
    packageStartupMessage(paste(strwrap(msg), collapse="\n"))
    OpenRNWIS()
  } else {
    packageStartupMessage("The RNWIS GUI is launched only in interactive 32-bit sessions.")
  }
  invisible()
}
