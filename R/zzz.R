.onLoad <- function(...) {
  if (interactive())
    OpenRNWIS()
  else
    return()
}
