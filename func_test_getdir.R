# Program to read in simulation data and compute various quantities
library(readxl,funr)

program.Dir <- getwd()
setwd(program.Dir)


#curr.scr <- setwd(dirname(rstudioapi::callFun("getActiveDocumentContext)"$path))
#' current script file (in full path)
#' @param
#' @return
#' @examples
#' works with Rscript, source() or in RStudio Run selection
#' @export
csf <- function() {
  # http://stackoverflow.com/a/32016824/2292993
  cmdArgs = commandArgs(trailingOnly = FALSE)
  needle = "--file="
  match = grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript via command line
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  } else {
    ls_vars = ls(sys.frames()[[1]])
    if ("fileName" %in% ls_vars) {
      # Source'd via RStudio
      return(normalizePath(sys.frames()[[1]]$fileName))
    } else {
      if (!is.null(sys.frames()[[1]]$ofile)) {
        # Source'd via R console
        return(normalizePath(sys.frames()[[1]]$ofile))
      } else {
        # RStudio Run Selection
        # http://stackoverflow.com/a/35842176/2292993
        return(normalizePath(rstudioapi::getActiveDocumentContext()$path))
      }
    }
  }
}

