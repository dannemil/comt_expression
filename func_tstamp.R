# Function to save timestamps for when an object is created into the file ".Rtimestamps" It only does this when options(recordTimestamps = TRUE).

tstamp <- function(yin)
     
     if (getOption("recordTimestamps")) {
     wr.success <- try(write(
                         x = paste(
                         deparse(substitute(yin)),   # convert to character string
#                        as.double(Sys.time()),
                         as.POSIXct(origin = "1970-01-01", x = as.double(Sys.time())),
                         sep = ","),
                         file = ".Rtimestamps",
                         append = TRUE))
     
     if (is.null(wr.success)) {
          write.success <- c("timestamp recorded")}
     else {
          write.success <- c("timestamp failed")
     }
     
     return(write.success)
}
