# Function to save timestamps for when an object is created into the file ".Rtimestamps" It only does this when options(recordTimestamps = TRUE).

tstamp <- function(yin)

     if (getOption("recordTimestamps")) {
     write.success <- try(write(
                         x = paste(
                         as.character(yin),
                         as.double(Sys.time()),
                         sep = ","),
                         file = ".Rtimestamps",
                         append = TRUE))
     
     return(write.success)
}
