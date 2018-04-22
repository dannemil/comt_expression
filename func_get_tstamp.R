# Function to get the timestamp for the first time an object was created ctime() and the last time it was changed mtime(). Format into standard date/time format.

ctime <- function(object) {
     target_object <- deparse(substitute(object))
     get_cmtime(target_object, 1)
}


mtime <- function(object) {
     target_object <- deparse(substitute(object))
     get_cmtime(target_object, 2)
}

get_cmtime <- function(target_object, mode) {
     if (!is.null(getOption("recordTimestamps")) & interactive()) {
          # only get dates when set in options and when in interactive mode
          if (getOption("recordTimestamps") & file.exists(".Rtimestamps")) {
               lines <- readLines(con <- file(".Rtimestamps"), warn = FALSE, encoding = "UTF-8")
               close(con)
               target_lines <- lines[grepl(paste0("^(", target_object, "),"), lines)]
               if (length(target_lines) > 0) {
                    if (mode == 1) {
                         # get first value, second element
                         timestamp <- unlist(strsplit(target_lines[1], ","))[2]
                    } else if (mode == 2) {
                         # get last value, second element
                         timestamp <- unlist(strsplit(target_lines[length(target_lines)], ","))[2]
                    }
                    ## transform to date
                    return(as.POSIXct(origin = "1970-01-01", x = as.double(timestamp)))
               } else {
                    return(NA)
               }
          }
     }
}
