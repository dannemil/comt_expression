# Function to keep track of R script names and their unique IDs.

rec.script <- function(Rin,ui)   {
     
     fileConn<-file(".Rscriptnames")
          fd <- data.frame(read.csv(fileConn))
#     close(fileConn)

flag.exist <- FALSE 
yt <- c(1)

while(!flag.exist & yt < (dim(fd)[1] + 1))   {

     if (grepl(ui,fd[yt,2]))  {
          
         flag.exist <- TRUE 
          
     } else {
          
     }
     
     yt <- inc(yt,)

}

if(!flag.exist) {

wr.success <- try(write(
     x = paste(
          #                    deparse(substitute(Rin)),   # convert to character string
          Rin,
          #                    deparse(substitute(ui)),
          ui,
          #                        as.double(Sys.time()),
          as.POSIXct(origin = "1970-01-01", x = as.double(Sys.time())),
          sep = ", "),
     file = ".Rscriptnames",
     append = TRUE))

if (is.null(wr.success)) {
     write.success <- c("script name and id recorded")}
else {
     write.success <- c("script name and id failed")
}

} else {

     write.success <- c('the program has already been recorded')
}

     return(write.success)
}
