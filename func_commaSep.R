### Function to take a vector of numbers, and turn it into a comma-separated string.
comma.Sep <- function(in.vec) {

     out.Sep <- matrix(rep(NA,((nchar(in.vec[1,2]) +1)*dim(in.vec)[1])),
                       ncol=dim(in.vec)[1])
     for (i in 1:dim(in.vec)[1]) {
          
          out.Sep[2:dim(out.Sep)[1],i] <- unlist(strsplit(in.vec[[2]][i], ""))
          out.Sep[1,i] <- in.vec[[1]][i]

     } # end number of promoters loop      
     
     return(out.Sep)

} # end function