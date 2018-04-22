### Function to determine how many NAs are in a vector and where the NA entries are
num.NA <- function(in.vec) {
     
     out.count.NA <- length(which(is.na(in.vec)))
     return(out.count.NA)
}

