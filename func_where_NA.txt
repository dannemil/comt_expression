# function to determine the indices of NA entries in a vector

where.NA <- function(in.vec) {
     
     posNA <- which(is.na(in.vec))
     return(posNA)
}