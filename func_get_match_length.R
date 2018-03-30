# Function to get the length of a vector as the num,ber of elements that match a given target.


len.match <- function(invec,targ) {
     
     
     len.out <- length(which(invec == targ))
     
     return(len.out)
     
}