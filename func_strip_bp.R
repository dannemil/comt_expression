# function to strip out the starting location (bp) from the Location column
# Chr22: 18.328774 -> 18.328774

strip.bp <- function(x,l) {
     
     temp.bp <- as.numeric(sub('.*\\:', '', x[l]$Location))
     
     return(temp.bp)
}
