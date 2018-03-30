# Function to strip the Chromosome number/name from the Location column in the primary data sets

strip.Chr <- function(in.data,j) {
     
     loc.temp.a <- sub("\\::*\\s[0-9]*\\.[0-9]*","",in.data[j]$Location)
     loc.temp.b <- lapply(loc.temp.a[j], function(x,j) gsub('Chr','',x), j)
     
     return(loc.temp.b)
}
