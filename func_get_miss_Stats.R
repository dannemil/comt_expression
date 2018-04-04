# Function to get the number of missing gene symbols and ENTREZIDs at each step of processing

get.miss.Stats <- function(x,i,cnames,rnames) {
     
     miss.Temp <- list()
     
     miss.Temp <- data.frame(miss.Symb=num.NA(x$Symbol),
                             miss.entrez=num.NA(x$ENTREZID),
                             miss.loc=num.NA(x$loc),
                             miss.bp=num.NA(x$bp))
     
     return(miss.Temp)
}
