# Function to get the number of missing gene symbols and ENTREZIDs at each step of processing

get.miss.Stats <- function(x,i,cnames,rnames) {
     
     miss.Temp <- list()
     
     miss.Temp <- data.frame(miss.Symb=num.NA(x$Symbol),
                             miss.entrez=num.NA(x$ENTREZID))
     
     return(miss.Temp)
}
