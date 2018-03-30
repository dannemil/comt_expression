# function to find gene Symbols in the four data frames for which the gene names are listed
# as LOXxxxx. Need to do this only at beginning of Symbol as in LOCxx not asx in BLOC1S1

genes.Are.locs <- function(in.data,j) {
     
#     loc.INDX <- grep('LOC',in.data[j]$Symbol)
     
     loc.INDX <- grep('LOC',substring(in.data[j]$Symbol,1,3))

               return(loc.INDX)
}    
