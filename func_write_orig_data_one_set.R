# Function to write out the primary data files after recovering additional values for various variables.

write.orig.one <- function(ax,namex)  {
     
# Interim: write back out the primary data files after finding missing values for various variables
# Note: they are written as txt files to prevent Excel from converting gene symbols such as MARCH8 to date format  
# write to files
     
# Using the extension, .txt, allows these to be imported into excel so that the column with gene Symbols can be set to text, preventing Excel from converting gene Symbols like MARCH1 to date format.
     success.write <- is.null(try(write.csv(as.data.frame(ax),paste("/Volumes/Macintosh_HD_3/genetics/genenetwork2/comt_",namex,"_correlation_results.csv.txt",sep=""),row.names=FALSE)))
# # # # # #

    return(success.write)
    
}
