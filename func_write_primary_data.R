# Function to write out the primary data files after recovering additional values for various variables.

write.primary <- function(ax,bx,cx,dx)  {
     
# Interim: write back out the primary data files after finding missing values for various variables
# Note: they are written as txt files to prevent Excel from converting gene symbols such as MARCH8 to date format  
# write to files
     
     success.write <- data.frame(prefront=NA,
                                 cbell=NA,
                                 tempor=NA,
                                 pons=NA)
     
# Using the extension, .txt, allows these to be imported into excel so that the column with gene Symbols can be set to text, preventing Excel from converting gene Symbols like MARCH1 to date format.
     success.write$prefront <- is.null(try(write.csv(as.data.frame(ax),"/Volumes/Macintosh_HD_3/genetics/genenetwork2/comt_correlations_20000_prefrontal_augmentedNew.csv.txt",row.names=FALSE)))
# # # # # #
     success.write$cbell <- is.null(try(write.csv(as.data.frame(bx),"/Volumes/Macintosh_HD_3/genetics/genenetwork2/comt_correlations_20000_cerebellum_augmentedNew.csv.txt",row.names=FALSE)))
# # # # # #
     success.write$tempor <- is.null(try(write.csv(as.data.frame(cx),"/Volumes/Macintosh_HD_3/genetics/genenetwork2/comt_correlations_20000_temporal_augmentedNew.csv.txt",row.names=FALSE)))
# # # # # #
     success.write$pons <- is.null(try(write.csv(as.data.frame(dx),"/Volumes/Macintosh_HD_3/genetics/genenetwork2/comt_correlations_20000_pons_augmentedNew.csv.txt",row.names=FALSE)))
     
    return(success.write)
    
}
