#' Get object size in megabytes (MB).
#' 
#' This function takes an object in the current environment and returns the size of its memory footprint in Mb. This function is a wrapper for object.size() that returns the memory size in bytes.
#' 
#' @aliases objsize objSize objSizeMb
#' @export
#' @author James L. Dannemiller \email{dannemil@rice.edu}
#' @keywords memory

#' @param xv An object (e.g., a data.frame, matrix, list(), etc.).
#' @return The memory size of the object in Mb.

#' @examples In these examples, both prefront.Data and illumina8v2.Data are data.frames.
#'        objSizeMb(object1)
#'        objSizeMb(illumina8v2.Data)
#'        
#'        \dontrun{
#'        objSizeMb(illumina8v2.Data)
#'        }
#' 

objSizeMb <- function(xv)   {
     
     osize <- round((object.size(xv)/1000000),3)
     
     osize <- c(paste(osize,' Mb',sep=''))
     
     return(osize)
     
}


