# Function to calculate the number of missing Chromosome names/numbers and gene start positions

get.miss.Chr.bp <- function(in.data,ji)  {
     
     un.num <- list()
     no.bp.num <- list()
     
     un.num <- lapply(major.Area[i], function(x,i) length(which(x$loc == 'Un')))
     no.bp.num <- lapply(major.Area[i], function(x,i) length(which(x$bp == 1.00)))
     
     miss.Chr.bp <- data.frame(matrix(c(un.num,
                                        no.bp.num),ncol=2))
     colnames(miss.Chr.bp) <- c('Missing Chr','Missing Start Positions')
     row.names(miss.Chr.bp) <- stand.Col.names
     
     miss.Chr.bp.tab <- xtable(miss.Chr.bp,
                               caption='Number of missing Chromosome names or numbers and number of missing gene start positions.')
     
     return(miss.Chr.bp.tab)
}
