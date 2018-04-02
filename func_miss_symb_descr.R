# Function to calcu;ate the number of missing gene names (symbols)

get.num.miss.symbols <- function(in.data,ji)  {
     
     missing.Data.symbol <- list()
     missing.Data.gene <- list()
     missing.Data <- data.frame(Missing_Gene_Symbols=rep(NA,4),
                                Missing_Gene_Descriptions=rep(NA,4))
     
     missing.Data.symbol <- lapply(in.data[ji], function(x,ji) sum(is.na(x[ji]$Symbol)))
     missing.Data.gene <- lapply(in.data[ji], function(x,ji) sum(is.na(x[ji]$Description)))
     
     missing.Data[,1] <- unlist(missing.Data.symbol)
     missing.Data[,2] <- unlist(missing.Data.gene)
     
     row.names(missing.Data) <- stand.Col.names
     
     
     num.miss.tab <- xtable(missing.Data,
                        caption=c('Missing gene symbols and gene descriptions by brain area.')) 
     
     return(num.miss.tab)
     
}
