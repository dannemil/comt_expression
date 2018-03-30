# function to get gene start, end positions and lengths

start.end.Length <- function(in.Data)  {

     # Only the Symbol column is input to this function     
     # gene.Starts <- getBM(c("hgnc_symbol", "chromosome_name", "start_position", "end_position"),
     #                     filters = c("hgnc_symbol"),
     #                     values = list(hgnc_symbol=in.Data),
     #                     mart = ensembl)
     
     gene.Starts <- getBM(attributes = c("illumina_humanref_8_v3", "hgnc_symbol",
                                         "chromosome_name", "start_position","end_position",
                                         "entrezgene"), filters = "illumina_humanref_8_v3", 
                          values = as.character(in.Data), mart = ensembl)

     
     return(gene.Starts)
     
     
}
