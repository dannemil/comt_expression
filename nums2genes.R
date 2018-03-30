# function to find gene Symbols in the four data frames that are just numbers and to convert
# them into proper gene symbols. Some of thje symbols are numbers because Excel 
# converts gene names such as MARCH1 into dates, then into the integers representing the dates.

nums2genes <- function(in.data,indx.in) {
     
     new.gene.Symbols <- getBM(attributes = c("hgnc_symbol","entrezgene"), filters = "entrezgene",
                               values = as.character(in.data$ENTREZID[c(indx.in)]), 
                               mart = ensembl)
#     A: new.gene.Symbols
#     B: as.character(in.data$ENTREZID[c(indx.Symb)])
     
     map.B.to.A <- match(as.character(in.data$ENTREZID[c(indx.in)]),
                         new.gene.Symbols$entrezgene)
     
     in.data$Symbol[c(indx.in)] <- new.gene.Symbols$hgnc_symbol[c(map.B.to.A)]
     # Necessary because Excel automatically converts MARCH1 to a date format
     
          return(in.data$Symbol[c(indx.in)]) 
     
}