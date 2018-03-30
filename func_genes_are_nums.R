# function to find gene Symbols in the four data frames that are numbers. Some of the 
# symbols are numbers because Excel converts gene names such as MARCH1 into dates, 
# then into the integers representing the dates.

genes.Are.nums <- function(in.data) {
     gene.Symbol.table <- as.data.frame(table(in.data$Symbol))
     
     # Find the indices of the gene names that are actually numbers
     num.Or.lett <- grepl('[A-Z]',substring(as.character(gene.Symbol.table[,1]),1,1))
     genes.As.nums <- length(subset(num.Or.lett,num.Or.lett == FALSE))
#     genes.As.nums <- subset(num.Or.lett,num.Or.lett == TRUE)
     
     indx.Symb <- which(in.data$Symbol %in% as.character(gene.Symbol.table[c(genes.As.nums),1]))
     
     return(indx.Symb)
     
}    
