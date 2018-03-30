# Function to return chi-square test results from the distribution of genes over chromosomes

chisq.genes.By.chr <- function(area.Data,input.Data.tab,mapIndx,lim1,lim2)   {
     
     
     ############# DEBUG
     # area.Data <- prefront.Data.onegene
     # input.Data.tab <- genes.By.chr.tab
     # mapIndx <- chrname2num
     # lim1 <- pos.neg.Lims[b,1]
     # lim2 <- pos.neg.Lims[b,2]
     
     
     ############# DEBUG
     
     doY <- any.Y(area.Data,lim1,lim2)
     

     genes.By.chr <- table(area.Data$loc)[c(mapIndx[1:24])]
     genes.By.chr.percent <- genes.By.chr[1:doY]/sum(genes.By.chr[1:doY])
#     genes.By.chr.percent.noY <- genes.By.chr[1:23]/sum(genes.By.chr[1:23])     
     
          
     chsq.genes.byChr <- chisq.test(input.Data.tab$Frequency, y = NULL, correct = FALSE,
                                       p = genes.By.chr.percent, rescale.p = FALSE,
                                       simulate.p.value = FALSE, B = 2000)
     
     
     return(chsq.genes.byChr)
     
}