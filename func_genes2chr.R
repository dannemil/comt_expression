# Function to produce table of the distribution of the top 200 +/- correlated genes across
# chromosomes

genesBychrom <- function(in.Data,mapIndx,lim1,lim2)  {
     
     doY <- any.Y(in.Data,lim1,lim2)
     
     topPosgenes.By.chr <- table(in.Data$loc[lim1:lim2])[c(mapIndx[1:doY])]
     topPosgenes.By.chr.percent <- topPosgenes.By.chr/sum(topPosgenes.By.chr)
     
     genes.By.chr <- table(in.Data$loc)[c(mapIndx[1:24])]
     genes.By.chr.percent <- genes.By.chr[1:doY]/sum(genes.By.chr[1:doY])
#     genes.By.chr.percent.noY <- genes.By.chr[1:23]/sum(genes.By.chr[1:23])
     
     binom.SD <- sqrt(200*genes.By.chr.percent*(1-genes.By.chr.percent))
     
     topPosgenes.By.chr.enrich <- round(topPosgenes.By.chr.percent/genes.By.chr.percent,2)
     
     topPosgenes.By.chr.pretab <- data.frame(name=map.Chrname.Chrnum$chrnum[1:23],
                                             nums=topPosgenes.By.chr,
                                             enrich=topPosgenes.By.chr.enrich)
     
     topPosgenes.By.chr.pretab <- topPosgenes.By.chr.pretab[,c(1,3,5)]
     
     topPosgenes.By.chr.pretab$expect <- round(200*genes.By.chr.percent,1)
     topPosgenes.By.chr.pretab <- topPosgenes.By.chr.pretab[,c(1,2,4,3)]
     
     binom.zval <- (topPosgenes.By.chr.pretab$nums.Freq - topPosgenes.By.chr.pretab$expect)/binom.SD[1:23]
     
     binom.Pval <- round(2*pnorm(abs(binom.zval), mean=0, sd=1.0,lower.tail=FALSE),2)
     
     topPosgenes.By.chr.pretab$pval <- binom.Pval
     
     colnames(topPosgenes.By.chr.pretab) <- c('Chromosome',
                                                'Frequency',
                                                'Expected',
                                                'Enrichment',
                                                'P value')
     
     
     return(topPosgenes.By.chr.pretab)
     
}