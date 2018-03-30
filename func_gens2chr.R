# Function to produce table of the distribution of the top 200 +/- correlated genes across
# chromosomes

genesBychrom <- function(in.Data,mapIndx)  {
     
     topPosgenes.By.chr <- table(prefront.Data.onegene$loc[1:201])[c(chrname2num[1:23])]
     topPosgenes.By.chr.percent <- topPosgenes.By.chr/sum(topPosgenes.By.chr)
     
     genes.By.chr <- table(prefront.Data.onegene$loc)[c(chrname2num[1:24])]
     genes.By.chr.percent <- genes.By.chr/sum(genes.By.chr)
     genes.By.chr.percent.noY <- genes.By.chr[1:23]/sum(genes.By.chr[1:23])
     
     binom.SD <- sqrt(200*genes.By.chr.percent.noY*(1-genes.By.chr.percent.noY))
     
     topPosgenes.By.chr.enrich <- round(topPosgenes.By.chr.percent/genes.By.chr.percent.noY,2)
     
     topPosgenes.By.chr.pretab <- data.frame(name=map.Chrname.Chrnum$chrnum[1:23],
                                             nums=topPosgenes.By.chr,
                                             enrich=topPosgenes.By.chr.enrich)
     
     topPosgenes.By.chr.pretabPF <- topPosgenes.By.chr.pretab[,c(1,3,5)]
     
     topPosgenes.By.chr.pretabPF$expect <- round(200*genes.By.chr.percent.noY,1)
     topPosgenes.By.chr.pretabPF <- topPosgenes.By.chr.pretabPF[,c(1,2,4,3)]
     
     binom.zval <- (topPosgenes.By.chr.pretabPF$nums.Freq - topPosgenes.By.chr.pretabPF$expect)/binom.SD[1:23]
     
     binom.Pval <- round(2*pnorm(abs(binom.zval), mean=0, sd=1.0,lower.tail=FALSE),2)
     
     topPosgenes.By.chr.pretabPF$pval <- binom.Pval
     
     colnames(topPosgenes.By.chr.pretabPF) <- c('Chromosome',
                                                'Frequency',
                                                'Expected',
                                                'Enrichment',
                                                'P value')
     
     
     chsq.genes.byChr.PF <- chisq.test(topPosgenes.By.chr.pretabPF$Frequency, y = NULL, correct = FALSE,
                                       p = genes.By.chr.percent.noY, rescale.p = FALSE,
                                       simulate.p.value = FALSE, B = 2000)
     
     
     topgene.By.chr.pf.tab <- xtable(topPosgenes.By.chr.pretabPF,
                                     caption=c(paste('Distribution of the top 200 positively ',"\\textit{COMT}",'-correlated genes by Chromosome',sep='')))
     
     #### The Expected column in this table shows how many genes would be expected to be in the top 200 for each chromosome based on the proportion of genes across the entire sample on each chromosome. The P value is a binomial test using z = (freq - expected)/sd where sd = sqrt(200*expectedProp*(1-expectedProp)). The Bonferroni corrected pval would be 0.05/23 == 0.002. None of the deviations individually are significant.
     
     topgene.By.chr.pf.ltx <- latex(topgene.By.chr.pf.tab,file='',
                                    caption = paste0('Distribution of the top 200 positively ',"\\textit{COMT}",'-correlated genes by Chromosome: Prefrontal Cortex',sep=''),
                                    caption.loc = c('top'),
                                    #                    cgroup = col.Groups,
                                    #                   n.cgroup = c(2,2),
                                    #                     colheads = add.col.Groups,
                                    #                   extracolheads = extra.Col.names,
                                    #                     rowlabel = '\n Brain Area',
                                    colnamesTexCmd="bfseries",
                                    booktabs=TRUE,
                                    label=c('tab:top200posbychr'),
                                    na.blank = TRUE,
                                    vbar = FALSE,
                                    longtable=TRUE,
                                    table.env=TRUE,
                                    #                     center=c('center'),
                                    continued=c('Top 200 positively correlated genes by chromosome continued'),
                                    first.hline.double = FALSE,
                                    append=FALSE,
                                    insert.bottom=c(paste('**There were no genes on the Y chromosome that fell within the 200 most strongly positively ',"\\textit{COMT}",'-correlated genes in Prefrontal Cortex',sep=''))
     )
     
     
     
     
     
     
     
     
     
     
     
     
     
     
}