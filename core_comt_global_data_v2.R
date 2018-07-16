################################################################################
# June 13, 2018
#
# This program loads the R libraries and the Bioconductor libraries and
# reads in the data files necessary to do the various types of analyses
# of the COMT project.
#
# The data are declared as global variables so that other programs can access
# that data without having to read it in again.
#
# This program also defines constants and variables globally.
#
# It does not do any actual data analysis.
################################################################################

###################################################################
###
### chunk.Status determines whether the code in a particular chunk shpuld be executed or not.
### It mostly applies to chunks generating tables and figures that have already been generated.
### In some cases, it is not used. Instead, a file.exists is used to check on a file that
### is saved out by that chunk. If the file exists, then the chunk is not executed.
###
############################################@#%%%%%%%%%%%%%%%%%%%%%
chunk.Status <- FALSE

library(SOAR)
library(ggplot2,ggthemes)
library(ggExtra)
library(psych)
library(knitr)
library(Hmisc)
library(openxlsx)
#library(xlsx)
library(xtable)
library(magrittr)
library(tables)
library(plyr)
library(rlist)
# library(qqman)
# library(manhattanly)
library(Cairo)
library(RColorBrewer)
library(HGNChelper)
library(tools)
library(scales)
library(devtools)
library(utils)
# library(ggman)
library(dplyr)
library(biomaRt)
library(combinat)
library(Rmpfr)
library(stats)
library(stringr)
library(mvtnorm)
library(miscFuncs)
library(reporttools)
library(glm2)
library(reshape2)
library(plotly)
library(gmp)
library(R.utils)
library(curl)
library(igraph)
library(data.tree)
library(gmodels)
library(digest)
library(seqinr)
library(tidyverse)
library(HDMD)
library(seqLogo)
library(PropCIs)
library(data.table)
library(GlobalOptions)
library(arrayhelpers)
library(ggseqlogo)
library(vcd)
library(matrixStats)
library(rJava)
library(corrplot)
library(survC1)
library(ontologyIndex)
library(ontologyPlot)
library(ggalt)
library("factoextra")
library(cluster)
library(hybridHclust)
library(progress)
library(tcltk)
library(ggcorrplot)
library(compHclust)
library(dendextend)
library(mvmesh)
library(extrafont)
library(extrafontdb)
library(ggrepel)
library(grid)
library(gridExtra)
library(gtable)
library(DescTools)
library(rcompanion)

setwd('/Volumes/Macintosh_HD_3/genetics/genenetwork2/')
work.path <- c('/Volumes/Macintosh_HD_3/genetics/genenetwork2/')

# new plots directory
new_plots <- c('/Volumes/Macintosh_HD_3/genetics/genenetwork2/new_plots/')

# RColorBrewer palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

options(scipen = 999, digits = 10, width = 60, knitr.table.format = "latex")
opts_chunk$set(include=FALSE,
               echo=FALSE,
               message=FALSE,
               warning=FALSE)

area.abbrv <- c('pf',
                'cb',
                'tm',
                'po'
)
################ READ THIS LIST OF TO-BE SOURCED FUNCTIONS AND PROGRAMS IN #################

fileConn <- file('sourced_functions_programs.txt')
sourced.Funcs <- 
     data.frame(rfunc=readLines(con=fileConn),stringsAsFactors = FALSE)

temp.Funcs <- str_replace(sourced.Funcs$rfunc,',','.R')
sourced.Funcs <- data.frame(rfunc=as.character(temp.Funcs),stringsAsFactors = FALSE)
temp2.Funcs <- paste(work.path,sourced.Funcs$rfunc,sep='') 
sourced.Funcs <- data.frame(rfunc=as.character(temp2.Funcs),stringsAsFactors = FALSE)
#close(fileConn)

source(sourced.Funcs$rfunc[1:length(sourced.Funcs)])
#############################################################################################

# Read in the mapping between Chromosome names ordered by text (e.g., 1, 10, 11, 12...2, 20...) and ordered by number (e.g., 1, 2, 3, 4 ..., 22, X, Y)

wbchr <- loadWorkbook('order_Chr_by_num.xlsx')   

# now tell R to read that workbook and which sheet
order.Chr.by.num <- data.frame(read.xlsx(wbchr,sheet = "chr_map"))


stand.Col.names <- c('Prefrontal',
                     'Cerebellum',
                     'Temporal',
                     'Pons')
stand.Col.names.aug <- c(paste(stand.Col.names[1],' Cortex',sep=''),
                         paste(stand.Col.names[2],'',sep=''),
                         paste(stand.Col.names[3],' Cortex',sep=''),
                         paste(stand.Col.names[4],'',sep=''))

gold.R <- c(1.61803398875)

     
     
     # myBioCLite() # This function updates only the packages that need to be updated
     source("https://bioconductor.org/biocLite.R")

biocLite('Biostrings')
library('Biostrings')

biocLite('BSgenome')
library('BSgenome')
biocLite('BSgenome.Hsapiens.NCBI.GRCh38')
library('BSgenome.Hsapiens.NCBI.GRCh38')
hsgen <- BSgenome.Hsapiens.NCBI.GRCh38

biocLite('GenomicFeatures')
library('GenomicFeatures')

biocLite('RMariaDB')
library('RMariaDB')

biocLite('org.Bt.eg.db')
library('org.Bt.eg.db')

library(GenomicRanges)
library(BSgenome.Hsapiens.UCSC.hg19)
BSgenome.Hsapiens.UCSC.hg19
# or
Hsapiens
class(Hsapiens)

##BSgenome contains the DNA sequences

#############
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
Txdb<- TxDb.Hsapiens.UCSC.hg19.knownGene
Txdb
class(Txdb)

library(Homo.sapiens)

biocLite('enrichR')
library('enrichR')
dbs.Enrichr <- listEnrichrDbs()

biocLite('topGO')
library('topGO')

biocLite('AnnotationHub')
library('AnnotationHub')

biocLite('AnnotationFuncs')
library('AnnotationFuncs')

ahub <- AnnotationHub(hub=getAnnotationHubOption("URL"), cache=getAnnotationHubOption("CACHE"), proxy=getAnnotationHubOption("PROXY"), localHub=FALSE)
ah <- subset(ahub, species == "Homo sapiens")
# retrieve records with, e.g., 'object[["AH18522"]]' 

biocLite('GOsummaries')
library('GOsummaries')

biocLite('clusterProfiler')
library('clusterProfiler')

biocLite(c("AnnotationDbi"))
biocLite(c("org.Hs.eg.db"))

library("org.Hs.eg.db")

biocLite(c('hgu95av2.db'))
library('hgu95av2.db')

source("https://bioconductor.org/workflows.R")

workflowInstall("annotation")

biocLite("TxDb.Hsapiens.UCSC.hg19.knownGene")
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene #shorthand (for convenience)


biocLite("GO.db")
biocLite("topGO")
biocLite("GOstats")


library("GO.db")
library("topGO")
library("GOstats")

biocLite('annotate')
library('annotate')

biocLite('motifRG')
biocLite('MotifDb')
library('motifRG')
library('MotifDb')

biocLite('PWMEnrich')
library('PWMEnrich')
biocLite('PWMEnrich.Hsapiens.background')
library('PWMEnrich.Hsapiens.background')

biocLite('RcisTarget')
library('RcisTarget')

biocLite("RDAVIDWebService")
library("RDAVIDWebService")

biocLite('GOSemSim')
library('GOSemSim')

biocLite('RamiGO')
library('RamiGO')



coexpress.URL <- data.frame(site=NA,addr=NA)
coexpress.URL[1,1:2] <- c('COXPRESdb','http://coxpresdb.jp/')
coexpress.URL[2,1:2] <- c('OMICtools','https://omictools.com/')
coexpress.URL[3,1:2] <- c('Coexpedia','http://www.coexpedia.org/search.php')
coexpress.URL[4,1:2] <- c('GeneFriends','http://www.genefriends.org/RNAseq/')
coexpress.URL[5,1:2] <- c('Illumina Probes','http://www.genomequebec.mcgill.ca/compgen/integratedvervetgenomics/transcriptome/Illumina/allprobes.html')
coexpress.URL[6,1:2] <- c('Gibbs Expression Data','https://www.ncbi.nlm.nih.gov/geo/query')
coexpress.URL[7,1:2] <- c('Train Online','https://www.ebi.ac.uk/training/online/course/arrayexpressdiscoverfunctionalgenomicsdataqui/references')
coexpress.URL[8,1:2] <- c('Enrichr','http://amp.pharm.mssm.edu/Enrichr/enrich')

url.Tab <- xtable(coexpress.URL)
url.Table <- latex(url.Tab,file='',
                   caption = c("Gene Coexpression Databases"),
                   caption.loc = c('top'),
                   colname = c('Name','URL'),
                   rowlabel = c(''),
                   colnamesTexCmd="bfseries",
                   booktabs=TRUE,
                   label=c('tabcoexpressurls'),
                   na.blank = TRUE,
                   vbar = FALSE,
                   longtable=TRUE,
                   table.env=TRUE,
                   center=c('center'),
                   continued=c('Gene Coexpression Databases Continued'),
                   first.hline.double = TRUE,
                   append=FALSE
)


          entrez_object <- org.Hs.egGO    
     
     ensembl <- useMart("ENSEMBL_MART_ENSEMBL", dataset = "hsapiens_gene_ensembl")
     #ensembl = useEnsembl(biomart="ensembl",dataset = "hsapiens_gene_ensembl")
     
     normal.chroms <- c(1:22, "X", "Y", "M")
     
     ah <- AnnotationHub()
     
     
     
     orgs <- AnnotationHub::query(ah, "org.Hs.eg.db")
     
     
     # columns(gene.DB)
     # keytypes(gene.DB)
     

          
          
          ### read the data file 
          
          fname <- c('comt_correlations_20000_all_areas_augmentedNew.xlsx')
     
     if (file.exists(fname))   {
          wball <- loadWorkbook(fname)   
          
          # now tell R to read that workbook and which sheet
          prefront.Data <- data.frame(read.xlsx(wball,sheet = "prefrontal"))
          
          cbell.Data <- data.frame(read.xlsx(wball,sheet = "cerebellum"))
          
          tempor.Data <- data.frame(read.xlsx(wball,sheet = "temporal"))
          
          pons.Data <- data.frame(read.xlsx(wball,sheet = "pons"))
          
          Store(prefront.Data,
                cbell.Data,
                tempor.Data,
                pons.Data)
          
          
          # create a list with these data frames to be used when the same operation is applied to all area
          
          major.Area <- list(pf=prefront.Data,
                             cb=cbell.Data,
                             tm=tempor.Data,
                             po=pons.Data)
          
     } else {
          print(paste('The data file ',fname,' does not exist',sep=''))
     }
     
     ################# Top 500 by area strongly COMT correlated genes (pval ranked) #################
     ### read the data file 
     
     
     fname <- c('top_500_pval_ranked.xlsx')
     
     if (file.exists(fname))   {
          wbtop <- loadWorkbook(fname) 
          
          # now tell R to read that workbook and which sheet
          top500.prefront.Data <- data.frame(read.xlsx(wbtop,sheet = "prefrontal"))
          
          top500.cbell.Data <- data.frame(read.xlsx(wbtop,sheet = "cerebellum"))
          
          top500.tempor.Data <- data.frame(read.xlsx(wbtop,sheet = "temporal"))
          
          top500.pons.Data <- data.frame(read.xlsx(wbtop,sheet = "pons"))
          
          Store(top500.prefront.Data,
                top500.cbell.Data,
                top500.tempor.Data,
                top500.pons.Data)
          
          # create a list with these data frames to be used when the same operation is applied to all area
          
          hold.Lowest.pvals <- list(pf=top500.prefront.Data,
                                    cb=top500.cbell.Data,
                                    tm=top500.tempor.Data,
                                    po=top500.pons.Data)
          
          hold.Lowest.pvals.pos <- list(pf=top500.prefront.Data[top500.prefront.Data$corsign == 1,],
                                        cb=top500.cbell.Data[top500.cbell.Data$corsign == 1,],
                                        tm=top500.tempor.Data[top500.tempor.Data$corsign == 1,],
                                        po=top500.pons.Data[top500.pons.Data$corsign == 1,])
          
          hold.Lowest.pvals.neg <- list(pf=top500.prefront.Data[top500.prefront.Data$corsign == -1,],
                                        cb=top500.cbell.Data[top500.cbell.Data$corsign == -1,],
                                        tm=top500.tempor.Data[top500.tempor.Data$corsign == -1,],
                                        po=top500.pons.Data[top500.pons.Data$corsign == -1,])
          
          hold.Lowest.pvals.sign <- list(pos=hold.Lowest.pvals.pos,
                                         neg=hold.Lowest.pvals.neg)
     } else {
          print(paste('The data file ',fname,' does not exist',sep=''))
     }
     
     
     
     ################################################################################################
     
     ################# Unique by area strongly COMT correlated genes ######################
     
     fname <- c('unique_genes_full_data.xlsx')
     
     if (file.exists(fname))   {
          wbuniq <- loadWorkbook(fname) 
          
          # now tell R to read that workbook and which sheet
          unique.prefront.Data <- data.frame(read.xlsx(wbuniq,sheet = "prefrontal"))
          
          unique.cbell.Data <- data.frame(read.xlsx(wbuniq,sheet = "cerebellum"))
          
          unique.tempor.Data <- data.frame(read.xlsx(wbuniq,sheet = "temporal"))
          
          unique.pons.Data <- data.frame(read.xlsx(wbuniq,sheet = "pons"))
          
          Store(unique.prefront.Data,
                unique.cbell.Data,
                unique.tempor.Data,
                unique.pons.Data)
          
          
          # create a list with these data frames to be used when the same operation is applied to all area
          
          unique.full.data.all <- list(pf=unique.prefront.Data,
                                       cb=unique.cbell.Data,
                                       tm=unique.tempor.Data,
                                       po=unique.pons.Data)
     } else {
          print(paste('The data file ',fname,' does not exist',sep=''))
     }
     
     
     # read in data that has only one probe per gene
     
     fname <- c('gene_as_unit_data.xlsx')
     
     if (file.exists(fname))   {
          wbgene <- loadWorkbook(fname)  
          
          # now tell R to read that workbook and which sheet
          gene.prefront.Data <- data.frame(read.xlsx(wbgene,sheet = "prefrontal"))
          
          gene.cbell.Data <- data.frame(read.xlsx(wbgene,sheet = "cerebellum"))
          
          gene.tempor.Data <- data.frame(read.xlsx(wbgene,sheet = "temporal"))
          
          gene.pons.Data <- data.frame(read.xlsx(wbgene,sheet = "pons"))
          
          # reorder data in each area by pval
          
          gene.prefront.Data <- gene.prefront.Data[order(gene.prefront.Data$Sample.p.r.,decreasing=FALSE), ]
          gene.cbell.Data <- gene.cbell.Data[order(gene.cbell.Data$Sample.p.r.,decreasing=FALSE), ]
          gene.tempor.Data <- gene.tempor.Data[order(gene.tempor.Data$Sample.p.r.,decreasing=FALSE), ]
          gene.pons.Data <- gene.pons.Data[order(gene.pons.Data$Sample.p.r.,decreasing=FALSE), ]
          
          Store(gene.prefront.Data,
                gene.cbell.Data,
                gene.tempor.Data,
                gene.pons.Data)
          
          
          # create a list with these data frames to be used when the same operation is applied to all area
          
          gene.Major <- list(pf=gene.prefront.Data,
                             cb=gene.cbell.Data,
                             tm=gene.tempor.Data,
                             po=gene.pons.Data)
     } else {
          print(paste('The data file ',fname,' does not exist',sep=''))
     }
