# File to get the missing, LOC, Un and 1.00 stats on the unprocessed COMT data

library(knitr)
library(openxlsx)
library(xtable)
#library(magrittr)
library(tables)
library(plyr)
library(scales)
library(utils)
# library(ggman)
library(dplyr)
library(biomaRt)
library(stringr)
library(curl)
library(git2r)
library(githubinstall)
library(devtools)
library('xml2')
library(RCurl)
library(XML)
library(rentrez)
library(data.table)
library(compare)

source('unique_id_generator.R')

fname <- c("/Volumes/Macintosh_HD_3/genetics/genenetwork2/original_unprocessed_data_stats.Rnw")

prog.Name <- unique.ID(fname)

prog.Name


     wballraw <- loadWorkbook('comt_all_areas_correlation_results.xlsx')   
     
     # now tell R to read that workbook and which sheet
     prefront.raw.Data <- data.frame(read.xlsx(wballraw,sheet = "prefrontal"))
     
     cbell.raw.Data <- data.frame(read.xlsx(wballraw,sheet = "cerebellum"))
     
     tempor.raw.Data <- data.frame(read.xlsx(wballraw,sheet = "temporal"))
     
     pons.raw.Data <- data.frame(read.xlsx(wballraw,sheet = "pons"))
     
     
     # create a list with these data frames to be used when the same operation is applied to all area
     
     major.raw.Area <- list(pf=prefront.raw.Data,
                        cb=cbell.raw.Data,
                        tm=tempor.raw.Data,
                        po=pons.raw.Data)
     
     ih <- c(1:4)
     
     missing.raw.and.LOC.report <- data.frame(symbol=rep(NA,4),
                                          Chr.name=rep(NA,4),
                                          start.pos= rep(NA,4),
                                          symb.LOC=rep(NA,4),
                                          Un.chr=rep(NA,4),
                                          bp1=rep(NA,4))
     
     missing.raw.and.LOC.report$symbol <- matrix(unlist(lapply(major.raw.Area[ih], function(x,ih) num.NA(x$Symbol), ih)), ncol=1)
     
     missing.raw.and.LOC.report$Chr.name <- matrix(unlist(lapply(major.raw.Area[ih], function(x,ih) num.NA(x$Location), ih)), ncol=1)
     
     missing.raw.and.LOC.report$start.pos <- matrix(unlist(lapply(major.raw.Area[ih], function(x,ih) num.NA(x$Location), ih)), ncol=1)
     
     missing.raw.and.LOC.report$symb.LOC <- matrix(unlist(lapply(major.raw.Area[ih], function(x,ih) sum(grepl('^LOC',x$Symbol)), ih)), ncol=1)
     
     missing.raw.and.LOC.report$Un.chr <- matrix(unlist(lapply(major.raw.Area[ih], function(x,ih) sum(grepl('^ChrUn',x$Location)), ih)), ncol=1)
     
     missing.raw.and.LOC.report$bp1 <- matrix(unlist(lapply(major.raw.Area[ih], function(x,ih) sum(grepl('1.000000',x$Location)), ih)), ncol=1)
     
     
     missing.raw.and.LOC.report.tab <- xtable( missing.raw.and.LOC.report,
                                               caption='The first three columns show the number of NA or missing values. \n The second three columns show gene symbols that begin with LOC, \n the number of Chromosomes named Un, and the number of starting locations labeled as 1.000000.')
     



