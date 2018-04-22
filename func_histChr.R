# Function to plot histograms of BP positions of probes with expression correlated with COMT

library(ggplot2,ggthemes)
library(ggExtra)
library(psych)
library(ggpubr)       ### Make sure this is installed
library(knitr)
library(Hmisc)
library(openxlsx)
library(xtable)
library(magrittr)
library(tables)
library(stargazer)
library(plyr)
library(rlist)
library(qqman)
library(manhattanly)
library(Cairo)
library(RColorBrewer)
library(HGNChelper)
library(tools)
library(scales)

# RColorBrewer palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


hist.by.Chr <- function(onlyChr,numChr,BPref,gname,nbins,binw) {
  
# Need to get maximum count
  histdata <- hist(onlyChr$BP, nbins, plot=FALSE, right=FALSE)
  max.count <- 1.2*max(histdata$counts)
  
histBP <- ggplot(onlyChr, aes(BP)) +
  geom_histogram(binwidth = binw,
                  color=cbPalette[6],
                 fill=cbPalette[6]) +
  scale_x_continuous(name='Base Pair Position (Mb)',
#                     limits=c((min(onlyChr$BP)-10),(max(onlyChr$BP)+10))) +
                      limits=c(0,75)) +
 #                    breaks = pretty_breaks(n = 8)) +
  scale_y_continuous(name='Count',
#                     limits=c(0,20),
                     limits=c(0,max.count),
                     breaks = pretty_breaks(n = 5)) +
  #    scale_fill_brewer(palette="Set3") +
  ggtitle(paste('Base Pair Positions of Probes on Chromosome ',
                as.character(numChr),sep='')) + 
  #    theme_minimal() + 
  theme_classic() + 
  theme(aspect.ratio=0.618) +
  theme(axis.line = element_line(colour = "black",size=0.25)) +
  theme(plot.title = element_text(color="black",
                                  face="bold",
                                  size=10,
                                  hjust=0)) +
  theme(axis.title = element_text(color="black",
                                  size=10,
                                  margin = margin(t = 4, r = 4, b = 0, l = 0)
  )) +
  theme(axis.text.x = element_text(size=10,
                                   margin = margin(t = 4, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(size=10,
                                   margin = margin(t = 0, r = 2, b = 0, l = 0))) +
  geom_vline(xintercept = BPref,
             col='red',
             size = 0.5) +
  annotate("text", x = BPref + 3.0,
           y = 0.8*max.count,
           label = gname,
           color="red",
           size=3)

    return(histBP)
}