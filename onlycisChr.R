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


cis.by.Chr <- function(onlyChrcis,numChr,BPref,gname) {

cisBPpos <- ggplot(onlyChrcis, aes(x=dist,y=pval)) +
  geom_point(color=cbPalette[6],
             size=0.75) +
  scale_x_continuous(name=paste('Base Pair Position Relative to ',gname,' (Mb)',sep=''),
                     limits=c((min(onlyChrcis$dist)-5),(max(onlyChrcis$dist)+5)),
                     breaks = pretty_breaks(n = 8)) +
  scale_y_continuous(name='-log10(P-value)') +
  # limits=c(0,15),
  # breaks = pretty_breaks(n = 5)) +
  #    scale_fill_brewer(palette="Set3") +
  ggtitle(paste('Base Pair Positions of Positively-Correlated Probes on Chromosome ',numChr,' \n Relative to ',gname,' vs. -log10(P-value)',sep='')) + 
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
                                  margin = margin(t = 0, r = 4, b = 4, l = 0)
  )) +
  theme(axis.text.x = element_text(size=10,
                                   margin = margin(t = 4, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(size=10,
                                   margin = margin(t = 0, r = 2, b = 0, l = 0))) +
  geom_vline(xintercept = c(0),
             col='red',
             size = 0.5) +
  annotate("text", x = c(3.0),
           y = 0.90*max(onlyChrcis$pval),
           label = c(gname),
           color="red",
           size=3) +
  annotate("text", x = c(-2.0),
           y = -1,
           label = c("5\'"),
           color='black',
           size=3) +
  annotate("text", x = c(2.0),
           y = -1,
           label = c("3\'"),
           color='black',
           size=3)

           return(cisBPpos)
}
