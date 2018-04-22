
hyper.plot <- function(prob.50,prob.200,loCut,hiCut,limseq,callingID)  {
     
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
     library(devtools)
     library(dplyr)
     
     # For plotting purposes, the last probability in each frame is 0, so leave that out
     # also because it is a discrete distribution, only plot every 5th or 10th point
     
     # Using <<- instead of <- allows the variable to used globally
     # Do this when prob.N.200 is first set up in another program
     
     # For plotting purposes, the last probability in each frame is 0, so leave that out
     # also because it is a discrete distribution, only plot every 5th or 10th point
     prob.N.200.plot <- prob.200[c(seq(1,hiCut,10)),]
     prob.N.50.plot <- prob.50[c(seq(1,loCut,5)),]
     
     brk <- limseq
     gold.R <- c(1.61803398875)
     my_title <- expression(paste("Chance Probability that k of the 50 or 200 Most Strongly ", italic("COMT-"),'Correlated',sep=''))
     subt <- c("Probes in a Reference Brain Area Will Match in Another Brain Areas")
     annot1 <- c('Prefrontal~Cortex~italic(cf)~Temporal~Cortex')
     annot2 <- as.character((expression(paste(italic("N"),' = 20,000 probes',sep=''))))
     annot3 <- as.character((expression(paste(italic("n"),' = 138 subjects',sep=''))))
     
     hyp.plot <- ggplot() +
          
          geom_point(data=prob.N.50.plot, aes(x=n.match,y=prob,group=1),
                     size=0.6,color='blue',shape=1) +
          
          # next two for top 200
          geom_point(data=prob.N.200.plot,aes(x=n.match,y=prob,group=1),
                     size=0.6,color='red',shape=6) +
          
          scale_y_continuous(breaks = brk, labels = comma(brk, digits = 1)) +
          
          scale_x_continuous(name='Number of Probes in Common (k)',
                             limits=c(0,hiCut),
                             breaks=seq(0,hiCut,25)) +
          
          theme_classic() +
          
          
          ggtitle(my_title) +
          
          ylab(expression(log[10](Probability))) + 
          theme(aspect.ratio=1/gold.R) + 
          theme(plot.title = element_text(color="black",
                                          size=8.5,
                                          hjust=0.5)) +
          theme(axis.title = element_text(color="black",
                                          size=10,
                                          vjust=1
          )) +
          theme(axis.text.x = element_text(size=8),
                axis.text.y = element_text(size=8)) +
          
          annotate("text", x = 110,
                   y = 10,
                   label = subt,
                   color="black",
                   size=3.0)  +
          annotate("text", x = 150,
                   y = -100,
                   label = annot2,
                   parse=TRUE,
                   color="black",
                   size=3.0)  +
          annotate("text", x = 148,
                   y = -135,
                   label = annot3,
                   parse=TRUE,
                   color="black",
                   size=3.0)    +
          
          annotate("text", x = 55,
                   y = -150,
                   label = c('Strongest 50'),
                   parse=FALSE,
                   color="blue",
                   size=3.0)   +
          
          annotate("text", x = 170,
                   y = -450,
                   label = c('Strongest 200'),
                   parse=FALSE,
                   color="red",
                   size=3.0)    +
          annotate("text", x = 27,
                   y = -470,
                   label = c('Every 5th (red) or 10th (blue) point plotted'),
                   parse=FALSE,
                   color="black",
                   size=2.0)   + 
          
# add the unique ID of the program that called this plotting function to the plot.
          
          annotate("text", x = 0.95*hiCut,
                   y = 0,
                   label = c(paste('unique ID: ',callingID,sep='')),
                   parse=FALSE,
                   color="gray50",
                   size=0.75) 
          
     
     return(hyp.plot)
     
}