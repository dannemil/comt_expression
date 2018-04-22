# Raincloud plots

library(readr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(plyr)
library(RColorBrewer)
library(reshape2)


get.raincloud.Plot <- function(in.data)   {

source("geom_flat_violin.R")


raincloud_theme = theme(
     text = element_text(size = 10),
     axis.title.x = element_text(size = 11),
     axis.title.y = element_text(size = 11),
     axis.text = element_text(size = 9),
     axis.text.x = element_text(angle = 0, vjust = 0.0),
     legend.title=element_text(size=16),
     legend.text=element_text(size=16),
     legend.position = "right",
     plot.title = element_text(lineheight=.8, face="bold", size = 12,hjust=0.5),
     panel.border = element_blank(),
     panel.grid.minor = element_blank(),
     panel.grid.major = element_blank(),
     axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
     axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

lb <- function(x) mean(x) - 3*sd(x)
ub <- function(x) mean(x) + 3*sd(x)

colNames.stacked.area <- colnames(in.data)
colnames(in.data)[which(colNames.stacked.area == 'Mean')] <- c('MeanExpression')

# sumld<- ddply(in.data, ~brainArea, summarise, mean = mean(MeanExpression), 
#               median = median(MeanExpression), lower = lb(MeanExpression), 
#               upper = ub(MeanExpression))

gplot <- ggplot(data = in.data, aes(y = MeanExpression, x = brainArea, fill = brainArea)) +
     geom_flat_violin(position = position_nudge(x = .2, y = .3), alpha = .8) +
     geom_point(aes(y = MeanExpression, color = brainArea), position = position_jitter(width = .10), 
                size = .02, alpha = c(.8)) +
     geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.8,
                  color=c(rep('black',4))) +
#     expand_limits(x = 5.25) +
     scale_y_continuous(name=c('Mean Expression')) +
     
     scale_x_discrete(name=c('Brain Area')) +
     ggtitle('Mean Expression Level per Gene by Brain Area' ) +
     expand_limits(y = 6.0) +
     guides(fill = FALSE) +
     guides(color = FALSE) +
     scale_color_brewer(palette = "Spectral") +
     scale_fill_brewer(palette = "Spectral") +
     coord_flip() +
     theme_bw() +
     raincloud_theme

return(gplot)

}