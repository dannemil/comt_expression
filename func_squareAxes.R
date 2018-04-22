### Function to plot on equal-SD scaled square axes.
######################### Begin square.Axes function ###################################
square.Axes <- function(xvar,yvar,title.Text,xlabel.Text,ylabel.Text,corval,subnum,prob,ptcol) {
  
  options(scipen = -3, digits = 1, width = 60)
  
  dat <- data.frame(x = c(xvar), y = c(yvar))
  
  if (prob < 0.001) {
    p.out <- c(' < 0.001')
  } else {
    p.out <- toString(prob)
  }
  
  range.Lims.x <- c(mean(xvar,na.rm=TRUE) - 4*sd(xvar,na.rm=TRUE), mean(xvar,na.rm=TRUE) + 4*sd(xvar,na.rm=TRUE))
  range.Lims.y <- c(mean(yvar,na.rm=TRUE) - 4*sd(yvar,na.rm=TRUE), mean(yvar,na.rm=TRUE) + 4*sd(yvar,na.rm=TRUE))
  
  
  
  sq.Plt <-   ggplot(dat, aes(x=xvar,y=yvar)) +
    geom_point(color = "blue",size=0.5) + 
    geom_smooth(method=lm,   # Add linear regression line
                se=FALSE,
                size = 0.75)  +  # Don't add shaded confidence region
    #            theme(aspect.ratio=1) +
    # This adds  the little lines to each axis that represent the densities of the variables
    geom_rug() +    
    geom_abline(intercept = mean(yvar,na.rm=TRUE)  + (-sign(corval))*(mean(xvar,na.rm=TRUE)/sd(xvar,na.rm=TRUE))*sd(yvar,na.rm=TRUE),
                slope = sign(corval)*(sd(yvar,na.rm=TRUE)/sd(xvar,na.rm=TRUE)),
                linetype='dashed',
                size = 1) + 
    scale_y_continuous(name=ylabel.Text,
                       limits=range.Lims.y) +
    scale_x_continuous(name=xlabel.Text,
                       limits=range.Lims.x) +
    ggtitle(title.Text) + 
    #    theme_minimal() + 
    theme_classic() + 
    theme(aspect.ratio=1) +
    theme(axis.line = element_line(colour = "grey80",size=1.0),
          panel.border = element_rect(colour = "grey80", fill=NA,
                                      size=2.5)) +
    theme(plot.title = element_text(color="black",
                                    size=12,
                                    hjust=0.5)) +
    theme(axis.title = element_text(color="black",
                                    size=12)) +
    theme(axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10)) +
    geom_vline(xintercept = mean(xvar,na.rm=TRUE),
               col='red',
               size = 1) +
    geom_hline(yintercept = mean(yvar,na.rm=TRUE),
               col='red',
               size = 1) +
    annotate("text", x = mean(xvar,na.rm=TRUE)+1.0*sd(xvar,na.rm=TRUE),
             y = mean(yvar,na.rm=TRUE)+3.8*sd(yvar,na.rm=TRUE),
             label = c(paste('r = ',toString(corval),sep='')),
             color="black",
             size=3,
             hjust = c(0)) +
    annotate("text", x = mean(xvar,na.rm=TRUE)+1.0*sd(xvar,na.rm=TRUE),
             y = mean(yvar,na.rm=TRUE) + 3.5*sd(yvar,na.rm=TRUE),
             label = c(paste('p = ',p.out,sep='')),
             color="black",
             size=3,
             hjust = c(0)) +
    annotate("text", x = mean(xvar,na.rm=TRUE) -2.8*sd(xvar,na.rm=TRUE),
             y = mean(yvar,na.rm=TRUE)+3.8*sd(yvar,na.rm=TRUE),
             label = c(paste('n = ',toString(subnum),sep='')),
             color="black",
             size=3)  
    
  
  return(sq.Plt) # This is what gets returned by the function. In this case it is an actual plot.
}                # <<<< This is actually the last stement in the function.