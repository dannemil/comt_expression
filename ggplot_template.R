#### template for ggplot

geom_point() +
     facet_wrap(~Subset, ncol = 1) +
     scale_x_continuous(breaks=c(-200,-150,-100,-50,1,50,100,150,200),
                        labels=c('-200','-150','-100','-50','+1','+50','+100','+150','+200')) +
     scale_y_continuous(breaks=c(0,0.05,0.10,0.15,0.20,0.25),
                        limits=c(0,0.25)) +
     labs(title="Proportion of CA Dinucleotide by Position Relative to TSS",x="\n Position of \"A\" Nucleotide in CA Dinucleotide", y = "Proportion CA",subtitle='Averaged Over Brain Areas') +
     #     geom_rangeframe(color='black') +
     theme_bw() + scale_colour_ptol() +
     theme(plot.title = element_text(hjust = 0.5)) +
     theme(plot.subtitle = element_text(hjust = 0.5)) +
     geom_vline(aes(xintercept = 1),linetype='dotted',size=0.4) +
     geom_hline(aes(yintercept=c(0.0625)),size=0.4) +
     geom_hline(aes(yintercept=c(0.0)),linetype='dotted',size=0.2) +
     geom_text(data = ann_text,
               mapping = aes(x = c(1,4,1,4), y = c(0.175,0.122,0.206,0.128), label = labels),
               hjust=-.2,
               size=2.5,
               fontface='bold') +
     annotate("text", x = c(-120), y = c(0.22), 
              label = c('The horizontal line with a y-intercept of 0.0625 is the expected proportion \n of CA dinucleotides under the assumption of random distribution.'),
              size = 1.5,
              color='black')
