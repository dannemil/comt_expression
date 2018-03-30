# File to estimate the probability of getting 8 or more matching genes across two brain areas on 
# Chr 5 falling within the 200 most strongly negatively correlated genes

Npf <- 715 # Genes measured on Ch 5 PfCtx
Ntm <- 729
N.common <- 567
samp.pf <- 20
samp.tm <- 21
k <- 8
h <- 1000   # addend to make unique numbers in the tm population
g <- 1000000   # number of Monte Carlo replications

pop.common <- matrix(seq(1,N.common,1),nrow=N.common)
pop.common.ch <- as.character(pop.common)

# PfCtx
pop.pf <- as.character(seq(1,Npf,1))

# TmCtx
pop.tm.uniq <- matrix(h + seq(N.common+1,N.common+(Ntm-N.common),1),
                      nrow = Ntm - N.common)
pop.tm <- rbind(pop.common,pop.tm.uniq)
pop.tm <- as.character(pop.tm)

# length(intersect(pop.pf,pop.tm))    567

# Choose 20 and 21 randomly from respective populations and compare

num.match <- matrix(rep(0,3*g),ncol=3)

for (i in 1:g) {
     
     chosen.pf <- pop.pf[sample(1:Npf, samp.pf, replace=F)]
     
          num.match[i,1] <- length(intersect(chosen.pf,pop.common.ch))
     
     chosen.tm <- pop.tm[sample(1:Ntm, samp.tm, replace=F)]
     
          num.match[i,2] <- length(intersect(chosen.tm,pop.common.ch))
     
          num.match[i,3] <- length(intersect(chosen.pf,chosen.tm))
     
}

table(num.match[,1])
table(num.match[,2])
table(num.match[,3])/g

mean(num.match[,1])
mean(num.match[,2])
mean(num.match[,3])





# 
# hist(num.match[,2])
hist(num.match[,3])


rm(num.match)

x <- ((567*20/715) + (567*21/729))/567

lambda <- x

cover.range <- seq(0,8,1)
dist.poiss <- as.bigq(dpois(cover.range, lambda, log = FALSE))

plot(cover.range,dist.poiss,
     ylab=c('Prob density'),
     xlab=c('x'))

