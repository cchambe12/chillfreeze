### Started 8 May 2019 - Cat
## Building stan models to assess impact of False springs on growth

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(bayesplot) ## for plotting
library(egg) ## for plotting
library(shinystan)
library(rstanarm)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Set working directory
setwd("~/Documents/git/chillfreeze/analyses")

source('source/stan_utility.R')

chill.stan <- read.csv("output/clean_dvr_60dayoutput.csv", header=TRUE)
#chill.stan <- read.csv("output/fakedata_height.csv", header=TRUE)

#chill.stan$ht.diff <- chill.stan$X60dayheight - chill.stan$lo.ht
chill.stan$ht.rgr <- (log(chill.stan$X60dayheight) - log(chill.stan$lo.ht)) * 10

#chill.stan$thickness <- ((chill.stan$thick1 + chill.stan$thick2)/2)*10
#chill.stan <- chill.stan[!is.na(chill.stan$thickness),]

#chill.stan <- chill.stan[!is.na(chill.stan$mg.cm2),]

#chill.stan <- chill.stan[!is.na(chill.stan$chlavg),]

chill.stan <- chill.stan[!is.na(chill.stan$ht.rgr),]
rmspp <- c("FAGGRA", "NYSSYL")
chill.stan <- chill.stan[!(chill.stan%in%rmspp),]


datalist.chill <- with(chill.stan, 
                       list(y = ht.rgr, 
                            tx = tx, 
                            chill1 = chill1, 
                            chill2 = chill2,
                            sp = as.numeric(as.factor(species)),
                            N = nrow(chill.stan),
                            n_sp = length(unique(chill.stan$species))
                       )
)

leaf.chill <- chill.stan[(chill.stan$chill!=3),]
datalist.leaf <- with(leaf.chill, 
                       list(y = thickness, 
                            tx = tx, 
                            chill1 = chill1,
                            sp = as.numeric(as.factor(species)),
                            N = nrow(leaf.chill),
                            n_sp = length(unique(leaf.chill$species))
                       )
)



#ht.inter.skewnormal = stan('stan/zarchive/htrgr_2level_normal.stan', data = datalist.chill,
 #                             iter = 4500, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99)) ###

ht.inter.normal = stan('stan/zarchive/htrgr_2level_normal.stan', data = datalist.chill,
                           iter = 4500, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99)) ###

thickness.chill2 = stan('stan/zarchive/thickness_2level.stan', data = datalist.leaf,
                        iter = 4500, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99)) ###

check_all_diagnostics(ht.inter.normal)


y <- as.vector(chill.stan$chlavg)
yrep <- rstan::extract(chl.inter.normal)
yrep <- yrep$yhat
ppc <- ppc_stat(y, yrep)
ppc.max <- ppc_stat(y, yrep, stat = "max")
ppc.min <- ppc_stat(y, yrep, stat = "min")
ppc.sd <- ppc_stat(y, yrep, stat = "sd")

#
quartz()
grid.arrange(ppc, ppc.sd, ppc.max, ppc.min, ncol=2, nrow=2)

save(dvr.inter.ncp.skew, file="stan/ht_inter_ncp_skewnormal.Rda")
  
  
  
  
  
  
  