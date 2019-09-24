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
library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Set working directory
setwd("~/Documents/git/chillfreeze/analyses")

source('source/stan_utility.R')

#chill.stan <- read.csv("output/clean_dvr_60dayoutput.csv", header=TRUE)
#chill.stan <- read.csv("output/fakedata_height.csv", header=TRUE)
chill.stan <- read.csv("output/clean_dvr_traits.csv")

chill.stan <- chill.stan[!is.na(chill.stan$ht.diff),]

#chill.stan$ht.diff <- chill.stan$X60dayheight - chill.stan$lo.ht
#chill.stan <- chill.stan[!is.na(chill.stan$ht.diff),]
#chill.stan$ht.rgr <- (log(chill.stan$X60dayheight) - log(chill.stan$lo.ht)) * 10
#chill.stan <- chill.stan[!is.na(chill.stan$ht.rgr),]

#chill.stan$thickness <- ((chill.stan$thick1 + chill.stan$thick2)/2)*10
#chill.stan <- chill.stan[!is.na(chill.stan$thickness),]


#chill.stan <- chill.stan[!is.na(chill.stan$mg.cm2),]
#chill.stan$mg.cm2 <- chill.stan$mg.cm2*100
#chill.stan$chlavg <- as.numeric(chill.stan$chlavg)

#chill.stan <- chill.stan[!is.na(chill.stan$chlavg),]

#chill.stan <- chill.stan[!is.na(chill.stan$ht.rgr),]

#chill.stan <- chill.stan[!is.na(chill.stan$tough),]

#chill.stan <- chill.stan[!is.na(chill.stan$gslength),]
#chill.stan$roottoshoot <- chill.stan$roots/chill.stan$shoots
#chill.stan <- chill.stan[!is.na(chill.stan$roottoshoot),]
#chill.stan$ht.rgr <- (log(chill.stan$X60dayheight) - log(chill.stan$lo.ht)) * 10

#toughness.mod <- brm(tough ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species), 
 #                    data=chill.stan, iter=4000, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99))

#ht.rgr.new <- brm(rgr_prebudset ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2|species), data=chill.stan,
 #                 iter=4000, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99))

#chlavg.mod <- brm(chlavg ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2|species), data=chill.stan,
 #                                  iter=4000, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99))

#thickness.mod <- brm(thickness ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2|species), data=chill.stan,
#                  iter=4000, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99))

#save(thickness.mod, file="~/Documents/git/chillfreeze/analyses/stan/thickness_brms.Rdata")

htmid.mod <- brm(ht.diff ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2|species), data=chill.stan,
               iter=4000, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99))

save(htmid.mod, file="~/Documents/git/chillfreeze/analyses/stan/htmid_brms.Rdata")

#dvr.mod <- brm(dvr ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2|species), data=chill.stan,
 #                 iter=4000, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99))

#save(dvr.mod, file="~/Documents/git/chillfreeze/analyses/stan/dvr_brms.Rdata")

if(FALSE){
datalist.chill <- with(chill.stan, 
                       list(y = tough, 
                            tx = tx, 
                            chill1 = chill1, 
                            chill2 = chill2,
                            sp = as.numeric(as.factor(species)),
                            N = nrow(chill.stan),
                            n_sp = length(unique(chill.stan$species))
                       )
)

tough.inter = stan('stan/toughness_2level_normal.stan', data = datalist.chill,
                              iter = 4000, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99)) ###
save(toughness.mod, file="~/Documents/git/chillfreeze/analyses/stan/toughness_brms.Rdata")

#chl.inter.normal = stan('stan/zarchive/chl_2level_normal.stan', data = datalist.chill,
                           #iter = 5000, warmup=3000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ###

#thickness.chill2 = stan('stan/zarchive/thickness_2level.stan', data = datalist.chill,
 #                       iter = 4500, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99)) ###

check_all_diagnostics(tough.inter)


y <- as.vector(chill.stan$ht.rgr)
yrep <- rstan::extract(htrgr.inter.normal)
yrep <- yrep$yhat
ppc <- ppc_stat(y, yrep)
ppc.max <- ppc_stat(y, yrep, stat = "max")
ppc.min <- ppc_stat(y, yrep, stat = "min")
ppc.sd <- ppc_stat(y, yrep, stat = "sd")

#
quartz()
grid.arrange(ppc, ppc.sd, ppc.max, ppc.min, ncol=2, nrow=2)

save(dvr.inter.ncp.skew, file="stan/ht_inter_ncp_skewnormal.Rda")
  
}  

  
  
  
  