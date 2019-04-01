### Started 1 April 2019 - Cat
## Building stan models to assess impact of False springs on DVR

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(bayesplot) ## for plotting
library(egg) ## for plotting
library(shinystan)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Set working directory
setwd("~/Documents/git/chillfreeze/analyses")

source('source/stan_utility.R')

chill.stan <- read.csv("output/clean_dvr_60dayoutput.csv", header=TRUE)
testdat <- read.csv("output/fakedata.csv", header=TRUE)
chill.stan <- chill.stan[!is.na(chill.stan$dvr),]
#nospp <- c("NYSSYL", "FAGGRA") # species to exclude for now because not enough data
#chill.stan <- chill.stan[!(chill.stan$species%in%nospp),]

chill.stan <- subset(chill.stan, select=c("dvr", "tx", "chill1", "chill2", "species"))

if(FALSE){
datalist.chill <- with(testdat, 
                     list(y = dvr, 
                          tx = tx, 
                          chill1 = chill1, 
                          chill2 = chill2,
                          sp = as.numeric(as.factor(species)),
                          N = nrow(testdat),
                          n_sp = length(unique(testdat$species))
                     )
)
}

datalist.chill <- with(chill.stan, 
                       list(y = dvr, 
                            tx = tx, 
                            chill1 = chill1, 
                            chill2 = chill2,
                            sp = as.numeric(as.factor(species)),
                            N = nrow(chill.stan),
                            n_sp = length(unique(chill.stan$species))
                       )
)

dvr.inter.normal = stan('stan/dvr_winter_2level.stan', data = datalist.chill,
                 iter = 2500, warmup=1500, control=list(max_treedepth = 12,adapt_delta = 0.99)) ### 32 divergent transitions
               
dvr.inter.cauchy = stan('stan/dvr_winter_2level_cauchy.stan', data = datalist.chill,
                        iter = 2500, warmup=1500, control=list(max_treedepth = 12,adapt_delta = 0.99)) ### 32 divergent transitions
      
dvr.inter.ncp = stan('stan/dvr_winter_2level_ncp.stan', data = datalist.chill,
                                      iter = 2500, warmup=1500, control=list(max_treedepth = 12,adapt_delta = 0.99)) ## 2 divergent transitions               

check_all_diagnostics(dvr.inter)


y <- as.vector(testdat$dvr)
yrep <- extract(dvr.inter)
yrep <- yrep$yhat
ppc <- ppc_stat(y, yrep)
ppc.max <- ppc_stat(y, yrep, stat = "max")
ppc.min <- ppc_stat(y, yrep, stat = "min")
ppc.sd <- ppc_stat(y, yrep, stat = "sd")

#
quartz()
grid.arrange(ppc, ppc.sd, ppc.max, ppc.min, ncol=2, nrow=2)



launch_shinystan(dvr.inter)

loo1 <- loo(dvr.inter.normal)

# Extract pointwise log-likelihood and compute LOO
log_lik_1 <- extract(dvr.inter.normal)

# as of loo v2.0.0 we can optionally provide relative effective sample sizes
# when calling loo, which allows for better estimates of the PSIS effective
# sample sizes and Monte Carlo error
r_eff <- relative_eff(exp(log_lik_1)) 

loo_1 <- loo(log_lik_1, r_eff = r_eff, cores = 2)
print(loo_1)