### Started 1 April 2019 - Cat
## Building stan models to assess impact of False springs on DVR

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

chill.stan <- read.csv("output/clean_dvr_60dayoutput.csv", header=TRUE)
#testdat <- read.csv("output/fakedata.csv", header=TRUE)
chill.stan <- chill.stan[!is.na(chill.stan$dvr),]
rmspp <- c("FAGGRA", "NYSSYL")

if(FALSE){
datalist.chill <- with(testdat, 
                     list(y = dvr, 
                          tx = tx, 
                          chill1 = chill1, 
                          #chill2 = chill2,
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


########################################################################################
###################### Determining best model ##########################################
########################################################################################
if(FALSE){
dvr.inter.ncp = stan('stan/dvr_winter_2level_ncp.stan', data = datalist.chill,
                 iter = 5000, warmup=2000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 

#dvr.inter.normal = stan('stan/dvr_winter_2level_ncp.stan', data = datalist.chill,
 #                       iter = 4000, warmup=2000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 
               
#dvr.inter.cauchy = stan('stan/dvr_winter_2level_cauchy.stan', data = datalist.chill,
 #                       iter = 2500, warmup=1500, control=list(max_treedepth = 12,adapt_delta = 0.99)) ### 
 

#dvr.rstanarm <- stan_glmer(dvr ~ tx*chill1 + tx*chill2 + (1|species), data=testdat) # Not as good!!
#dvr.brms <- brm(dvr ~ tx*chill1 + tx*chill2 + (1|species), data=testdat)
#dvr.brms.student <- brm(dvr ~ tx*chill1 + tx*chill2 + (1|species), data=testdat, family = student())


check_all_diagnostics(dvr.inter.ncp.skew)


y <- as.vector(chill.stan$dvr)
yrep <- extract(dvr.inter.skewnormal)
yrep <- yrep$yhat
ppc <- ppc_stat(y, yrep)
ppc.max <- ppc_stat(y, yrep, stat = "max")
ppc.min <- ppc_stat(y, yrep, stat = "min")
ppc.sd <- ppc_stat(y, yrep, stat = "sd")

#
quartz()
grid.arrange(ppc, ppc.sd, ppc.max, ppc.min, ncol=2, nrow=2)

library(loo)
log_lik_1 <- extract_log_lik(dvr.inter.normal, merge_chains = FALSE)
log_lik_2 <- extract_log_lik(dvr.inter.ncp, merge_chains = FALSE)
# as of loo v2.0.0 we can optionally provide relative effective sample sizes
# when calling loo, which allows for better estimates of the PSIS effective
# sample sizes and Monte Carlo error
r_eff <- relative_eff(exp(log_lik_1)) 

loo_1 <- loo(log_lik_1, r_eff = r_eff, cores = 2)
print(loo_1)

r_eff_2 <- relative_eff(exp(log_lik_2)) 

loo_2 <- loo(log_lik_2, r_eff = r_eff_2, cores = 2)
print(loo_2)

}

########################################################################################
########################################################################################
dvr.inter.ncp = stan('stan/dvr_winter_2level_ncp.stan', data = datalist.chill,
                     iter = 5000, warmup=2000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ## 7 divergent transitions


#save(dvr.inter.ncp, file="stan/dvr_inter_ncp.Rda")
 

########################################################################################
########################################################################################
if(FALSE){
chill.drought <- read.csv("output/clean_dvr_drought.csv", header=TRUE)

datalist.chill.drought <- with(chill.drought, 
                       list(y = dvr, 
                            tx = tx, 
                            chill1 = chill1, 
                            chill2 = chill2,
                            drought1 = drought1, 
                            drought2 = drought2,
                            sp = as.numeric(as.factor(species)),
                            N = nrow(chill.drought),
                            n_sp = length(unique(chill.drought$species))
                       )
)


dvr.inter.ncp.drought = stan('stan/dvr_winter_2level_ncp_drought_skew.stan', data = datalist.chill.drought,
                     iter = 4000, warmup=2000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ## 

check_all_diagnostics(dvr.inter.ncp.drought)


y <- as.vector(chill.stan$dvr)
yrep <- extract(dvr.inter.ncp.drought)
yrep <- yrep$yhat
ppc <- ppc_stat(y, yrep)
ppc.max <- ppc_stat(y, yrep, stat = "max")
ppc.min <- ppc_stat(y, yrep, stat = "min")
ppc.sd <- ppc_stat(y, yrep, stat = "sd")

save(dvr.inter.ncp.drought, file="stan/dvr_inter_ncp_drought.Rda")
}
