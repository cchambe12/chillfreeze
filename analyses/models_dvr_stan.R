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
#chill.stan <- chill.stan[!is.na(chill.stan$dvr),]
#nospp <- c("NYSSYL", "FAGGRA") # species to exclude for now because not enough data
#chill.stan <- chill.stan[!(chill.stan$species%in%nospp),]

chill.stan$ht.rate <- ((chill.stan$X60dayheight - chill.stan$lo.ht)/chill.stan$lo.ht) * 100
chill.stan$ht.rgr <- (log(chill.stan$X60dayheight) - log(chill.stan$lo.ht)) * 10
chill.stan$ht.perc <- (chill.stan$X60dayheight/chill.stan$lo.ht)*100
chill.stan$RGR.stand <- (chill.stan$X60dayheight - log(chill.stan$lo.ht))
chill.stan$thickness <- ((chill.stan$thick1 + chill.stan$thick2)/2)*10


chill.stan <- subset(chill.stan, select=c("thickness", "tx", "chill1", "chill2", "species", "ht.rgr", "id", "chlavg", "ht.perc"))
leaf.chill <- chill.stan[!is.na(chill.stan$thickness),]
rgr.chill <- chill.stan[!is.na(chill.stan$ht.rgr),]
leaf.chill <- leaf.chill[(leaf.chill$chill1==1),]
chl.chill <- chill.stan[!is.na(chill.stan$chlavg),]
ht.chill <- chill.stan[!is.na(chill.stan$ht.rate),]

ht.chill1$mu_thickness <- ht.chill1$thickness
leaf.chill$mu_tx <- leaf.chill$tx

thick.tx1 <- stan_glmer(thickness ~ tx + (tx|species), data=leaf.chill)

rgr.tx1 <- stan_glmer(ht.rgr ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2|species), data=rgr.chill)
chl.tx1 <- stan_glmer(chlavg ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2|species), data=chl.chill) ## Actually interesting!!! Woohoo!!!
ht.tx1 <- stan_glmer(ht.rate ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2|species), data=ht.chill)


df <- as.data.frame(summary(chl.tx1))
row.names(df) <- c("mu_a", "mu_tx", "a_sp[1]", "mu_tx[1]", "a_sp[2]", "mu_tx[2]", "a_sp[3]", "mu_tx[3]", "a_sp[4]", "mu_tx[4]",
                  "a_sp[5]", "mu_tx[5]", "a_sp[6]", "mu_tx[6]", "a_sp[7]", "mu_tx[7]", "a_sp[8]", "mu_tx[8]", 
                  "sigma", "sigma_mu_a", "sigma_mu_a_tx", "sigma_mu_tx", "mean_PPD", "log-posterior")


output <- tidy(thick.tx1, prob=0.5, robust=TRUE)
save(ht.arm.twochill, file="stan/ht.brm.twochill.Rda")

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

rmspp <- c("NYSSYL", "FAGGRA")
check <- ht.chill1[(ht.chill1$id!="SALPUR_128"),]
check <- check[(check$species%in%rmspp),]

datalist.chill <- with(ht.chill1, 
                       list(y = ht.rgr, 
                            tx = tx, 
                            chill1 = chill1, 
                            chill2 = chill2,
                            sp = as.numeric(as.factor(species)),
                            N = nrow(ht.chill1),
                            n_sp = length(unique(ht.chill1$species))
                       )
)


########################################################################################
###################### Determining best model ##########################################
########################################################################################
if(FALSE){
dvr.inter.normal = stan('stan/dvr_winter_2level.stan', data = datalist.chill,
                 iter = 2500, warmup=1500, control=list(max_treedepth = 12,adapt_delta = 0.99)) ### 
               
#dvr.inter.cauchy = stan('stan/dvr_winter_2level_cauchy.stan', data = datalist.chill,
 #                       iter = 2500, warmup=1500, control=list(max_treedepth = 12,adapt_delta = 0.99)) ### 
 
rgr.rmspp.inter.ncp = stan('stan/dvr_winter_2level_ncp.stan', data = datalist.chill,
                                      iter = 2500, warmup=1500, control=list(max_treedepth = 12,adapt_delta = 0.99)) ## 

chl.inter.ncp = stan('stan/dvr_winter_2level_ncp.stan', data = datalist.chill,
                          iter = 2500, warmup=1500, control=list(max_treedepth = 12,adapt_delta = 0.99))

#dvr.rstanarm <- stan_glmer(dvr ~ tx*chill1 + tx*chill2 + (1|species), data=testdat) # Not as good!!
#dvr.brms <- brm(dvr ~ tx*chill1 + tx*chill2 + (1|species), data=testdat)
#dvr.brms.student <- brm(dvr ~ tx*chill1 + tx*chill2 + (1|species), data=testdat, family = student())


check_all_diagnostics(htdiff.inter.ncp)


y <- as.vector(chill.stan$ht.diff)
yrep <- extract(ht.inter.ncp)
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
                     iter = 2500, warmup=1500, control=list(max_treedepth = 12,adapt_delta = 0.99)) ## 


save(dvr.inter.ncp, file="stan/dvr_inter_ncp_nofaggranyssyl.Rda")
save(dvr.inter.ncp, file="stan/dvr_inter_ncp.Rda")
 

########################################################################################
########################################################################################
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

dvr.inter.ncp.drought = stan('stan/dvr_winter_2level_ncp_drought.stan', data = datalist.chill.drought,
                     iter = 2500, warmup=1500, control=list(max_treedepth = 12,adapt_delta = 0.99)) ## 

save(dvr.inter.ncp.drought, file="stan/dvr_inter_ncp_drought.Rda")

