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
#chill.stan <- read.csv("output/clean_dvr_drought.csv")

chill.stan <- chill.stan[!is.na(chill.stan$dvr),]
rmspp <- c("FAGGRA", "NYSSYL")
chill.stan <- chill.stan[!(chill.stan$species%in%rmspp),]

get_prior(dvr ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
          data=chill.stan)

dvr.mod <- brm(dvr ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
                    data=chill.stan, iter=4000, warmup=2500, 
                    prior = prior(normal(20, 10), class=Intercept),
                    control=list(max_treedepth=15, adapt_delta=0.99))
save(dvr.mod, file="~/Documents/git/chillfreeze/analyses/stan/dvr_brms.Rdata")


chill.stan$gslength.bb <- chill.stan$budsetdoy - chill.stan$budburst
#chill.stan$gslength.lo <- chill.stan$budsetdoy - chill.stan$leafout
chill.stan.gs <- chill.stan[!is.na(chill.stan$gslength.lo),]

gslength.modlo <- brm(gslength.lo ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
                       data=chill.stan.gs, iter=4000, warmup=2500, 
                    prior = prior(normal(200, 40), class=Intercept),
                       control=list(max_treedepth=15, adapt_delta=0.99))
save(gslength.modlo, file="~/Documents/git/chillfreeze/analyses/stan/gslengthlo_brms_adjusted.Rdata")


chill.stan.gsbb <- chill.stan[!is.na(chill.stan$gslength.bb),]
gslength.modbb <- brm(gslength.bb ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
                      data=chill.stan.gsbb, iter=4000, warmup=2500, 
                      prior = prior(normal(200, 40), class=Intercept),
                      control=list(max_treedepth=15, adapt_delta=0.99))
save(gslength.modbb, file="~/Documents/git/chillfreeze/analyses/stan/gslengthbb_brms_adjusted.Rdata")


chill.stan.ht <- chill.stan[!is.na(chill.stan$ht.final),]
chill.stan.ht$finaldiff <- chill.stan.ht$ht.final - chill.stan.ht$lo.ht
chill.stan.ht$finaldiff.rate <- (chill.stan.ht$finaldiff/chill.stan.ht$gslength.lo)*10 ## this makes the units mm/day
chill.stan.ht$finaldiff.rgr <- (log(chill.stan.ht$ht.final)-log(chill.stan.ht$lo.ht))/(chill.stan.ht$gslength.lo) * 1000 
chill.stan.htrate <- chill.stan.ht[!is.na(chill.stan.ht$finaldiff.rgr),]

get_prior(finaldiff.rgr ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
          data=chill.stan.htrate)

htdiffrgr.mod <- brm(finaldiff.rgr ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
                            data=chill.stan.htrate, iter=4000, warmup=2500,
                           control=list(max_treedepth=15, adapt_delta=0.99),
                         prior = prior(normal(5, 20), class=Intercept))
save(htdiffrgr.mod, file="~/Documents/git/chillfreeze/analyses/stan/htfinalrgr_brms.Rdata")

rmspp <- c("FAGGRA", "NYSSYL")
chill.stan <- chill.stan[!(chill.stan$species%in%rmspp),]
chill.stan.ht <- chill.stan[!is.na(chill.stan$ht.diff),]

get_prior(ht.diff ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
          data=chill.stan.ht)

htdiff.mod <- brm(ht.diff ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
                  data=chill.stan.ht, iter=4000, warmup=2500, 
                  control=list(max_treedepth=15, adapt_delta=0.99),
                  prior = prior(normal(5, 10), class=Intercept))
save(htdiff.mod, file="~/Documents/git/chillfreeze/analyses/stan/htdiff_brms.Rdata")

chill.stan <- chill.stan[!is.na(chill.stan$mg.cm2),]
chill.stan$mg.cm2 <- chill.stan$mg.cm2*100
chill.stan$chlavg <- as.numeric(chill.stan$chlavg)

chill.stan <- chill.stan[!is.na(chill.stan$chlavg),]
rmspp <- c("FAGGRA", "NYSSYL")
chill.stan <- chill.stan[!(chill.stan$species%in%rmspp),]

get_prior(chlavg ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
          data=chill.stan)

chl.mod <- brm(chlavg ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
                 data=chill.stan, iter=4000, warmup=2500, 
                 prior = prior(normal(30, 10), class=Intercept),
                 control=list(max_treedepth=15, adapt_delta=0.99))
save(chl.mod, file="~/Documents/git/chillfreeze/analyses/stan/chlavg_brms.Rdata")

#chill.stan <- chill.stan[!is.na(chill.stan$ht.rgr),]

#chill.stan <- chill.stan[!is.na(chill.stan$tough),]

#chill.stan <- chill.stan[!is.na(chill.stan$gslength),]
#chill.stan$roottoshoot <- chill.stan$roots/chill.stan$shoots
#chill.stan <- chill.stan[!is.na(chill.stan$shoots),]
#rmsppfornow <- c("FAGGRA", "NYSSYL", "ALNRUG")
#chill.stan <- chill.stan[!(chill.stan$species%in%rmsppfornow),]

chill.stan.rt <- chill.stan[!is.na(chill.stan$roots),]

get_prior(roots ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
          data=chill.stan.rt)

roots.mod <- brm(roots ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
            data=chill.stan.rt, iter=4000, warmup=2500, 
            prior = prior(normal(30, 15), class=Intercept),
            control=list(max_treedepth=15, adapt_delta=0.99))
save(roots.mod, file="~/Documents/git/chillfreeze/analyses/stan/roots_brms.Rdata")

chill.stan.sh <- chill.stan[!is.na(chill.stan$shoots),]

shoots.mod <- brm(shoots ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
                data=chill.stan.sh, iter=4000, warmup=2500,
                prior = prior(normal(30, 15), class=Intercept),
                control=list(max_treedepth=15, adapt_delta=0.99))
save(shoots.mod, file="~/Documents/git/chillfreeze/analyses/stan/shoots_brms.Rdata")

chill.stan$rootmassfrac <- chill.stan$roots/chill.stan$totbiomass
chill.stan$rootmassfrac.100 <- (chill.stan$roots/chill.stan$totbiomass)*100
chill.stan.rmf <- chill.stan[!is.na(chill.stan$rootmassfrac.100),]
get_prior(rootmassfrac.100 ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species), data=chill.stan.rmf)
          
rootmassfrac.mod <- brm(rootmassfrac.100 ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
           data=chill.stan.rmf, iter=4000, warmup=2500,
           prior = prior(normal(75, 25), class=Intercept),
           control=list(max_treedepth=15, adapt_delta=0.99))
save(rootmassfrac.mod, file="~/Documents/git/chillfreeze/analyses/stan/rootmassfrac100_brms.Rdata")

chill.stan$totbiomass <- chill.stan$roots + chill.stan$shoots
chill.stan.tb <- chill.stan[!is.na(chill.stan$totbiomass),]
totbiomass.mod <- brm(totbiomass ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
                       data=chill.stan.tb, iter=4000, warmup=2500, 
                      prior = prior(normal(40, 15), class=Intercept),
                       control=list(max_treedepth=15, adapt_delta=0.99))
save(totbiomass.mod, file="~/Documents/git/chillfreeze/analyses/stan/totbiomass_brms.Rdata")

chill.stan$biomassrate <- (chill.stan$totbiomass/chill.stan$gslength.lo)
chill.stan.tb <- chill.stan[!is.na(chill.stan$biomassrate),]
get_prior(biomassrate ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species), data=chill.stan.tb)
biomassrate.mod <- brm(biomassrate ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
                      data=chill.stan.tb, iter=4000, warmup=2500, 
                      prior = prior(normal(0, 15), class=Intercept),
                      control=list(max_treedepth=15, adapt_delta=0.99))
save(biomassrate.mod, file="~/Documents/git/chillfreeze/analyses/stan/biomassrate_brms.Rdata")


#chill.stan$ht.rgr <- (log(chill.stan$X60dayheight) - log(chill.stan$lo.ht)) * 10

chill.stan.meri <- chill.stan[!is.na(chill.stan$meristem),]

get_prior(meristem ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
          data=chill.stan.meri, family=binomial(link="logit"))

meri.mod <- brm(meristem ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
                data=chill.stan.meri, family=bernoulli(link="logit"), iter=4000, warmup=2500,
                prior = prior(normal(0, 10)),
                control=list(max_treedepth=15, adapt_delta=0.99))
save(meri.mod, file="~/Documents/git/chillfreeze/analyses/stan/meristem_brms.Rdata")

chill.stan.th <- chill.stan[!is.na(chill.stan$thick),]
chill.stan.th$thick <- chill.stan.th$thick*1000

get_prior(thick ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
           data=chill.stan.th)

thickness.mod <- brm(thick ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
                    data=chill.stan.th, iter=4000, warmup=2500, 
                    control=list(max_treedepth=15, adapt_delta=0.99))
save(thickness.mod, file="~/Documents/git/chillfreeze/analyses/stan/thickness_brms.Rdata")

chill.stan.tough <- chill.stan[!is.na(chill.stan$tough),]

get_prior(tough ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
          data=chill.stan.tough)

toughness.mod <- brm(tough ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species),
                            data=chill.stan.tough, iter=4000, warmup=2500,
                            prior=prior(normal(0,10)),
                            control=list(max_treedepth=15, adapt_delta=0.99))
save(toughness.mod, file="~/Documents/git/chillfreeze/analyses/stan/toughness_brms.Rdata")

if(FALSE){
meri.drought <- brm(meristem ~ tx*chill1 + tx*chill2 + tx*drought1 + tx*drought2 + (1| species),
    data=chill.stan, family=binomial(link="logit"), iter=4000, warmup=2500, 
    control=list(max_treedepth=15, adapt_delta=0.99))
save(meri.drought, file="~/Documents/git/chillfreeze/analyses/stan/meridrought_brms.Rdata")
}

#toughness.mod <- brm(reltough ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2 | species), 
 #                    data=chill.stan, iter=4000, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99))
#save(toughness.mod, file="~/Documents/git/chillfreeze/analyses/stan/reltoughness_brms.Rdata")

#ht.rgr.new <- brm(rgr_prebudset ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2|species), data=chill.stan,
 #                 iter=4000, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99))

#chlavg.mod <- brm(chlavg ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2|species), data=chill.stan,
 #                                  iter=4000, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99))

#thickness.mod <- brm(thickness ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2|species), data=chill.stan,
#                  iter=4000, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99))

#save(thickness.mod, file="~/Documents/git/chillfreeze/analyses/stan/thickness_brms.Rdata")

#htmid.mod <- brm(ht.diff ~ tx*chill1 + tx*chill2 + (tx*chill1 + tx*chill2|species), data=chill.stan,
 #              iter=4000, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99))

#save(htmid.mod, file="~/Documents/git/chillfreeze/analyses/stan/htmid_brms.Rdata")

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

  
  
  
  