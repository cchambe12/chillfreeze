### Started 1 April 2019 - Cat
## Building stan models to assess impact of False springs on DVR

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(RColorBrewer)
library(rstan)
library(dplyr)
library(broom)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Set working directory
setwd("~/Documents/git/chillfreeze/analyses")

## load the model
#load("stan/dvr_brms.Rdata")
#load("stan/gslengthlo_brms_adjusted.Rdata")
#load("stan/meristem_brms.Rdata")

#load("stan/chlavg_brms.Rdata")
#load("stan/toughness_brms.Rdata")
#load("stan/thickness_brms.Rdata")


#load("stan/htfinal_brms.Rdata")
#load("stan/htfinalrate_brms.Rdata")
#load("stan/totbiomass_brms.Rdata")
#load("stan/biomassrate_brms.Rdata")
#load("stan/roottoshoot_brms.Rdata")

if(FALSE){
##load("stan/rgr_prebudset_brms.Rdata")
##load("stan/htdiff_brms.Rdata")
##load("stan/roots_brms.Rdata")
##load("stan/shoots_brms.Rdata")
}

chill.stan <- read.csv("output/clean_dvr_traits.csv")
#chill.stan$gslength.lo <- chill.stan$bset - chill.stan$leafout
#chill.stan <- chill.stan[!is.na(chill.stan$dvr),]

chill.stan$species.name <- NA
chill.stan$species.name <- ifelse(chill.stan$species=="ACESAC", "Acer saccharinum", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="ALNRUG", "Alnus rugosa", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="BETPAP", "Betula papyrifera", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="BETPOP", "Betula populifolia", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="CORRAC", "Cornus racemosa", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="SALPUR", "Salix purpurea", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="SORAME", "Sorbus americana", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="VIBDEN", "Viburnum dentatum", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="FAGGRA", "Fagus grandifolia", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="NYSSYL", "Nyssa sylvatica", chill.stan$species.name)

rmspp <- c("FAGGRA", "NYSSYL")
chill.stan <- chill.stan[!(chill.stan$species%in%rmspp),]


#### Now for mu plots based of bb_analysis/models_stan_plotting.R ###
figpath <- "figures"
figpathmore <- "htfinalrate_brms" ### change based on model

cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 10, name = "Paired"), 8)
# display.brewer.all()
alphahere = 0.4
#xlab <- "Model estimate of change in \nduration of vegetative risk (days)"
xlab <- "Model estimate of change in growing season length (days)"
#xlab <- "Model estimate of change in shoot apical meristem damage"

#xlab <- expression(paste("Model estimate of change in chlorophyll content (mg/", cm^2, ")"))
#xlab <- "Model estimate of change in leaf toughness (N)"
#mu <- expression(mu)
#xlab <- expression(paste("Model estimate of change in leaf thickness (", mu, "m)", sep="")) ## change based on model

#xlab <- "Model estimate of change in shoot growth (cm)"
xlab <- "Model estimate of change in shoot growth per day (mm/day)"
#xlab <- "Model estimate of change in total biomass (g)"
#xlab <- "Model estimate of change in belowground \nto aboveground biomass ratio (g)"

if(FALSE){
#xlab <- "Model estimate of change in belowground biomass (g)"
#xlab <- "Model estimate of change in aboveground biomass (g)"
}



#sumer.ni <- summary()$summary
#sumer.ni[grep("mu_", rownames(sumer.ni)),]

#sort(unique(chill.stan$species)) # numbers are alphabetical

spp <- unique(chill.stan$species)

modelhere <- htdiffrate.mod

tx <- coef(modelhere, prob=c(0.25, 0.75))$species[, c(1, 3:4), 2] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select(mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(spp)){
  new.names[i]<-paste("tx", "[", i, "]", sep="")
}
tx$parameter<-new.names
chill1 <- coef(modelhere, prob=c(0.25, 0.75))$species[, c(1, 3:4), 3] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select( mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(spp)){
  new.names[i]<-paste("chill1", "[", i, "]", sep="")
}
chill1$parameter<-new.names
mod.ranef<-full_join(tx, chill1)
chill2 <- coef(modelhere, prob=c(0.25, 0.75))$species[, c(1, 3:4), 4] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select( mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(spp)){
  new.names[i]<-paste("chill2", "[", i, "]", sep="")
}
chill2$parameter<-new.names
mod.ranef <- full_join(mod.ranef, chill2)
txchill1 <- coef(modelhere, prob=c(0.25, 0.75))$species[, c(1, 3:4), 5] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select( mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(spp)){
  new.names[i]<-paste("tx:chill1", "[", i, "]", sep="")
}
txchill1$parameter<-new.names
mod.ranef<-full_join(mod.ranef, txchill1)
txchill2 <- coef(modelhere, prob=c(0.25, 0.75))$species[, c(1, 3:4), 6] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select( mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(spp)){
  new.names[i]<-paste("tx:chill2", "[", i, "]", sep="")
}
txchill2$parameter<-new.names
mod.ranef<-full_join(mod.ranef, txchill2)

modoutput <- tidy(modelhere, prob=c(0.5))
#quartz()
source("exp_muplot_brms.R")

#muplotfx(modelhere, "", 8, 8, c(0,5), c(-10, 10) , 16.5, 3.5) ## use for DVR
#muplotfx(modelhere, "", 8, 8, c(0,5), c(-35, 35) , 36.5, 3.5) ## use for gslengthlo
#muplotfx(modelhere, "", 8, 8, c(0,5), c(-8, 8) , 9, 3.5) ## meristem

#muplotfx(modelhere, "", 8, 8, c(0,5), c(-5, 5) , 5.5, 3.5) ## chloro
#muplotfx(modelhere, "", 8, 8, c(0,5), c(-.15, .15) , .16, 3.5) ##toughness
#muplotfx(modelhere, "", 8, 8, c(0,5), c(-35, 35) , 36.5, 3.5) ## thickness

#muplotfx(modelhere, "", 8, 8, c(0,5), c(-22, 22) , 23.5, 3.5) ## ht final
#muplotfx(modelhere, "", 8, 8, c(0,5), c(-8, 8) , 9, 3.5) ## tot biomass
muplotfx(modelhere, "", 8, 8, c(0,5), c(-1, 1) , 1.1, 3.5) ### root to shoot and growth rate


#### Not included for now: muplotfx(modelhere, "", 8, 8, c(0,5), c(-10, 10) , 11, 3.5) ## ht diff