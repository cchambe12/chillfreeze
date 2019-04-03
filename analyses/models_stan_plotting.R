### Started 1 April 2019 - Cat
## Building stan models to assess impact of False springs on DVR

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(RColorBrewer)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Set working directory
setwd("~/Documents/git/chillfreeze/analyses")

## load the model
#load("stan/dvr_inter_ncp_nofaggranyssyl.Rda")
#load("stan/dvr_inter_ncp.Rda")
load("stan/dvr_inter_ncp_drought.Rda")

chill.stan <- read.csv("output/clean_dvr_drought.csv", header=TRUE)
chill.stan <- chill.stan[!is.na(chill.stan$dvr),]

#nospp <- c("NYSSYL", "FAGGRA") # species to exclude for now because not enough data
#chill.stan <- chill.stan[!(chill.stan$species%in%nospp),]

chill.stan <- subset(chill.stan, select=c("dvr", "tx", "chill1", "chill2", "species"))

chill.stan$species.name <- NA
chill.stan$species.name <- ifelse(chill.stan$species=="SALPUR", "Salix purpurea", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="CORRAC", "Cornus racemosa", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="BETPAP", "Betula papyrifera", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="BETPOP", "Betula populifolia", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="ALNRUG", "Alnus rugosa", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="SORAME", "Sorbus americana", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="ACESAC", "Acer saccharinum", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="VIBDEN", "Viburnum dentatum", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="FAGGRA", "Fagus grandifolia", chill.stan$species.name)
chill.stan$species.name <- ifelse(chill.stan$species=="NYSSYL", "Nyssa sylvatica", chill.stan$species.name)


#### Now for mu plots based of bb_analysis/models_stan_plotting.R ###
figpath <- "figures"
figpathmore <- "dvr_ncp_noncen_drought"

source("exp_muplot_drought.R")
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 10, name = "Paired"), 10)
# display.brewer.all()
alphahere = 0.4

sumer.ni <- summary(dvr.inter.ncp.drought)$summary
sumer.ni[grep("mu_", rownames(sumer.ni)),]

sort(unique(chill.stan$species)) # numbers are alphabetical


modelhere <- dvr.inter.ncp.drought
quartz()
muplotfx(modelhere, "", 8, 8, c(0,9), c(-15, 18) , 19.5, 5.5)
