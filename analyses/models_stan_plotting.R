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
#load("stan/dvr_inter_ncp_skewnormal.Rda")
#load("stan/dvr_inter_ncp_drought.Rda")

#chill.stan <- read.csv("output/clean_dvr_drought.csv", header=TRUE)
#chill.stan <- read.csv("output/clean_dvr_60dayoutput.csv", header=TRUE)
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
#chill.stan$species.name <- ifelse(chill.stan$species=="FAGGRA", "Fagus grandifolia", chill.stan$species.name)
#chill.stan$species.name <- ifelse(chill.stan$species=="NYSSYL", "Nyssa sylvatica", chill.stan$species.name)


#### Now for mu plots based of bb_analysis/models_stan_plotting.R ###
figpath <- "figures"
figpathmore <- "toughtness" ### change based on model

source("exp_muplot.R")
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 10, name = "Paired"), 8)
# display.brewer.all()
alphahere = 0.4
xlab <- "Model estimate of change in \nleaf toughness (N)" ## change based on model

sumer.ni <- summary()$summary
sumer.ni[grep("mu_", rownames(sumer.ni)),]

sort(unique(chill.stan$species)) # numbers are alphabetical


modelhere <- dvr.inter.ncp
#quartz()
muplotfx(modelhere, "", 8, 8, c(0,5), c(-10, 10) , 10.5, 3.5)
#muplotfx(modelhere, "", 8, 8, c(0,5), c(-1.5, 1.5) , 1.7, 3.5)

