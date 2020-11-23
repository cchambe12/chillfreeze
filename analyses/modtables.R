## 24 March 2020 - Cat
# Setting up model output tables

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(RColorBrewer)
library(rstan)
library(dplyr)
library(broom)

# Set Working Directory
setwd("~/Documents/git/chillfreeze/analyses")

chillfrz <- read.csv("output/clean_dvr_traits.csv", header=TRUE)
rmspp <- c("NYSSYL", "FAGGRA")
chillfrz <- chillfrz[!(chillfrz$species%in%rmspp),]

## load the model
load("stan/dvr_brms.Rdata")
load("stan/gslengthlo_brms_adjusted.Rdata") 
load("stan/meristem_brms.Rdata")

load("stan/chlavg_brms.Rdata")
load("stan/toughness_brms.Rdata")
load("stan/thickness_brms.Rdata")

load("stan/htfinal_brms.Rdata") 
load("stan/htdiffrate_brms.Rdata")
load("stan/totbiomass_brms.Rdata")
load("stan/biomassrate100_brms.Rdata")
load("stan/roottoshoot_brms.Rdata")

mod <- thickness.mod

mod90<-as.data.frame(tidy(mod, prob=0.9))
names(mod90)<-c("term", "estimate", "error", "10%", "90%")
mod50<-as.data.frame(tidy(mod, prob=0.5))
names(mod50)<-c("term", "estimate", "error", "25%", "75%")
modfull <- full_join(mod90, mod50)
mod98<-as.data.frame(tidy(mod, prob=0.98))
names(mod98)<-c("term", "estimate", "error", "2%", "98%")
modfull <- full_join(modfull, mod98)
modfull <- subset(modfull, select=c("term", "estimate", "2%", "10%", "25%", "75%", "90%", "98%"))
write.csv(modfull, file="output/thickness_modeloutput.csv", row.names=FALSE)





