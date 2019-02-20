### Start analysis! 
# 1 February 2019 - Cat
## See if there are any trends in DVR with treatment

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(dplyr)
library(lubridate)
library(chillR)
library(tidyr)
library(brms) ## just for initial glances during experiment!
library(rstan)
library(rstanarm)


# Setting working directory
setwd("~/Documents/git/chillfreeze/analyses/input")

obs <- read.csv("chillobs.csv", header = TRUE)

## Start cleaning a bit...
obs$bb <- as.Date(obs$bb, "%m/%d/%y")
obs$bb <- yday(obs$bb)

obs$lo <- as.Date(obs$lo, "%m/%d/%y")
obs$lo <- yday(obs$lo)

obs$start <- as.Date(obs$start, format = "%m/%d/%y") 

obs$budburst <- NA
obs$budburst <- ifelse(obs$start=="2018-12-24", obs$bb + 6, obs$budburst)
obs$budburst <- ifelse(obs$start=="2019-01-07", obs$bb - 7, obs$budburst)
obs$budburst <- ifelse(obs$start=="2019-01-21", obs$bb - 21, obs$budburst)

obs$leafout <- NA
obs$leafout <- ifelse(obs$start=="2018-12-24", obs$lo + 6, obs$leafout)
obs$leafout <- ifelse(obs$start=="2019-01-07", obs$lo - 7, obs$leafout)
obs$leafout <- ifelse(obs$start=="2019-01-21", obs$lo - 21, obs$leafout)

treats <- c(24, 44, 64, 84, 104, 124, 144, 164,
        26, 46, 66, 86, 106, 126, 146, 166,
        28, 48, 68, 88, 108, 128, 148, 168)


obs$inds <- substr(obs$id, 8, 10)

obs$tx <- ifelse(obs$inds %in% treats, 1, 0)

obs$chill <- NA
obs$chill <- ifelse(obs$start=="2018-12-24", 1, obs$chill)
obs$chill <- ifelse(obs$start=="2019-01-07", 2, obs$chill)
obs$chill <- ifelse(obs$start=="2019-01-21", 3, obs$chill)

obs$ht2 <- as.Date(obs$leafout + 28, origin = obs$start)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## Breakdown treatments and experiment - 19 February 2019 issues with greenhouse!
if(FALSE){
howfaralong <- obs
howfaralong$chilltx <- NA
howfaralong$chilltx <- ifelse(howfaralong$chill==1, "4wks", howfaralong$chilltx)
howfaralong$chilltx <- ifelse(howfaralong$chill==2, "6wks", howfaralong$chilltx)
howfaralong$chilltx <- ifelse(howfaralong$chill==3, "8wks", howfaralong$chilltx)

howfaralong$chilltx <- paste(howfaralong$chilltx, howfaralong$tx, sep="_")

howfaralong.leafout <- howfaralong[!is.na(howfaralong$lo),]
table(howfaralong.leafout$chilltx)

howfaralong.budburst <- howfaralong[is.na(howfaralong$lo) & !is.na(howfaralong$bb),]
table(howfaralong.budburst$chilltx)

howfaralong.budburst.frz <- howfaralong.budburst[(howfaralong.budburst$tx ==1 & howfaralong.budburst$frz!=""),]
table(howfaralong.budburst.frz$chilltx)
}
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 




chill.stan <- subset(obs, select=c("id", "budburst", "leafout", "tx", "chill", "lo.ht", "onemonth.ht", "ChlAvg"))


chill.stan$chill1 = ifelse(chill.stan$chill == 2, 1, 0) 
chill.stan$chill2 = ifelse(chill.stan$chill == 3, 1, 0) 

with(chill.stan, table(chill1, chill2))

chill.stan$species <- substr(chill.stan$id, 0, 6)
chill.stan$dvr <- chill.stan$leafout - chill.stan$budburst
chill.stan$ht.diff <- chill.stan$onemonth.ht - chill.stan$lo.ht

chill.stan <- chill.stan[!is.na(chill.stan$dvr),]

totspp <- c("ALNRUG", "BETPAP", "BETPOP", "CORRAC", "SALPUR")
chill.complete <- subset(chill.stan, chill.stan$tx <=2)
chill.complete <- chill.complete[(chill.complete$species %in% totspp),]

fit.dvr.tot <- brm(dvr ~ tx*species + tx*chill1 + chill1*species, data=chill.complete)

### just a quick lm model to see relationships
fit.dvr <- brm(dvr ~ tx*species + chill1 + chill2, data = chill.stan)
fit.bb <- brm(budburst ~ chill1 + chill2 + species, data=chill.stan)
fit.lo <- brm(leafout ~ chill1 + chill2 + tx + species, data=chill.stan)
fit.ht <- lm(dvr ~ lo.ht + species, data = chill.stan) # simple curiosity!


fit.ht.diff <- lm(ht.diff ~ tx, data=chill.stan)
fit.chl <- lm(ChlAvg ~ tx, data=chill.stan)




