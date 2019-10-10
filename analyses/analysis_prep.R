### Start analysis! 
# 1 February 2019 - Cat
## See if there are any trends in DVR with treatment

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(RColorBrewer)
library(egg)
library(brms) ## just for initial glances during experiment!
library(rstan)


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

obs$ht.mid <- as.Date(obs$leafout + 60, origin = obs$start)

#whentomeasure <- subset(obs, select=c("id", "ht.mid"))
#whentomeasure <- whentomeasure[!duplicated(whentomeasure),]
#write.csv(whentomeasure, file="~/Desktop/whentomeasureheights.csv", row.names = FALSE)

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

howfaralong.frz <- howfaralong[(howfaralong$tx ==1 & howfaralong$frz!=""),]
table(howfaralong.frz$chilltx)
}
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

chill.stan <- subset(obs, select=c("id", "budburst", "leafout", "tx", "chill", "lo.ht", "thick1", "thick2", "tough1", "tough2",
                                  "budset", "shoots", "roots", "ht.final", "mg.cm2", "phenol", "chlavg",
                                  "ht.diff", "meristem", "ht.prebudset"))


chill.stan$chill1 = ifelse(chill.stan$chill == 2, 1, 0) 
chill.stan$chill2 = ifelse(chill.stan$chill == 3, 1, 0) 

with(chill.stan, table(chill1, chill2))

chill.stan$species <- substr(chill.stan$id, 0, 6)
chill.stan$dvr <- chill.stan$leafout - chill.stan$budburst ### Using this point of code for "drought" effect test
#chill.stan$ht.diff <- chill.stan$X60dayheight - chill.stan$lo.ht 
#chill.stan$chlavg <- apply(chill.stan[,8:11], 1, mean)
chill.stan$tough <- ifelse(!is.na(chill.stan$tough2), (chill.stan$tough1 + chill.stan$tough2)/2, chill.stan$tough1)
chill.stan$thick <- (chill.stan$thick1 + chill.stan$thick2)/2

chill.stan$tough.date <- NA
chill.stan$tough.date <- ifelse(chill.stan$chill==1, (170+6), chill.stan$tough.date)
chill.stan$tough.date <- ifelse(chill.stan$chill==2, (184-7), chill.stan$tough.date)
chill.stan$tough.date <- ifelse(chill.stan$chill==3, (184-21), chill.stan$tough.date)

chill.stan$tough.age <- chill.stan$tough.date - chill.stan$lo

chill.stan$reltough <- (chill.stan$tough/chill.stan$tough.age)*1000

chill.stan$ht.late <- chill.stan$ht.prebudset - chill.stan$lo.ht

chill.stan$ht.date.new <- NA
chill.stan$ht.date.new <- ifelse(chill.stan$chill==1, (chill.stan$ht.date+6), chill.stan$tough.date)
chill.stan$ht.date.new <- ifelse(chill.stan$chill==2, (chill.stan$ht.date-7), chill.stan$tough.date)
chill.stan$ht.date.new <- ifelse(chill.stan$chill==3, (chill.stan$ht.date-21), chill.stan$tough.date)

chill.stan$budset <- as.Date(chill.stan$budset, "%m/%d/%y")
chill.stan$bset <- yday(chill.stan$budset)

chill.stan$budsetdoy <- NA
chill.stan$budsetdoy <- ifelse(chill.stan$chill==1, (chill.stan$budsetdoy+6), chill.stan$bset)
chill.stan$budsetdoy <- ifelse(chill.stan$chill==2, (chill.stan$budsetdoy-7), chill.stan$bset)
chill.stan$budsetdoy <- ifelse(chill.stan$chill==3, (chill.stan$budsetdoy-21), chill.stan$bset)

chill.stan$gslength <- chill.stan$budsetdoy - chill.stan$lo

chill.stan$rgr_prebudset <- (chill.stan$ht.prebudset - chill.stan$lo.ht)/(chill.stan$ht.date.new - chill.stan$leafout)*100

chill.stan$rgr_final <- (chill.stan$ht.final - chill.stan$lo.ht)/(chill.stan$gslength)*100

source("..//standardcurve.R")

chill.stan$folin <- folinfunc(as.numeric(chill.stan$phenol))


write.csv(chill.stan, file="~/Documents/git/chillfreeze/analyses/output/clean_dvr_traits.csv", row.names=FALSE)

#write.csv(chill.stan, file="~/Documents/git/chillfreeze/analyses/output/clean_dvr_60dayoutput.csv", row.names=FALSE)

#########################
#### Below code is just for quick checks with data coming in, to delete when experiment is over!!

chill.stan <- chill.stan[!is.na(chill.stan$dvr),]

#totspp <- c("ACESAC", "ALNRUG", "BETPAP", "BETPOP", "CORRAC", "SALPUR", "SORAME", "VIBDEN")
#chill.complete <- subset(chill.stan, chill.stan$tx <=2)
#chill.complete <- chill.stan[(chill.stan$species %in% totspp),]
chill.complete <- chill.stan

##### Add in "Drought" treatment
### "Drought" treatment
fourdoy <- yday(as.Date("2019-02-19", origin = obs$start)) + 6
sixdoy <- yday(as.Date("2019-02-19", origin = obs$start)) - 7
eightdoy <- yday(as.Date("2019-02-19", origin = obs$start)) - 21

chill.complete$drought <- ifelse(chill.complete$leafout >= fourdoy & chill.complete$chill==1, 1, 0)
chill.complete$drought <- ifelse(chill.complete$leafout >= sixdoy & chill.complete$chill==2, 1, 0)
chill.complete$drought <- ifelse(chill.complete$leafout >= eightdoy & chill.complete$chill==3, 1, 0)

chill.complete$drought <- ifelse(chill.complete$budburst <= fourdoy & chill.complete$leafout >= fourdoy & chill.complete$chill==1, 2, chill.complete$drought)
chill.complete$drought <- ifelse(chill.complete$budburst <= sixdoy & chill.complete$leafout >= sixdoy & chill.complete$chill==2, 2, chill.complete$drought)
chill.complete$drought <- ifelse(chill.complete$budburst <= eightdoy & chill.complete$leafout >= eightdoy & chill.complete$chill==3, 2, chill.complete$drought)

chill.complete$drought1 = ifelse(chill.complete$drought == 1, 1, 0) 
chill.complete$drought2 = ifelse(chill.complete$drought == 2, 1, 0) 

#write.csv(chill.complete, file="~/Documents/git/chillfreeze/analyses/output/clean_dvr_drought.csv", row.names=FALSE)

fit.dvr.all.drought <- brm(dvr ~ tx*chill1 + tx*chill2 + tx*drought1 + tx*drought2 + (1|species), data = chill.complete)
fit.dvr.all <- brm(dvr ~ tx*chill1 + tx*chill2 + (1|species), data = chill.complete)

### just a quick lm model to see relationships
fit.dvr <- brm(dvr ~ tx*chill1 + tx*chill2 + (1|species), data = chill.stan)
fit.bb <- brm(budburst ~ chill1 + chill2 + (1|species), data=chill.stan)
fit.lo <- brm(leafout ~ tx*chill1 + tx*chill2 + (1|species), data=chill.stan)
#fit.ht <- lm(dvr ~ lo.ht + species, data = chill.stan) # simple curiosity!


#fit.ht.diff <- lm(ht.diff ~ tx, data=chill.stan)
#fit.chl <- lm(ChlAvg ~ tx, data=chill.stan)




