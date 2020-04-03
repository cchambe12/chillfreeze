## 3 April 2020 - Cat
# Are we seeing a reassembling of communities? If insufficient chilling and/or a false spring, is the order of budburst/leafout or budset changing?

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

# Set Working Directory
setwd("~/Documents/git/chillfreeze/analyses")

chillfrz <- read.csv("output/clean_dvr_traits.csv", header=TRUE)
rmspp <- c("NYSSYL", "FAGGRA")
chillfrz <- chillfrz[!(chillfrz$species%in%rmspp),]

### It would be good to know if the order of species changes
# in any of your treatments. You can use  the
# 'order' or 'rank' command in R I think to transform mean leafout to just
#a rank and try to plot how consistent this is across treatments.

bbandgs <- subset(chillfrz, select=c("budburst", "leafout", "tx", "chill", "bset", "species"))
bbandgs <- na.omit(bbandgs)
bbandgs$txchill <- paste0(bbandgs$tx, bbandgs$chill)

unique(bbandgs$txchill)
# "01"  "11"  "02"  "12"  "03"  "13"

fourwk <- bbandgs[(bbandgs$txchill=="01"),]
fourwktx <- bbandgs[(bbandgs$txchill=="11"),]

sixwk <- bbandgs[(bbandgs$txchill=="02"),]
sixwktx <- bbandgs[(bbandgs$txchill=="12"),]

eightwk <- bbandgs[(bbandgs$txchill=="03"),]
eightwktx <- bbandgs[(bbandgs$txchill=="13"),]

### Now, let's check out the ranks...
## 4 weeks leafout:
fourwk$loorder <- ave(fourwk$leafout, fourwk$species)
# SALPUR BETPAP CORRAC BETPOP ALNRUG SORAME ACESAC VIBDEN

fourwktx$loorder <- ave(fourwktx$leafout, fourwktx$species)
# SALPUR BETPAP CORRAC BETPOP ALNRUG SORAME ACESAC VIBDEN - THE SAME!!

## 6 weeks:
sixwk$loorder <- ave(sixwk$leafout, sixwk$species)
# SALPUR BETPAP CORRAC BETPOP ALNRUG VIBDEN SORAME ACESAC - VIBDEN making a move!

sixwktx$loorder <- ave(sixwktx$leafout, sixwktx$species)
# SALPUR CORRAC BETPOP BETPAP VIBDEN ALNRUG SORAME ACESAC - what happened to BETPAP?? Again VIBDEN for the win!

## 8 weeks:
eightwk$loorder <- ave(eightwk$leafout, eightwk$species)
# SALPUR BETPAP CORRAC BETPOP ALNRUG VIBDEN SORAME ACESAC - same as 6 weeks

eightwktx$loorder <- ave(eightwktx$leafout, eightwktx$species)
# SALPUR CORRAC BETPOP BETPAP VIBDEN ALNRUG SORAME ACESAC - again same as 6 weeks!!! weird!


## 4 weeks budset:
fourwk$bsorder <- ave(fourwk$budsetdoy, fourwk$species)
# SORAME CORRAC ACESAC BETPAP VIBDEN BETPOP SALPUR ALNRUG

fourwktx$bsorder <- ave(fourwktx$budsetdoy, fourwktx$species)
# CORRAC SORAME BETPAP ACESAC VIBDEN BETPOP SALPUR ALNRUG

## 6 weeks budset:
sixwk$bsorder <- ave(sixwk$budsetdoy, sixwk$species)
# CORRAC SORAME BETPAP VIBDEN ACESAC SALPUR BETPOP ALNRUG - a good bit of reshuffling

sixwktx$bsorder <- ave(sixwktx$budsetdoy, sixwktx$species)
# CORRAC SORAME ACESAC BETPAP VIBDEN SALPUR BETPOP ALNRUG

## 8 weeks budset:
eightwk$bsorder <- ave(eightwk$budsetdoy, eightwk$species)
# CORRAC SORAME BETPAP ACESAC VIBDEN SALPUR BETPOP ALNRUG 

eightwktx$bsorder <- ave(eightwktx$budsetdoy, eightwktx$species)
# SORAME CORRAC BETPAP ACESAC VIBDEN SALPUR BETPOP ALNRUG


### Clean things up a bit then plot
ggplot(bbandgs, aes(x=leafout, y=budsetdoy))





