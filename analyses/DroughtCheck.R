# 1 February 2019 - Cat
## How bad was this "drought" effect??? ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


#### Using chill.stan from AnalysisPrep.R - not saving since it's always changing

# The most similar groups: using Phylogeny from Ladwig et al 2019 paper
### 1) ALNGLU, BETPAP, BETPOP
### 2) CORRAC, NYSSYL
### 3) SORAME, VIBDEN 
### 4) FAGGRA, ACESAC, SALPUR


## Let's start with 4 weeks of chilling... 
fourweeks <- chill.stan[(chill.stan$chill=="1"),]

group1 <- c("ALNGLU", "BETPAP", "BETPOP") ### Betulaceae family
fourweeks.grp1 <- fourweeks[(fourweeks$species %in% group1),]







