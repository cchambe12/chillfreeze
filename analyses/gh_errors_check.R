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

### "Drought" treatment
fourdoy <- yday(as.Date("2019-02-19", origin = obs$start)) + 6
sixdoy <- yday(as.Date("2019-02-19", origin = obs$start)) - 7
eightdoy <- yday(as.Date("2019-02-19", origin = obs$start)) - 21


## Let's start with 4 weeks of chilling... 
###### VIBDEN and ACESAC were potentially affected for 4 weeks of chilling ######
fourweeks <- chill.stan[(chill.stan$chill=="1"),]

group1 <- c("ALNRUG", "BETPAP", "BETPOP") ### Betulaceae family - Just one ALNRUG individual and was on par with everything else
fourweeks.grp1 <- fourweeks[(fourweeks$species %in% group1),]

fourweeks.grp1$lo.tx.drought <- ifelse(fourweeks.grp1$leafout >= fourdoy, 1, 0)
fourweeks.grp1$bb.tx.drought <- ifelse(fourweeks.grp1$budburst <= fourdoy & fourweeks.grp1$leafout >= fourdoy, 1, 0)
#fourweeks.grp1$frz.tx.drought <- ifelse(fourweeks.grp1$fr < fourdoy, 1, 0)


group2 <- c("CORRAC", "NYSSYL") ### Not enough info for NYSSYL yet
fourweeks.grp2 <- fourweeks[(fourweeks$species %in% group2),]

fourweeks.grp2$lo.tx.drought <- ifelse(fourweeks.grp2$leafout >= fourdoy, 1, 0)
fourweeks.grp2$bb.tx.drought <- ifelse(fourweeks.grp2$budburst <= fourdoy & fourweeks.grp2$leafout >= fourdoy, 1, 0)
#fourweeks.grp1$frz.tx.drought <- ifelse(fourweeks.grp1$fr < fourdoy, 1, 0)

group3 <- c("SORAME", "VIBDEN") ### 
fourweeks.grp3 <- fourweeks[(fourweeks$species %in% group3),]

fourweeks.grp3$lo.tx.drought <- ifelse(fourweeks.grp3$leafout >= fourdoy, 1, 0)
fourweeks.grp3$bb.tx.drought <- ifelse(fourweeks.grp3$budburst <= fourdoy & fourweeks.grp3$leafout >= fourdoy, 1, 0)

mean(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==1)]) #21.25
mean(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==0)]) #17.07

sd(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==1)]) #8.04
sd(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==0)]) #3.93

mean(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==1 & fourweeks.grp3$tx==0)]) #18.7
mean(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==0 & fourweeks.grp3$tx==0)]) #15.875

mean(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==1 & fourweeks.grp3$tx==1)]) #23.2222
mean(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==0 & fourweeks.grp3$tx==1)]) #18.67

sd(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==1 & fourweeks.grp3$tx==1)]) #8.423
sd(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==0 & fourweeks.grp3$tx==1)]) #4.03


mean(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==1 & fourweeks.grp3$species=="SORAME")]) #16.6
mean(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==0) & fourweeks.grp3$species=="SORAME"]) #16.6

#### VIBDEN seems to have been affected but it's actually the frost treatment - or just one individual
mean(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==1 & fourweeks.grp3$species=="VIBDEN")]) # 23.363
mean(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==0 & fourweeks.grp3$species=="VIBDEN")]) # 18.25

mean(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==1 & fourweeks.grp3$species=="VIBDEN"
                         & fourweeks.grp3$tx==0)]) # 20
mean(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==0 & 
                           fourweeks.grp3$species=="VIBDEN" & fourweeks.grp3$tx==0)]) # 17.5

mean(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==0 & 
                           fourweeks.grp3$species=="VIBDEN" & fourweeks.grp3$tx==1)]) # 19
mean(fourweeks.grp3$dvr[(!is.na(fourweeks.grp3$dvr) & fourweeks.grp3$lo.tx.drought==0 & 
                           fourweeks.grp3$species=="VIBDEN" & fourweeks.grp3$tx==1)]) # 19



group4 <- c("FAGGRA", "ACESAC", "SALPUR") ### 
fourweeks.grp4 <- fourweeks[(fourweeks$species %in% group4),]

fourweeks.grp4$lo.tx.drought <- ifelse(fourweeks.grp4$leafout >= fourdoy, 1, 0)
fourweeks.grp4$bb.tx.drought <- ifelse(fourweeks.grp4$budburst <= fourdoy & fourweeks.grp4$leafout >= fourdoy, 1, 0)
#fourweeks.grp1$frz.tx.drought <- ifelse(fourweeks.grp1$fr < fourdoy, 1, 0)

mean(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==1)]) #15
mean(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==0)]) #11.2

sd(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==1)]) #2.954
sd(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==0)]) #3.147

mean(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==1 & fourweeks.grp4$tx==0)]) #13.5
mean(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==0 & fourweeks.grp4$tx==0)]) #9.846

mean(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==1 & fourweeks.grp4$tx==1)]) #15.75
mean(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==0 & fourweeks.grp4$tx==1)]) #13

mean(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==1 & fourweeks.grp4$species=="ACESAC")]) #15.33
mean(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==0 & fourweeks.grp4$species=="ACESAC")]) #12.286

sd(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==1 & fourweeks.grp4$species=="ACESAC")]) #2.598
sd(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==0 & fourweeks.grp4$species=="ACESAC")]) #3.498

mean(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==1 & 
                           fourweeks.grp4$species=="ACESAC" & fourweeks.grp4$tx==0)]) #15
mean(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==0 & 
                           fourweeks.grp4$species=="ACESAC" & fourweeks.grp4$tx==0)]) #11.4

sd(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==1 & 
                           fourweeks.grp4$species=="ACESAC" & fourweeks.grp4$tx==0)]) #1
sd(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==0 & 
                           fourweeks.grp4$species=="ACESAC" & fourweeks.grp4$tx==0)]) #3.435

mean(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==1 & 
                           fourweeks.grp4$species=="ACESAC" & fourweeks.grp4$tx==1)]) #15.5
mean(fourweeks.grp4$dvr[(!is.na(fourweeks.grp4$dvr) & fourweeks.grp4$lo.tx.drought==0 & 
                           fourweeks.grp4$species=="ACESAC" & fourweeks.grp4$tx==1)]) #14.5

################################################################################################
################################################################################################
######## Now for 6 weeks of chilling... 
###### ALNRUG potentially but most likely due to frost treatment ######
###### ACESAC potentially  ######
sixweeks <- chill.stan[(chill.stan$chill=="2"),]
################################################################################################

group1 <- c("ALNRUG", "BETPAP", "BETPOP") ### Betulaceae family - Just one ALNRUG individual and was on par with everything else
sixweeks.grp1 <- sixweeks[(sixweeks$species %in% group1),]

sixweeks.grp1$lo.tx.drought <- ifelse(sixweeks.grp1$leafout >= sixdoy, 1, 0)
sixweeks.grp1$bb.tx.drought <- ifelse(sixweeks.grp1$budburst <= sixdoy & sixweeks.grp1$leafout >= sixdoy, 1, 0)
#sixweeks.grp1$frz.tx.drought <- ifelse(sixweeks.grp1$fr < sixdoy, 1, 0)

mean(sixweeks.grp1$dvr[(!is.na(sixweeks.grp1$dvr) & sixweeks.grp1$lo.tx.drought==1)]) #22.61538
mean(sixweeks.grp1$dvr[(!is.na(sixweeks.grp1$dvr) & sixweeks.grp1$lo.tx.drought==0)]) #17.20588

mean(sixweeks.grp1$dvr[(!is.na(sixweeks.grp1$dvr) & sixweeks.grp1$lo.tx.drought==1)
                       & sixweeks.grp1$species=="ALNRUG"]) #22
mean(sixweeks.grp1$dvr[(!is.na(sixweeks.grp1$dvr) & sixweeks.grp1$lo.tx.drought==0
                        & sixweeks.grp1$species=="ALNRUG")]) #18.4

mean(sixweeks.grp1$dvr[(!is.na(sixweeks.grp1$dvr) & sixweeks.grp1$lo.tx.drought==1)
                       & sixweeks.grp1$species=="ALNRUG" & sixweeks.grp1$tx==0]) #19.5
mean(sixweeks.grp1$dvr[(!is.na(sixweeks.grp1$dvr) & sixweeks.grp1$lo.tx.drought==0
                        & sixweeks.grp1$species=="ALNRUG" & sixweeks.grp1$tx==0)]) #18.4

sd(sixweeks.grp1$dvr[(!is.na(sixweeks.grp1$dvr) & sixweeks.grp1$lo.tx.drought==1)
                       & sixweeks.grp1$species=="ALNRUG" & sixweeks.grp1$tx==0]) #19.5 +/- 2.12132
sd(sixweeks.grp1$dvr[(!is.na(sixweeks.grp1$dvr) & sixweeks.grp1$lo.tx.drought==0
                        & sixweeks.grp1$species=="ALNRUG" & sixweeks.grp1$tx==0)]) #18.4 +/- 1.34164

mean(sixweeks.grp1$dvr[(!is.na(sixweeks.grp1$dvr) & sixweeks.grp1$lo.tx.drought==1)
                       & sixweeks.grp1$species=="ALNRUG" & sixweeks.grp1$tx==1]) #19.5
mean(sixweeks.grp1$dvr[(!is.na(sixweeks.grp1$dvr) & sixweeks.grp1$lo.tx.drought==0
                        & sixweeks.grp1$species=="ALNRUG" & sixweeks.grp1$tx==1)]) #18.4



group2 <- c("CORRAC", "NYSSYL") ### Not enough info for NYSSYL yet
sixweeks.grp2 <- sixweeks[(sixweeks$species %in% group2),]

sixweeks.grp2$lo.tx.drought <- ifelse(sixweeks.grp2$leafout >= sixdoy, 1, 0)
sixweeks.grp2$bb.tx.drought <- ifelse(sixweeks.grp2$budburst <= sixdoy & sixweeks.grp2$leafout >= sixdoy, 1, 0)
#sixweeks.grp1$frz.tx.drought <- ifelse(sixweeks.grp1$fr < sixdoy, 1, 0)

mean(sixweeks.grp2$dvr[(!is.na(sixweeks.grp2$dvr) & sixweeks.grp2$lo.tx.drought==1)]) #21.333
mean(sixweeks.grp2$dvr[(!is.na(sixweeks.grp2$dvr) & sixweeks.grp2$lo.tx.drought==0)]) #16.69

mean(sixweeks.grp2$dvr[(!is.na(sixweeks.grp2$dvr) & sixweeks.grp2$lo.tx.drought==1
                        & sixweeks.grp2$tx==0)]) #21.333
mean(sixweeks.grp2$dvr[(!is.na(sixweeks.grp2$dvr) & sixweeks.grp2$lo.tx.drought==0
                        & sixweeks.grp2$tx==0)]) #16.69

mean(sixweeks.grp2$dvr[(!is.na(sixweeks.grp2$dvr) & sixweeks.grp2$lo.tx.drought==1
                        & sixweeks.grp2$tx==1)]) #21.333
mean(sixweeks.grp2$dvr[(!is.na(sixweeks.grp2$dvr) & sixweeks.grp2$lo.tx.drought==0
                        & sixweeks.grp2$tx==1)]) #17.4


group3 <- c("SORAME", "VIBDEN") ###  Seems to be okay! Not enough data really but overall good
sixweeks.grp3 <- sixweeks[(sixweeks$species %in% group3),]

sixweeks.grp3$lo.tx.drought <- ifelse(sixweeks.grp3$leafout >= sixdoy, 1, 0)
sixweeks.grp3$bb.tx.drought <- ifelse(sixweeks.grp3$budburst >= sixdoy & sixweeks.grp3$leafout >= sixdoy, 1, 0)

mean(sixweeks.grp3$dvr[(!is.na(sixweeks.grp3$dvr) & sixweeks.grp3$lo.tx.drought==1)]) #14.27586
mean(sixweeks.grp3$dvr[(!is.na(sixweeks.grp3$dvr) & sixweeks.grp3$lo.tx.drought==0)]) #16

sd(sixweeks.grp3$dvr[(!is.na(sixweeks.grp3$dvr) & sixweeks.grp3$lo.tx.drought==1)]) #3.682939
sd(sixweeks.grp3$dvr[(!is.na(sixweeks.grp3$dvr) & sixweeks.grp3$lo.tx.drought==0)]) #1.732051


group4 <- c("FAGGRA", "ACESAC", "SALPUR") ### can't really compare. All SALPUR were fully leafed out, 
                                            ## whereas the others were not
sixweeks.grp4 <- sixweeks[(sixweeks$species %in% group4),]

sixweeks.grp4$lo.tx.drought <- ifelse(sixweeks.grp4$leafout >= sixdoy, 1, 0)
sixweeks.grp4$bb.tx.drought <- ifelse(sixweeks.grp4$budburst <= sixdoy & sixweeks.grp4$leafout >= sixdoy, 1, 0)
#sixweeks.grp1$frz.tx.drought <- ifelse(sixweeks.grp1$fr < sixdoy, 1, 0)

mean(sixweeks.grp4$dvr[(!is.na(sixweeks.grp4$dvr) & sixweeks.grp4$bb.tx.drought==1)
                       & sixweeks.grp4$species=="ACESAC"]) #13.75
mean(sixweeks.grp4$dvr[(!is.na(sixweeks.grp4$dvr) & sixweeks.grp4$bb.tx.drought==0
                        & sixweeks.grp4$species=="ACESAC")]) #9.75


mean(sixweeks.grp4$dvr[(!is.na(sixweeks.grp4$dvr) & sixweeks.grp4$bb.tx.drought==1)
                       & sixweeks.grp4$species=="ACESAC" & sixweeks.grp4$tx==0]) #14.67
mean(sixweeks.grp4$dvr[(!is.na(sixweeks.grp4$dvr) & sixweeks.grp4$bb.tx.drought==0
                        & sixweeks.grp4$species=="ACESAC" & sixweeks.grp4$tx==0)]) #7


mean(sixweeks.grp4$dvr[(!is.na(sixweeks.grp4$dvr) & sixweeks.grp4$bb.tx.drought==1)
                       & sixweeks.grp4$species=="ACESAC" & sixweeks.grp4$tx==1]) #13.75
mean(sixweeks.grp4$dvr[(!is.na(sixweeks.grp4$dvr) & sixweeks.grp4$bb.tx.drought==0
                        & sixweeks.grp4$species=="ACESAC" & sixweeks.grp4$tx==1)]) #9.75


################################################################################################
################################################################################################
######## Now for 8 weeks of chilling... 
###### Group 1 potentially but very most likely due to frost treatment ######
###### SORAME - especially when combined with frost treatment ######
eightweeks <- chill.stan[(chill.stan$chill=="3"),]
################################################################################################

group1 <- c("ALNRUG", "BETPAP", "BETPOP") ### Betulaceae family - 
eightweeks.grp1 <- eightweeks[(eightweeks$species %in% group1),]

eightweeks.grp1$lo.tx.drought <- ifelse(eightweeks.grp1$leafout >= eightdoy, 1, 0)
eightweeks.grp1$bb.tx.drought <- ifelse(eightweeks.grp1$budburst < eightdoy & eightweeks.grp1$leafout >= eightdoy, 1, 0)
#eightweeks.grp1$frz.tx.drought <- ifelse(eightweeks.grp1$fr < eightdoy, 1, 0)

mean(eightweeks.grp1$dvr[(!is.na(eightweeks.grp1$dvr) & eightweeks.grp1$lo.tx.drought==1)]) #16.6
mean(eightweeks.grp1$dvr[(!is.na(eightweeks.grp1$dvr) & eightweeks.grp1$lo.tx.drought==0)]) #13.9

mean(eightweeks.grp1$dvr[(!is.na(eightweeks.grp1$dvr) & eightweeks.grp1$lo.tx.drought==1)
                       & eightweeks.grp1$species=="ALNRUG"]) #18.5383
mean(eightweeks.grp1$dvr[(!is.na(eightweeks.grp1$dvr) & eightweeks.grp1$lo.tx.drought==0
                        & eightweeks.grp1$species=="ALNRUG")]) #NA

mean(eightweeks.grp1$dvr[(!is.na(eightweeks.grp1$dvr) & eightweeks.grp1$lo.tx.drought==1)
                       & eightweeks.grp1$tx==0]) #13.84615
mean(eightweeks.grp1$dvr[(!is.na(eightweeks.grp1$dvr) & eightweeks.grp1$lo.tx.drought==0
                        & eightweeks.grp1$tx==0)]) #13.3333

mean(eightweeks.grp1$dvr[(!is.na(eightweeks.grp1$dvr) & eightweeks.grp1$lo.tx.drought==1)
                       & eightweeks.grp1$tx==1]) #18.22828
mean(eightweeks.grp1$dvr[(!is.na(eightweeks.grp1$dvr) & eightweeks.grp1$lo.tx.drought==0
                        & eightweeks.grp1$tx==1)]) #19



group2 <- c("CORRAC", "NYSSYL") ### Not enough info for NYSSYL yet
eightweeks.grp2 <- eightweeks[(eightweeks$species %in% group2),]

eightweeks.grp2$lo.tx.drought <- ifelse(eightweeks.grp2$leafout >= eightdoy, 1, 0)
eightweeks.grp2$bb.tx.drought <- ifelse(eightweeks.grp2$budburst <= eightdoy & eightweeks.grp2$leafout >= eightdoy, 1, 0)
#eightweeks.grp1$frz.tx.drought <- ifelse(eightweeks.grp1$fr < eightdoy, 1, 0)

mean(eightweeks.grp2$dvr[(!is.na(eightweeks.grp2$dvr) & eightweeks.grp2$bb.tx.drought==1)]) #17
mean(eightweeks.grp2$dvr[(!is.na(eightweeks.grp2$dvr) & eightweeks.grp2$bb.tx.drought==0)]) #16


group3 <- c("SORAME", "VIBDEN") ###  Seems to be okay! Not enough data really but overall good
eightweeks.grp3 <- eightweeks[(eightweeks$species %in% group3),]

eightweeks.grp3$lo.tx.drought <- ifelse(eightweeks.grp3$leafout >= eightdoy, 1, 0)
eightweeks.grp3$bb.tx.drought <- ifelse(eightweeks.grp3$budburst <= eightdoy & eightweeks.grp3$leafout >= eightdoy, 1, 0)

mean(eightweeks.grp3$dvr[(!is.na(eightweeks.grp3$dvr) & eightweeks.grp3$bb.tx.drought==1)]) #15.77273
mean(eightweeks.grp3$dvr[(!is.na(eightweeks.grp3$dvr) & eightweeks.grp3$bb.tx.drought==0)]) #11.7

mean(eightweeks.grp3$dvr[(!is.na(eightweeks.grp3$dvr) & eightweeks.grp3$bb.tx.drought==1
                          & eightweeks.grp3$species=="SORAME")]) #15.33
mean(eightweeks.grp3$dvr[(!is.na(eightweeks.grp3$dvr) & eightweeks.grp3$bb.tx.drought==0
                          & eightweeks.grp3$species=="SORAME")]) #11.7

mean(eightweeks.grp3$dvr[(!is.na(eightweeks.grp3$dvr) & eightweeks.grp3$bb.tx.drought==1
                          & eightweeks.grp3$species=="SORAME" & eightweeks.grp3$tx==0)]) #13.67
mean(eightweeks.grp3$dvr[(!is.na(eightweeks.grp3$dvr) & eightweeks.grp3$bb.tx.drought==0
                          & eightweeks.grp3$species=="SORAME" & eightweeks.grp3$tx==0)]) #11.6
mean(eightweeks.grp3$dvr[(!is.na(eightweeks.grp3$dvr) & eightweeks.grp3$bb.tx.drought==1
                          & eightweeks.grp3$species=="SORAME" & eightweeks.grp3$tx==1)]) #17
mean(eightweeks.grp3$dvr[(!is.na(eightweeks.grp3$dvr) & eightweeks.grp3$bb.tx.drought==0
                          & eightweeks.grp3$species=="SORAME" & eightweeks.grp3$tx==1)]) #11.8




group4 <- c("FAGGRA", "ACESAC", "SALPUR") ### Is actually species effect
eightweeks.grp4 <- eightweeks[(eightweeks$species %in% group4),]

eightweeks.grp4$lo.tx.drought <- ifelse(eightweeks.grp4$leafout >= eightdoy, 1, 0)
eightweeks.grp4$bb.tx.drought <- ifelse(eightweeks.grp4$budburst <= eightdoy & eightweeks.grp4$leafout >= eightdoy, 1, 0)
#eightweeks.grp1$frz.tx.drought <- ifelse(eightweeks.grp1$fr < eightdoy, 1, 0)

mean(eightweeks.grp1$dvr[(!is.na(eightweeks.grp1$dvr) & eightweeks.grp1$bb.tx.drought==1)]) #16.6
mean(eightweeks.grp1$dvr[(!is.na(eightweeks.grp1$dvr) & eightweeks.grp1$bb.tx.drought==0)]) #13.9



