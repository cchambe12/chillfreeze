## load the model
load("stan/dvr_brms.Rdata")
load("stan/gslengthlo_brms.Rdata") 
load("stan/meristem_brms.Rdata")

load("stan/chlavg_brms.Rdata")
load("stan/toughness_brms.Rdata")
load("stan/thickness_brms.Rdata")

load("stan/htdiff_brms.Rdata") 
load("stan/totbiomass_brms.Rdata")
load("stan/roottoshoot_brms.Rdata")


### Now for Species level variation in DVR:
dvrrandtx<-ranef(dvr.mod, pars="tx", summary=FALSE)
acesactx <- round(mean(dvrrandtx$species[,1,]), digits=2) ## -0.36
acesactxsd <- round(sd(dvrrandtx$species[,1,]), digits=2) ## 0.77
alnrugtx <- round(mean(dvrrandtx$species[,2,]), digits=2) ## 0.37
alnrugtxsd <- round(sd(dvrrandtx$species[,2,]), digits=2) ## 0.81
betpaptx <- round(mean(dvrrandtx$species[,3,]), digits=2) ## 0.45
betpaptxsd <- round(sd(dvrrandtx$species[,3,]), digits=2) ## 0.82
betpoptx <- round(mean(dvrrandtx$species[,4,]), digits=2) ## -0.43
betpoptxsd <- round(sd(dvrrandtx$species[,4,]), digits=2) ## 0.8
corractx <- round(mean(dvrrandtx$species[,5,]), digits=2) ## 0
corractxsd <- round(sd(dvrrandtx$species[,5,]), digits=2) ## 0.69
salpurtx <- round(mean(dvrrandtx$species[,6,]), digits=2) ## -0.14
salpurtxsd <- round(sd(dvrrandtx$species[,6,]), digits=2) ## 0.85
sorametx <- round(mean(dvrrandtx$species[,7,]), digits=2) ## -0.21
sorametxsd <- round(sd(dvrrandtx$species[,7,]), digits=2) ## 0.74
vibdentx <- round(mean(dvrrandtx$species[,8,]), digits=2) ## 0.35
vibdentxsd <- round(sd(dvrrandtx$species[,8,]), digits=2) ## 0.82