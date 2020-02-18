### 18 Feb 2020 - Cat
### Biomass figures

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(RColorBrewer)
library(egg)


# Setting working directory
setwd("~/Documents/git/chillfreeze/analyses/output")

chill.stan <- read.csv("clean_dvr_traits.csv", header=TRUE)

labs <- c("SALPUR"=expression(paste(italic("Salix purpurea"))),
          "CORRAC"=expression(paste(italic("Cornus racemosa"))),
          "BETPAP"=expression(paste(italic("Betula papyrifera"))),
          "BETPOP"=expression(paste(italic("Betula populifolia"))),
          "ALNRUG"=expression(paste(italic("Alnus incana"))),
          "SORAME"=expression(paste(italic("Sorbus americana"))),
          "ACESAC"=expression(paste(italic("Acer saccharinum"))),
          "VIBDEN"=expression(paste(italic("Viburnum dentatum"))),
          "FAGGRA"=expression(paste(italic("Fagus grandifolia"))),
          "NYSSYL"=expression(paste(italic("Nyssa sylvatica")))) 

cols <- colorRampPalette(brewer.pal(10,"Paired"))(10)

# to use for now until all species leafout
values <- c( "SALPUR"="#A6CEE3", "CORRAC"="#1F78B4", "BETPAP"="#B2DF8A", "BETPOP"="#33A02C", 
             "ALNRUG"="#FB9A99", "SORAME"="#E31A1C", "ACESAC"="#FDBF6F", 
             "VIBDEN"="#FF7F00", "FAGGRA"="#CAB2D6", "NYSSYL"="#6A3D9A")

species_order <- c("SALPUR", "CORRAC", "BETPAP", "BETPOP", "ALNRUG", "SORAME", "ACESAC", "VIBDEN", "FAGGRA", "NYSSYL")

chill.stan.ratio <- chill.stan[!is.na(chill.stan$shoots),]

quartz()
ggplot(chill.stan.ratio, aes(x=shoots, y=roots, col=as.factor(tx))) + geom_point(aes(col=as.factor(tx),shape=as.factor(tx))) + 
  geom_abline(intercept=0, slope=1) + 
  #stat_smooth(aes(x=shoots, y=roots, linetype=as.factor(chill)), method="lm", se=FALSE) + 
  scale_color_manual(values=c("blue", "red"), labels=c(0,1)) + facet_wrap(~species)

