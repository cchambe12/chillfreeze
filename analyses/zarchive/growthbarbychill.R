## 29 May 2019 - Cat
# Hoping to disentangle species differences between treatments

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

##### Now for some bar plots with error bars, ordered by day of budburst #####

#### THINGS TO CHANGE BASED ON DIFFERENT TRAITS/RESPONSE VARIABLES!!!! #####
x <- "totbiomass" ## name for response ## ht.diff, dvr, tough
#mu <- expression(mu)
ylab <- "Total biomass (g)" ##"Leaf toughness (mN/leaf age(days))" # expression(paste("Leaf thickness (", mu, "m)", sep="")) ### y axis label
ylim <- c(8,100) ##c(0.5,7) ## c(-5,85) for rgr60, #c(5,30) for dvr, c(0.1,1) for tough, c(0.01,0.3) for thick
chillfrz$x <- chillfrz$totbiomass

############################################################################

dvrbar <- subset(chillfrz, select=c("species", "chill", "x", "tx", "budburst"))
dvrbar <- na.omit(dvrbar)
dvrbar <- dvrbar[!duplicated(dvrbar),]

dvrbar$species.name <- NA
dvrbar$species.name <- ifelse(dvrbar$species=="ACESAC", "Acer saccharinum", dvrbar$species.name)
dvrbar$species.name <- ifelse(dvrbar$species=="ALNRUG", "Alnus rugosa", dvrbar$species.name)
dvrbar$species.name <- ifelse(dvrbar$species=="BETPAP", "Betula papyrifera", dvrbar$species.name)
dvrbar$species.name <- ifelse(dvrbar$species=="BETPOP", "Betula populifolia", dvrbar$species.name)
dvrbar$species.name <- ifelse(dvrbar$species=="CORRAC", "Cornus racemosa", dvrbar$species.name)
dvrbar$species.name <- ifelse(dvrbar$species=="SALPUR", "Salix purpurea", dvrbar$species.name)
dvrbar$species.name <- ifelse(dvrbar$species=="SORAME", "Sorbus americana", dvrbar$species.name)
dvrbar$species.name <- ifelse(dvrbar$species=="VIBDEN", "Viburnum dentatum", dvrbar$species.name)

dvrbar$code <- reorder(dvrbar$species, dvrbar$budburst)

dvrbar$chillname <- NA
dvrbar$chillname <- ifelse(dvrbar$chill==1, "4 weeks", dvrbar$chillname)
dvrbar$chillname <- ifelse(dvrbar$chill==2, "6 weeks", dvrbar$chillname)
dvrbar$chillname <- ifelse(dvrbar$chill==3, "8 weeks", dvrbar$chillname)


dvrbar$dvrmean <- ave(dvrbar$x, dvrbar$chill, dvrbar$species)
dvrbar$dvrsd <- ave(dvrbar$x, dvrbar$chill, dvrbar$species, FUN=sd)
dvrbar$ymin <- dvrbar$dvrmean-dvrbar$dvrsd
dvrbar$ymax <- dvrbar$dvrmean+dvrbar$dvrsd

cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)
dvrbar4 <- ggplot(dvrbar, aes(x=code, y=dvrmean, fill=code, alpha=chillname)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ymin, ymax=ymax),width = 0.2, position=position_dodge(0.9)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = "none",
        axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("") + 
  ylab(ylab) + 
  scale_fill_manual(name="Species", values=cols,
                    labels=dvrbar$code) +
  scale_x_discrete(labels=c("ACESAC"="Acer saccharinum",
                            "ALNRUG"="Alnus rugosa",
                            "BETPAP"="Betula papyrifera",
                            "BETPOP"="Betula populifolia",
                            "CORRAC"="Cornus racemosa", 
                            "SALPUR"="Salix purpurea",
                            "SORAME"="Sorbus americana",
                            "VIBDEN"="Viburnum dentatum")) +
  scale_alpha_manual(name="Levels of chilling", values=c(0.1, 0.5, 1), labels=dvrbar$chillname) +
  guides(fill=FALSE)  + coord_cartesian(xlim=c(1, 8), ylim=ylim, expand=TRUE)

quartz()
dvrbar4
