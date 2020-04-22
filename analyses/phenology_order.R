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

bbandgs <- subset(chillfrz, select=c("budburst", "leafout", "tx", "chill", "budsetdoy", "species"))
bbandgs <- na.omit(bbandgs)
bbandgs$txchill <- paste0(bbandgs$chill, bbandgs$tx)

unique(bbandgs$txchill)
# "10"  "11"  "20"  "21"  "30"  "31"



#### Let's look at overall averages and then do barplots... I think that will be the best first stab
bbandgs$lobyspp <- ave(bbandgs$leafout, bbandgs$species)
bbandgs$code <- reorder(bbandgs$species, bbandgs$lobyspp) ## Levels: SALPUR CORRAC BETPAP BETPOP ALNRUG VIBDEN SORAME ACESAC

### Okay, now we're going to try and rank them based on order by treatment by species
# Step 1)
bbandgs$lobytxchill <- ave(bbandgs$leafout, bbandgs$species, bbandgs$txchill)

bbandgs$species_tx <- paste(bbandgs$species, bbandgs$txchill, sep="_")
bbandgs$code <- reorder(bbandgs$species_tx, bbandgs$lobytxchill)


bbandgs$rank <- rank(bbandgs$lobytxchill, bbandgs$species_tx)

# Step 2) Plot using this rank
cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)
rankbytx <- ggplot(bbandgs, aes(y=rank, x=txchill, col=species)) + 
  geom_line(aes(group=species)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.text = element_text(face="italic")) +
  xlab("") + 
  ylab("Order/Rank of leafout") +  
  scale_color_manual(name="Species", values=cols,
                     labels=c("ACESAC"="Acer saccharinum",
                            "ALNRUG"="Alnus rugosa",
                            "BETPAP"="Betula papyrifera",
                            "BETPOP"="Betula populifolia",
                            "CORRAC"="Cornus racemosa", 
                            "SALPUR"="Salix purpurea",
                            "SORAME"="Sorbus americana",
                            "VIBDEN"="Viburnum dentatum")) +
  scale_x_discrete(labels=c("10"="Control x \n4wks Chill",
                              "11"="False Spring x \n4wks Chill",
                              "20"="Control x \n6wks Chill",
                              "21"="False Spring x \n6wks Chill",
                              "30"="Control x \n8wks Chill",
                              "31"="False Spring x \n8wks Chill")) + coord_cartesian(expand = c(0,0))


quartz()
rankbytx

## Great!! 

# Step 3: Now let's try and reorder and be easier about days ranking system...
bbandgs$lobytxchill <- ave(bbandgs$leafout, bbandgs$species, bbandgs$txchill)








# Step 1) For budset now
bbandgs$bsetbytxchill <- ave(bbandgs$budsetdoy, bbandgs$species, bbandgs$txchill)

bbandgs$species_tx <- paste(bbandgs$species, bbandgs$txchill, sep="_")
bbandgs$codebset <- reorder(bbandgs$species_tx, bbandgs$lobytxchill)


bbandgs$rankbset <- rank(bbandgs$bsetbytxchill, bbandgs$species_tx)

# Step 2) Plot using this rank
cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)
bsetrankbytx <- ggplot(bbandgs, aes(y=rankbset, x=txchill, col=species)) + 
  geom_line(aes(group=species)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.text = element_text(face="italic")) +
  xlab("") + 
  ylab("Order/Rank of leafout") +  
  scale_color_manual(name="Species", values=cols,
                     labels=c("ACESAC"="Acer saccharinum",
                              "ALNRUG"="Alnus rugosa",
                              "BETPAP"="Betula papyrifera",
                              "BETPOP"="Betula populifolia",
                              "CORRAC"="Cornus racemosa", 
                              "SALPUR"="Salix purpurea",
                              "SORAME"="Sorbus americana",
                              "VIBDEN"="Viburnum dentatum")) +
  scale_x_discrete(labels=c("10"="Control x \n4wks Chill",
                            "11"="False Spring x \n4wks Chill",
                            "20"="Control x \n6wks Chill",
                            "21"="False Spring x \n6wks Chill",
                            "30"="Control x \n8wks Chill",
                            "31"="False Spring x \n8wks Chill")) + coord_cartesian(expand = c(0,0))


quartz()
bsetrankbytx


