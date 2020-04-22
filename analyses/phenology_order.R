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














colz <- colorRampPalette(brewer.pal(6,"Paired"))(6)

orderlo <- ggplot(bbandgs, aes(x=code, y=lobytxchill, col=txchill)) + 
  geom_line(aes(group=txchill)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("") + 
  ylab("Day of year (leafout)") + 
  scale_x_discrete(labels=c("ACESAC"="Acer saccharinum",
                            "ALNRUG"="Alnus rugosa",
                            "BETPAP"="Betula papyrifera",
                            "BETPOP"="Betula populifolia",
                            "CORRAC"="Cornus racemosa", 
                            "SALPUR"="Salix purpurea",
                            "SORAME"="Sorbus americana",
                            "VIBDEN"="Viburnum dentatum")) +
  scale_color_manual(name="Treatments", values=c("01"=colz[1], 
                                                 "11"=colz[2],
                                                 "02"=colz[3],
                                                 "12"=colz[4],
                                                 "03"=colz[5],
                                                 "13"=colz[6]),
                    labels=c("01"="Control x 4wks Chill",
                             "11"="False Spring x 4wks Chill",
                             "02"="Control x 6wks Chill",
                             "12"="False Spring x 4wks Chill",
                             "03"="Control x 8wks Chill",
                             "13"="False Spring x 4wks Chill")) + coord_cartesian(expand = c(0,0))


quartz()
orderlo


bbandgs$bsetbyspp <- ave(bbandgs$budsetdoy, bbandgs$species)
bbandgs$codebset <- reorder(bbandgs$species, bbandgs$bsetbyspp) ## Levels: SALPUR CORRAC BETPAP BETPOP ALNRUG VIBDEN SORAME ACESAC
bbandgs$bsetbytxchill <- ave(bbandgs$budsetdoy, bbandgs$species, bbandgs$txchill)

orderbudset <- ggplot(bbandgs, aes(x=codebset, y=bsetbytxchill, col=txchill)) + 
  geom_line(aes(group=txchill)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("") + 
  ylab("Day of year (budset)") + 
  scale_x_discrete(labels=c("ACESAC"="Acer saccharinum",
                            "ALNRUG"="Alnus rugosa",
                            "BETPAP"="Betula papyrifera",
                            "BETPOP"="Betula populifolia",
                            "CORRAC"="Cornus racemosa", 
                            "SALPUR"="Salix purpurea",
                            "SORAME"="Sorbus americana",
                            "VIBDEN"="Viburnum dentatum")) +
  scale_color_manual(name="Treatments", values=c("01"=colz[1], 
                                                 "11"=colz[2],
                                                 "02"=colz[3],
                                                 "12"=colz[4],
                                                 "03"=colz[5],
                                                 "13"=colz[6]),
                     labels=c("01"="Control x 4wks Chill",
                              "11"="False Spring x 4wks Chill",
                              "02"="Control x 6wks Chill",
                              "12"="False Spring x 4wks Chill",
                              "03"="Control x 8wks Chill",
                              "13"="False Spring x 4wks Chill")) + coord_cartesian(expand = c(0,0))


quartz()
orderbudset



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
cols <- colorRampPalette(brewer.pal(6,"Dark2"))(6)
ggplot(bbandgs, aes(x=leafout, y=budsetdoy, col=txchill)) + theme_classic() + geom_point() + geom_smooth(method="lm", aes(col=txchill)) +
  scale_color_manual(name=("Treatment"), values=cols,
                     labels=c("01"="4 wks: Control",
                              "11"="4 wks: Treatment",
                              "02"="6 wks: Control",
                              "12"="6 wks: Treatment",
                              "03"="8 wks: Control",
                              "13"="8 wks: Treatment"))

##################################################################################
cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)
orderlo <- ggplot(bbandgs, aes(x=code, y=lobytxchill, col=code, linetype=txchill)) + 
  geom_bar(stat="identity", position=position_dodge(), fill="white") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        axis.text.x = element_text(face = "italic", angle=45, hjust=1)) +
  xlab("") + 
  ylab("Day of year (leafout)") + 
  scale_color_manual(name="Species", values=cols,
                     labels=c("ACESAC"="Acer saccharinum",
                              "ALNRUG"="Alnus rugosa",
                              "BETPAP"="Betula papyrifera",
                              "BETPOP"="Betula populifolia",
                              "CORRAC"="Cornus racemosa", 
                              "SALPUR"="Salix purpurea",
                              "SORAME"="Sorbus americana",
                              "VIBDEN"="Viburnum dentatum")) +
  scale_x_discrete(labels=c("ACESAC"="Acer saccharinum",
                            "ALNRUG"="Alnus rugosa",
                            "BETPAP"="Betula papyrifera",
                            "BETPOP"="Betula populifolia",
                            "CORRAC"="Cornus racemosa", 
                            "SALPUR"="Salix purpurea",
                            "SORAME"="Sorbus americana",
                            "VIBDEN"="Viburnum dentatum")) +
  scale_linetype_manual(name="Treatments", values=c(1:6), 
                        labels=c("01"="Control x 4wks Chill",
                                 "02"="Control x 6wks Chill",
                                 "03"="Control x 8wks Chill",
                                 "11"="False Spring x 4wks Chill",
                                 "12"="False Spring x 4wks Chill",
                                 "13"="False Spring x 4wks Chill")) + coord_cartesian(expand = c(0,0))


quartz()
orderlo




orderlo <- ggplot(bbandgs, aes(x=code, y=lobytxchill, fill=txchill)) + 
  geom_bar(stat="identity", position = position_dodge()) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("") + 
  ylab("Day of year (leafout)") + 
  scale_x_discrete(labels=c("ACESAC"="Acer saccharinum",
                            "ALNRUG"="Alnus rugosa",
                            "BETPAP"="Betula papyrifera",
                            "BETPOP"="Betula populifolia",
                            "CORRAC"="Cornus racemosa", 
                            "SALPUR"="Salix purpurea",
                            "SORAME"="Sorbus americana",
                            "VIBDEN"="Viburnum dentatum")) +
  scale_fill_manual(name="Treatments", values=colz, 
                    labels=c("01"="Control x 4wks Chill",
                             "02"="Control x 6wks Chill",
                             "03"="Control x 8wks Chill",
                             "11"="False Spring x 4wks Chill",
                             "12"="False Spring x 4wks Chill",
                             "13"="False Spring x 4wks Chill")) + coord_cartesian(expand = c(0,0))


quartz()
orderlo





colz <- colorRampPalette(brewer.pal(6,"Paired"))(6)
orderbset <- ggplot(bbandgs, aes(x=codebset, y=bsetbytxchill, fill=txchill)) + 
  geom_bar(stat="identity") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("") + 
  ylab("Day of year (budset)") + 
  scale_x_discrete(labels=c("ACESAC"="Acer saccharinum",
                            "ALNRUG"="Alnus rugosa",
                            "BETPAP"="Betula papyrifera",
                            "BETPOP"="Betula populifolia",
                            "CORRAC"="Cornus racemosa", 
                            "SALPUR"="Salix purpurea",
                            "SORAME"="Sorbus americana",
                            "VIBDEN"="Viburnum dentatum")) +
  scale_fill_manual(name="Treatments", values=colz, 
                    labels=c("01"="Control x 4wks Chill",
                             "02"="Control x 6wks Chill",
                             "03"="Control x 8wks Chill",
                             "11"="False Spring x 4wks Chill",
                             "12"="False Spring x 4wks Chill",
                             "13"="False Spring x 4wks Chill")) + coord_cartesian(expand = c(0,0))


quartz()
orderbset
