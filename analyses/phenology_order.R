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

bbandgs.sub <- subset(bbandgs, select=c("rank", "txchill", "species"))
bbandgs.sub <- bbandgs.sub[!duplicated(bbandgs.sub),]

bbandgs.sub <- arrange(bbandgs.sub, rank)
bbandgs.sub$rank_order <- 1:nrow(bbandgs.sub)

# Step 2) Plot using this rank
cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)
rankbytx <- ggplot(bbandgs.sub, aes(y=rank_order, x=txchill, col=species)) + 
  geom_line(aes(group=species)) + 
  coord_cartesian(ylim=c(0, 55)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = "none",
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.text = element_text(face="italic")) +
  xlab("") + 
  ylab("Order of leafout") +  
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
                              "31"="False Spring x \n8wks Chill")) + scale_y_continuous(expand=c(0,0))


#quartz()
#rankbytx

#### Now let's look at raw leafout data by treatments
leafoutbytx <- ggplot(bbandgs, aes(y=leafout, x=txchill, col=species)) +  geom_jitter(width=0.2) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position="none",
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.text = element_text(face="italic")) +
  xlab("") + 
  ylab("Day of leafout") +  
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
                            "31"="False Spring x \n8wks Chill")) 

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(leafoutbytx)


#quartz()

png("figures/leafout_orderandraw.png", ### makes it a nice png and saves it so it doesn't take forever to load as a pdf!
    width=12,
    height=4.5, units="in", res = 350 )

grid.arrange(rankbytx, leafoutbytx, mylegend, ncol=3, widths=c(1.2,1.2,0.35))
dev.off()

## Great!! 

# Step 3: Now let's try and reorder and be easier about days ranking system...
bbandgs$lobytxchill <- ave(bbandgs$leafout, bbandgs$species, bbandgs$txchill)


### So I think this has to be a for loop... where we order species_tx by rank and then adjust by window of 2 days
speciestxrank <- subset(bbandgs, select=c(species_tx, lobytxchill, txchill, rank))
speciestxrank <- speciestxrank[!duplicated(speciestxrank),]
speciestxrank <- arrange(speciestxrank, rank)

speciestxrank$rank_order <- 1:nrow(speciestxrank)

speciestxrank <- arrange(speciestxrank, txchill, rank)

speciestxrank$rank_adj <- NA

txs <- sort(unique(speciestxrank$species_tx))
newranks <- c()

for(j in 1:length(txs)) {  #j=5
  
  subby <- subset(speciestxrank, as.numeric(speciestxrank$txchill)==txs[j])
  
  for(i in c(1:nrow(subby))) { ## i=2
    
    if(i==1) {
      
      subby$rank_adj[i] <- subby$rank_order[i]
      
    } else if (i>=3 && (subby$lobytxchill[i] - 3) <= subby$lobytxchill[i-2]) {
      
      subby$rank_adj[i] <- subby$rank_order[i-2] + 0.8
      
    } else if (i>=2 && (subby$lobytxchill[i] - 3) <= subby$lobytxchill[i-1]) {
      
      subby$rank_adj[i] <- subby$rank_order[i-1] + 0.4
      
    } else 
      
      subby$rank_adj[i] <- subby$rank_order[i]
  
  }
  
  newranks <- c(newranks, subby$rank_adj)
  
}

speciestxrank$rank_adj <- NULL

spp_rankfix <- cbind(speciestxrank, newranks)

bbandgs_rankfix <- full_join(bbandgs, spp_rankfix)
  

cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)
rankadj_bytx <- ggplot(bbandgs_rankfix, aes(y=newranks, x=txchill, col=species)) + 
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
                            "31"="False Spring x \n8wks Chill")) + scale_y_continuous(expand = c(0, 0))


quartz()
rankadj_bytx


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

##################################################################################
speciestxrank_bset <- subset(bbandgs, select=c(species_tx, bsetbytxchill, txchill, rankbset))
speciestxrank_bset <- speciestxrank_bset[!duplicated(speciestxrank_bset),]
speciestxrank_bset <- arrange(speciestxrank_bset, rankbset)

speciestxrank_bset$rank_order <- 1:nrow(speciestxrank_bset)

speciestxrank_bset <- arrange(speciestxrank_bset, txchill, rankbset)

speciestxrank_bset$rank_adj <- NA

txs <- as.numeric(sort(unique(speciestxrank_bset$txchill)))
newranks <- c()

for(j in 1:length(txs)) {  #j=5
  
  subby <- subset(speciestxrank_bset, as.numeric(speciestxrank_bset$txchill)==txs[j])
  
  for(i in c(1:nrow(subby))) { ## i=2
    
    if(i==1) {
      
      subby$rank_adj[i] <- subby$rank_order[i]
      
    } else if (i>=3 && (subby$bsetbytxchill[i] - 3) <= subby$bsetbytxchill[i-2]) {
      
      subby$rank_adj[i] <- subby$rank_order[i-2] + 0.8
      
    } else if (i>=2 && (subby$bsetbytxchill[i] - 3) <= subby$bsetbytxchill[i-1]) {
      
      subby$rank_adj[i] <- subby$rank_order[i-1] + 0.4
      
    } else 
      
      subby$rank_adj[i] <- subby$rank_order[i]
    
  }
  
  newranks <- c(newranks, subby$rank_adj)
  
}

speciestxrank_bset$rank_adj <- NULL

spp_rankfix_bset <- cbind(speciestxrank_bset, newranks)

bbandgs_rankfix_bset <- full_join(bbandgs, spp_rankfix_bset)


cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)
rankadj_bytx_bset <- ggplot(bbandgs_rankfix_bset, aes(y=newranks, x=txchill, col=species)) + 
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
                            "31"="False Spring x \n8wks Chill")) + scale_y_continuous(expand = c(0, 0))


quartz()
rankadj_bytx_bset





############### Add in budburst for the supp:

#### Let's look at overall averages and then do barplots... I think that will be the best first stab
bbandgs$lobyspp <- ave(bbandgs$budburst, bbandgs$species)
bbandgs$code <- reorder(bbandgs$species, bbandgs$lobyspp) ## Levels: SALPUR CORRAC BETPAP BETPOP ALNRUG VIBDEN SORAME ACESAC

### Okay, now we're going to try and rank them based on order by treatment by species
# Step 1)
bbandgs$lobytxchill <- ave(bbandgs$budburst, bbandgs$species, bbandgs$txchill)

bbandgs$species_tx <- paste(bbandgs$species, bbandgs$txchill, sep="_")
bbandgs$code <- reorder(bbandgs$species_tx, bbandgs$lobytxchill)


bbandgs$rank <- rank(bbandgs$lobytxchill, bbandgs$species_tx)

bbandgs.sub <- subset(bbandgs, select=c("rank", "txchill", "species"))
bbandgs.sub <- bbandgs.sub[!duplicated(bbandgs.sub),]

bbandgs.sub <- arrange(bbandgs.sub, rank)
bbandgs.sub$rank_order <- 1:nrow(bbandgs.sub)

# Step 2) Plot using this rank
cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)
rankbytx <- ggplot(bbandgs.sub, aes(y=rank_order, x=txchill, col=species)) + 
  geom_line(aes(group=species)) + 
  coord_cartesian(ylim=c(0, 55)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = "none",
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.text = element_text(face="italic")) +
  xlab("") + 
  ylab("Order of budburst") +  
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
                            "31"="False Spring x \n8wks Chill")) + scale_y_continuous(expand=c(0,0))


#quartz()
#rankbytx

#### Now let's look at raw budburst data by treatments
budburstbytx <- ggplot(bbandgs, aes(y=budburst, x=txchill, col=species)) +  geom_jitter(width=0.2) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position="none",
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.text = element_text(face="italic")) +
  xlab("") + 
  ylab("Day of budburst") +  
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
                            "31"="False Spring x \n8wks Chill")) 

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(budburstbytx)


#quartz()

png("figures/budburst_orderandraw.png", ### makes it a nice png and saves it so it doesn't take forever to load as a pdf!
    width=12,
    height=4.5, units="in", res = 350 )

grid.arrange(rankbytx, budburstbytx, mylegend, ncol=3, widths=c(1.2,1.2,0.35))
dev.off()


