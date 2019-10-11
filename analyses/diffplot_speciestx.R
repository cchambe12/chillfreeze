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
x <- "reltough" ## name for response ## ht.diff, dvr, tough
#mu <- expression(mu)
ylab <- "Leaf toughness (mN/leaf age(days))" # expression(paste("Leaf thickness (", mu, "m)", sep="")) ### y axis label
ylim <- c(0.5,7) ## c(-5,85) for rgr60, #c(5,30) for dvr, c(0.1,1) for tough, c(0.01,0.3) for thick
chillfrz$x <- chillfrz$reltough

if(x=="meristem"){
  meri <- subset(chillfrz, select=c(id, chill, x, tx, species))
  meri <- meri[!is.na(meri$x),]
  meri <- within(meri, { meritot <- as.numeric(ave(id, species, tx, chill, FUN=length))}) # total number of inds per group
  meridmg <- meri[(meri$x==1),] 
  meridmg$meridmg <- as.numeric(ave(meridmg$x, meridmg$species, meridmg$tx, meridmg$chill, FUN=length))# dmg meristem observations per group
  meridmg <- dplyr::select(meridmg, -id, -x)
  
  meri <- dplyr::select(meri, -id, -x)
  meri <- left_join(meri, meridmg)
  
  meri$meridmg <- ifelse(is.na(meri$meridmg), 0, meri$meridmg)
  meri$meriprop <- meri$meridmg/meri$meritot
  
  chillfrz <- left_join(chillfrz, meri)
  chillfrz$x <- chillfrz$meriprop
}


############################################################################

dvrbar <- subset(chillfrz, select=c("species", "chill", "x", "tx", "budburst"))
dvrbar <- na.omit(dvrbar)
dvrbar <- dvrbar[!duplicated(dvrbar),]
dvrbar$tx <-ifelse(dvrbar$tx==0, "control", "treatment")

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

chill.bars4 <- dvrbar[(dvrbar$chill==1),]
chill.bars6 <- dvrbar[(dvrbar$chill==2),]
chill.bars8 <- dvrbar[(dvrbar$chill==3),]

chill.bars4$dvrmean <- ave(chill.bars4$x, chill.bars4$tx, chill.bars4$species)
chill.bars4$dvrsd <- ave(chill.bars4$x, chill.bars4$tx, chill.bars4$species, FUN=sd)
chill.bars4$ymin <- chill.bars4$dvrmean-chill.bars4$dvrsd
chill.bars4$ymax <- chill.bars4$dvrmean+chill.bars4$dvrsd
#chill.bars4$meancont <- mean(chill.bars4[x && chill.bars4$tx=="control"]) # 0.44
#chill.bars4$meantx <- mean(chill.bars4$rgr_prebudset[chill.bars4$tx=="treatment"]) # 0.38

cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)
dvrbar4 <- ggplot(chill.bars4, aes(x=code, y=dvrmean, fill=code, alpha=tx)) + 
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
                    labels=chill.bars4$code) +
  #geom_hline(aes(yintercept = meancont), col="black", alpha=0.3, linetype="dashed") +
  #geom_hline(aes(yintercept = meantx), col="black", alpha=1, linetype="dashed") +
  #scale_y_continuous(breaks = sort(c(seq(min(5), max(35), by=5), 16.0, 18.8))) +
  scale_x_discrete(labels=c("ACESAC"="Acer saccharinum",
                              "ALNRUG"="Alnus rugosa",
                              "BETPAP"="Betula papyrifera",
                              "BETPOP"="Betula populifolia",
                              "CORRAC"="Cornus racemosa", 
                              "SALPUR"="Salix purpurea",
                              "SORAME"="Sorbus americana",
                              "VIBDEN"="Viburnum dentatum")) +
  scale_alpha_manual(name="Treatments", values=c(0.1, 1), labels=chill.bars4$tx) +
  guides(fill=FALSE) +
  ggtitle("A. Four weeks chilling")  + coord_cartesian(xlim=c(1, 8), ylim=ylim, expand=TRUE)

chill.bars6$dvrmean <- ave(chill.bars6$x, chill.bars6$tx, chill.bars6$species)
chill.bars6$dvrsd <- ave(chill.bars6$x, chill.bars6$tx, chill.bars6$species, FUN=sd)
chill.bars6$ymin <- chill.bars6$dvrmean-chill.bars6$dvrsd
chill.bars6$ymax <- chill.bars6$dvrmean+chill.bars6$dvrsd
#chill.bars6$meancont <- mean(chill.bars6$rgr_prebudset[chill.bars6$tx=="control"]) #15.4
#chill.bars6$meantx <- mean(chill.bars6$rgr_prebudset[chill.bars6$tx=="treatment"]) #17.5

dvrbar6 <- ggplot(chill.bars6, aes(x=code, y=dvrmean, fill=code, alpha=tx)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ymin, ymax=ymax),width = 0.2, position=position_dodge(0.9)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = "none",
        axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")#,
        #axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank()
        ) +
  xlab("") + 
  ylab(ylab) + 
  #geom_hline(aes(yintercept = meancont), col="black", alpha=0.3, linetype="dashed") +
  #geom_hline(aes(yintercept = meantx), col="black", alpha=1, linetype="dashed") +
  #scale_y_continuous(breaks = sort(c(seq(min(5), max(35), length.out=5), 15.4, 17.5))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=chill.bars4$code) +
  scale_x_discrete(labels=c("ACESAC"="Acer saccharinum",
                            "ALNRUG"="Alnus rugosa",
                            "BETPAP"="Betula papyrifera",
                            "BETPOP"="Betula populifolia",
                            "CORRAC"="Cornus racemosa", 
                            "SALPUR"="Salix purpurea",
                            "SORAME"="Sorbus americana",
                            "VIBDEN"="Viburnum dentatum")) +
  scale_alpha_manual(name="Treatments", values=c(0.1, 1), labels=chill.bars6$tx) +
  guides(fill=FALSE) +
  ggtitle("B. Six weeks chilling") + coord_cartesian(xlim=c(1, 8), ylim=ylim, expand=TRUE)


chill.bars8$dvrmean <- ave(chill.bars8$x, chill.bars8$tx, chill.bars8$species)
chill.bars8$dvrsd <- ave(chill.bars8$x, chill.bars8$tx, chill.bars8$species, FUN=sd)
chill.bars8$ymin <- chill.bars8$dvrmean-chill.bars8$dvrsd
chill.bars8$ymax <- chill.bars8$dvrmean+chill.bars8$dvrsd
#chill.bars8$meancont <- mean(chill.bars8$rgr_prebudset[chill.bars8$tx=="control"]) #13.2
#chill.bars8$meantx <- mean(chill.bars8$rgr_prebudset[chill.bars8$tx=="treatment"]) #16.9

dvrbar8 <- ggplot(chill.bars8, aes(x=code, y=dvrmean, fill=code, alpha=tx)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ymin, ymax=ymax),width = 0.2, position=position_dodge(0.9)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        #legend.position = "none",
        axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")#,
        #axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank()
        ) +
  xlab("") + 
  ylab(ylab) + 
  #geom_hline(aes(yintercept = meancont), col="black", alpha=0.3, linetype="dashed") +
  #geom_hline(aes(yintercept = meantx), col="black", alpha=1, linetype="dashed") +
  #scale_y_continuous(breaks = sort(c(seq(min(5), max(35), length.out=5), 13.2, 16.9))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=chill.bars4$code) +
  scale_x_discrete(labels=c("ACESAC"="Acer saccharinum",
                            "ALNRUG"="Alnus rugosa",
                            "BETPAP"="Betula papyrifera",
                            "BETPOP"="Betula populifolia",
                            "CORRAC"="Cornus racemosa", 
                            "SALPUR"="Salix purpurea",
                            "SORAME"="Sorbus americana",
                            "VIBDEN"="Viburnum dentatum")) +
  scale_alpha_manual(name="", values=c(0.1, 1), labels=chill.bars8$tx) +
  guides(fill=FALSE) +
  ggtitle("C. Eight weeks chilling") + coord_cartesian(xlim=c(1, 8), ylim=ylim, expand=TRUE)


quartz()
dvrbarplot <- grid.arrange(dvrbar4, dvrbar6, dvrbar8, ncol=3, widths=c(1, 1, 1.3))

ggsave(paste("figures/",x,"_speciesplot.png",sep=""),width=30, height=12,units="cm",bg = "white",dpi=500, plot=dvrbarplot)


