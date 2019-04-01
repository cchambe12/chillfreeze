### 1 April 2019 - Cat
### Initial figures 

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(RColorBrewer)
library(egg)


# Setting working directory
setwd("~/Documents/git/chillfreeze/analyses/output")

chill.stan <- read.csv("clean_dvr_60dayoutput.csv", header=TRUE)

labs <- c("ACESAC"=expression(paste(italic("Acer saccharinum"))),
          "ALNRUG"=expression(paste(italic("Alnus incana"))),
          "BETPAP"=expression(paste(italic("Betula papyrifera"))),
          "BETPOP"=expression(paste(italic("Betula populifolia"))),
          "CORRAC"=expression(paste(italic("Cornus racemosa"))),
          "FAGGRA"=expression(paste(italic("Fagus grandifolia"))),
          "NYSSYL"=expression(paste(italic("Nyssa sylvatica"))),
          "SALPUR"=expression(paste(italic("Salix purpurea"))),
          "SORAME"=expression(paste(italic("Sorbus americana"))),
          "VIBDEN"=expression(paste(italic("Viburnum dentatum")))) 


fourweeks <- subset(chill.stan, chill.stan$chill==1)
fourweeks <- fourweeks[!is.na(fourweeks$ht.diff),]
cols <- colorRampPalette(brewer.pal(8,"Set2"))(8)
height<- ggplot(fourweeks, aes(x=species, y=ht.diff, alpha=tx)) + geom_boxplot(aes(alpha=as.factor(tx), fill=as.factor(species), col=as.factor(species)), outlier.shape=NA) +
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        axis.title.x = element_blank()) + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Height Difference (cm)") +
  scale_alpha_manual(name="Treatment", values=c(0.2, 0.7),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_fill_manual(name="Species", values=cols,
                    labels=labs) + 
  scale_color_manual(name="Species", values=cols,
                     labels=labs) + scale_x_discrete(labels=labs) +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)

chlorophyll<- ggplot(fourweeks, aes(x=species, y=mg.cm2, alpha=tx)) + geom_boxplot(aes(alpha=as.factor(tx), fill=as.factor(species), col=as.factor(species)), outlier.shape=NA) +
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        axis.title.x = element_blank()) + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Chlorophyll Content (mg/cm2)") +
  scale_alpha_manual(name="Treatment", values=c(0.2, 0.7),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_fill_manual(name="Species", values=cols,
                    labels=labs) + 
  scale_color_manual(name="Species", values=cols,
                     labels=labs) + scale_x_discrete(labels=labs) +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)

sixweeks <- subset(chill.stan, chill.stan$chill==2)
sixweeks <- sixweeks[!is.na(sixweeks$ht.diff),]

height.six<- ggplot(sixweeks, aes(x=species, y=ht.diff, alpha=tx)) + geom_boxplot(aes(alpha=as.factor(tx), fill=as.factor(species), col=as.factor(species)), outlier.shape=NA) +
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        axis.title.x = element_blank()) + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Height Difference (cm)") +
  scale_alpha_manual(name="Treatment", values=c(0.2, 0.7),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_fill_manual(name="Species", values=cols,
                    labels=labs) + 
  scale_color_manual(name="Species", values=cols,
                     labels=labs) + scale_x_discrete(labels=labs) +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)

chlorophyll.six<- ggplot(sixweeks, aes(x=species, y=mg.cm2, alpha=tx)) + geom_boxplot(aes(alpha=as.factor(tx), fill=as.factor(species), col=as.factor(species)), outlier.shape=NA) +
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        axis.title.x = element_blank()) + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Chlorophyll Content (mg/cm2)") +
  scale_alpha_manual(name="Treatment", values=c(0.2, 0.7),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_fill_manual(name="Species", values=cols,
                    labels=labs) + 
  scale_color_manual(name="Species", values=cols,
                     labels=labs) + scale_x_discrete(labels=labs) +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)




quartz()
ggarrange(height, chlorophyll, height.six, chlorophyll.six, ncol=2)


#### More plots on DVR
fourweeks.dvr <- subset(chill.stan, chill.stan$chill==1)
fourweeks.dvr <- fourweeks.dvr[!is.na(fourweeks.dvr$dvr),]

species_order <- c("SALPUR", "CORRAC", "BETPAP", "BETPOP", "ALNRUG", "SORAME", "ACESAC", "VIBDEN", "FAGGRA", "NYSSYL")

dvr<- ggplot(fourweeks.dvr, aes(x=level_order, y=dvr, alpha=tx)) + geom_boxplot(aes(alpha=as.factor(tx), fill=as.factor(species), col=as.factor(species)), outlier.shape=NA) +
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        axis.title.x = element_blank()) + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Duration of Vegetative Risk") +
  scale_alpha_manual(name="Treatment", values=c(0.2, 0.7),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_fill_manual(name="Species", values=cols,
                    labels=labs) + 
  scale_color_manual(name="Species", values=cols,
                     labels=labs) + scale_x_discrete(labels=labs) +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)
quartz()
dvr

colz <- colorRampPalette(brewer.pal(11,"Set3"))(10)
ggplot(fourweeks.dvr, aes(x=budburst, y=dvr)) + geom_point(aes(alpha=as.factor(tx))) +
  theme_classic() + geom_smooth(aes(col=as.factor(tx)), method="lm") +
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Duration of Vegetative Risk") + xlab("Day of Budburst") +
  scale_alpha_manual(name="Treatment", values=c(0.2, 0.7),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_color_manual(name="Treatment", values=c("grey", "black"),
                     labels=c("0"="Control", "1"="False Spring"))

