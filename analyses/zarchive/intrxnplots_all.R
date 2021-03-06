## 20 March 2020 - Cat
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


dvr <- subset(chillfrz, select=c("species", "chill", "dvr", "tx"))
dvr <- dvr[!duplicated(dvr),]
dvr <- na.omit(dvr)

cols <- colorRampPalette(brewer.pal(3,"Dark2"))(3)
dvrplot <- ggplot(dvr, aes(x=tx, y=dvr, shape=as.factor(chill), colour=as.factor(chill), fill=as.factor(chill))) +
  geom_smooth(method="lm", se=FALSE) + geom_jitter(width=0.1) +
  geom_point() +
  theme_classic() +
  theme(legend.position="none") +
  ggtitle("a)") +
  xlab("") +
  ylab("Duration of Vegetative Risk (days)") +
  expand_limits(y=0) +
  scale_y_continuous(breaks = seq(0, 40, by=5)) + 
  scale_x_continuous(breaks = c(0,1), labels=c("Control", "False Spring")) +
  scale_color_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                   "6 weeks",
                                                                   "8 weeks")) +
  scale_fill_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                   "6 weeks",
                                                                   "8 weeks")) +
  scale_shape_manual(name="Chill Treatment", values=c(15, 16, 17), labels=c("4 weeks",
                                                                  "6 weeks",
                                                                  "8 weeks"))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(dvrplot)

#ggsave(paste("figures/dvrsimple.png",sep=""),width=15, height=12,units="cm",bg = "white",dpi=500, plot=dvrplot)


if(TRUE){
#### Meristem
meristem <- subset(chillfrz, select=c("species", "chill", "meristem", "tx"))
meristem <- meristem[!duplicated(meristem),]
meristem <- na.omit(meristem)

cols <- colorRampPalette(brewer.pal(3,"Dark2"))(3)
meriplot <- ggplot(meristem, aes(x=tx, y=meristem, shape=as.factor(chill), colour=as.factor(chill), fill=as.factor(chill))) +
  geom_smooth(method="lm", se=FALSE) + geom_jitter(width=0.1, height=0.1) +
  geom_point() +
  theme_classic() + 
  theme(legend.position = "none") +
  ggtitle("f)") +
  xlab("") +
  ylab("Shoot apical meristem damage") +
  coord_cartesian(expand = TRUE) +
  #scale_y_continuous(breaks = c(0,1), labels=c("No damage", "Damage")) + 
  scale_x_continuous(breaks = c(0,1), labels=c("Control", "False Spring")) +
  scale_color_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                   "6 weeks",
                                                                   "8 weeks")) +
  scale_fill_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                  "6 weeks",
                                                                  "8 weeks")) +
  scale_shape_manual(name="Chill Treatment", values=c(15, 16, 17), labels=c("4 weeks",
                                                                            "6 weeks",
                                                                            "8 weeks"))

#ggsave(paste("figures/merisimple.png",sep=""),width=15, height=12,units="cm",bg = "white",dpi=500, plot=meriplot)
}

#### Total Biomass
totbiomass <- subset(chillfrz, select=c("species", "chill", "totbiomass", "tx"))
totbiomass <- totbiomass[!duplicated(totbiomass),]
totbiomass <- na.omit(totbiomass)

cols <- colorRampPalette(brewer.pal(3,"Dark2"))(3)
totbioplot <- ggplot(totbiomass, aes(x=tx, y=totbiomass, shape=as.factor(chill), colour=as.factor(chill), fill=as.factor(chill))) +
  geom_smooth(method="lm", se=FALSE) + geom_jitter(width=0.1) +
  geom_point() +
  theme_classic() + 
  theme(legend.position="none") +
  ggtitle("h)") +
  xlab("") +
  ylab("Total Biomass (g)") +
  coord_cartesian(expand = TRUE) +
  #scale_y_continuous(breaks = c(0,1), labels=c("No damage", "Damage")) + 
  scale_x_continuous(breaks = c(0,1), labels=c("Control", "False Spring")) +
  scale_color_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                   "6 weeks",
                                                                   "8 weeks")) +
  scale_fill_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                  "6 weeks",
                                                                  "8 weeks")) +
  scale_shape_manual(name="Chill Treatment", values=c(15, 16, 17), labels=c("4 weeks",
                                                                            "6 weeks",
                                                                            "8 weeks"))

#ggsave(paste("figures/totbiomasssimple.png",sep=""),width=15, height=12,units="cm",bg = "white",dpi=500, plot=totbioplot)

#ggplot(totbiomass, aes(x=as.character(chill), y=totbiomass)) + 
 # geom_boxplot()

#### Height
ht.final <- subset(chillfrz, select=c("species", "chill", "ht.final", "tx"))
ht.final <- ht.final[!duplicated(ht.final),]
ht.final <- na.omit(ht.final)

cols <- colorRampPalette(brewer.pal(3,"Dark2"))(3)
htfinalplot <- ggplot(ht.final, aes(x=tx, y=ht.final, shape=as.factor(chill), colour=as.factor(chill), fill=as.factor(chill))) +
  geom_smooth(method="lm", se=FALSE) + geom_jitter(width=0.1) +
  geom_point() +
  theme_classic() + 
  theme(legend.position = "none") +
  ggtitle("g)") +
  xlab("") +
  ylab("Total shoot growth (cm)") +
  coord_cartesian(expand = TRUE) +
  #scale_y_continuous(breaks = c(0,1), labels=c("No damage", "Damage")) + 
  scale_x_continuous(breaks = c(0,1), labels=c("Control", "False Spring")) +
  scale_color_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                   "6 weeks",
                                                                   "8 weeks")) +
  scale_fill_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                  "6 weeks",
                                                                  "8 weeks")) +
  scale_shape_manual(name="Chill Treatment", values=c(15, 16, 17), labels=c("4 weeks",
                                                                            "6 weeks",
                                                                            "8 weeks"))

#ggsave(paste("figures/htfinalsimple.png",sep=""),width=15, height=12,units="cm",bg = "white",dpi=500, plot=htfinalplot)

#### Toughness
tough <- subset(chillfrz, select=c("species", "chill", "tough", "tx"))
tough <- tough[!duplicated(tough),]
tough <- na.omit(tough)

cols <- colorRampPalette(brewer.pal(3,"Dark2"))(3)
toughplot <- ggplot(tough, aes(x=tx, y=tough, shape=as.factor(chill), colour=as.factor(chill), fill=as.factor(chill))) +
  geom_smooth(method="lm", se=FALSE) + geom_jitter(width=0.1) +
  geom_point() +
  theme_classic() + 
  theme(legend.position="none") +
  ggtitle("d)") +
  xlab("") +
  ylab("Leaf toughness (N)") +
  coord_cartesian(expand = TRUE) +
  #scale_y_continuous(breaks = c(0,1), labels=c("No damage", "Damage")) + 
  scale_x_continuous(breaks = c(0,1), labels=c("Control", "False Spring")) +
  scale_color_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                   "6 weeks",
                                                                   "8 weeks")) +
  scale_fill_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                  "6 weeks",
                                                                  "8 weeks")) +
  scale_shape_manual(name="Chill Treatment", values=c(15, 16, 17), labels=c("4 weeks",
                                                                            "6 weeks",
                                                                            "8 weeks"))

#ggsave(paste("figures/toughsimple.png",sep=""),width=15, height=12,units="cm",bg = "white",dpi=500, plot=toughplot)

#### thickness
thick <- subset(chillfrz, select=c("species", "chill", "thick", "tx"))
thick <- thick[!duplicated(thick),]
thick <- na.omit(thick)

cols <- colorRampPalette(brewer.pal(3,"Dark2"))(3)
thickplot <- ggplot(thick, aes(x=tx, y=thick, shape=as.factor(chill), colour=as.factor(chill), fill=as.factor(chill))) +
  geom_smooth(method="lm", se=FALSE) + geom_jitter(width=0.1) +
  geom_point() +
  theme_classic() + 
  theme(legend.position="none") +
  ggtitle("e)") +
  xlab("") +
  ylab(expression(paste("Leaf thickness (", mu, "m)", sep=""))) +
  coord_cartesian(expand = TRUE) +
  #scale_y_continuous(breaks = c(0,1), labels=c("No damage", "Damage")) + 
  scale_x_continuous(breaks = c(0,1), labels=c("Control", "False Spring")) +
  scale_color_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                   "6 weeks",
                                                                   "8 weeks")) +
  scale_fill_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                  "6 weeks",
                                                                  "8 weeks")) +
  scale_shape_manual(name="Chill Treatment", values=c(15, 16, 17), labels=c("4 weeks",
                                                                            "6 weeks",
                                                                            "8 weeks"))

#ggsave(paste("figures/thicksimple.png",sep=""),width=15, height=12,units="cm",bg = "white",dpi=500, plot=thickplot)

#### Growing season length
gslength <- subset(chillfrz, select=c("species", "chill", "gslength", "tx"))
gslength <- gslength[!duplicated(gslength),]
gslength <- na.omit(gslength)

cols <- colorRampPalette(brewer.pal(3,"Dark2"))(3)
gslengthplot <- ggplot(gslength, aes(x=tx, y=gslength, shape=as.factor(chill), colour=as.factor(chill), fill=as.factor(chill))) +
  geom_smooth(method="lm", se=FALSE) + geom_jitter(width=0.1) +
  geom_point() +
  theme_classic() + 
  theme(legend.position="none") +
  ggtitle("b)") +
  xlab("") +
  ylab("Growing season length (days)") +
  coord_cartesian(expand = TRUE) +
  #scale_y_continuous(breaks = c(0,1), labels=c("No damage", "Damage")) + 
  scale_x_continuous(breaks = c(0,1), labels=c("Control", "False Spring")) +
  scale_color_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                   "6 weeks",
                                                                   "8 weeks")) +
  scale_fill_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                  "6 weeks",
                                                                  "8 weeks")) +
  scale_shape_manual(name="Chill Treatment", values=c(15, 16, 17), labels=c("4 weeks",
                                                                            "6 weeks",
                                                                            "8 weeks"))

#ggsave(paste("figures/gslengthsimple.png",sep=""),width=15, height=12,units="cm",bg = "white",dpi=500, plot=gslengthplot)

#### Aboveground biomass
shoots <- subset(chillfrz, select=c("species", "chill", "shoots", "tx"))
shoots <- shoots[!duplicated(shoots),]
shoots <- na.omit(shoots)

cols <- colorRampPalette(brewer.pal(3,"Dark2"))(3)
shootsplot <- ggplot(shoots, aes(x=tx, y=shoots, shape=as.factor(chill), colour=as.factor(chill), fill=as.factor(chill))) +
  geom_smooth(method="lm", se=FALSE) + geom_jitter(width=0.1) +
  geom_point() +
  theme_classic() + 
  xlab("Treatment") +
  ylab("Aboveground biomass (g)") +
  coord_cartesian(expand = TRUE) +
  #scale_y_continuous(breaks = c(0,1), labels=c("No damage", "Damage")) + 
  scale_x_continuous(breaks = c(0,1), labels=c("Control", "False Spring")) +
  scale_color_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                   "6 weeks",
                                                                   "8 weeks")) +
  scale_fill_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                  "6 weeks",
                                                                  "8 weeks")) +
  scale_shape_manual(name="Chill Treatment", values=c(15, 16, 17), labels=c("4 weeks",
                                                                            "6 weeks",
                                                                            "8 weeks"))

#ggsave(paste("figures/shootssimple.png",sep=""),width=15, height=12,units="cm",bg = "white",dpi=500, plot=shootsplot)

#### belowground biomass
roots <- subset(chillfrz, select=c("species", "chill", "roots", "tx"))
roots <- roots[!duplicated(roots),]
roots <- na.omit(roots)

cols <- colorRampPalette(brewer.pal(3,"Dark2"))(3)
rootsplot <- ggplot(roots, aes(x=tx, y=roots, shape=as.factor(chill), colour=as.factor(chill), fill=as.factor(chill))) +
  geom_smooth(method="lm", se=FALSE) + geom_jitter(width=0.1) +
  geom_point() +
  theme_classic() + 
  xlab("Treatment") +
  ylab("Belowground biomass (g)") +
  coord_cartesian(expand = TRUE) +
  #scale_y_continuous(breaks = c(0,1), labels=c("No damage", "Damage")) + 
  scale_x_continuous(breaks = c(0,1), labels=c("Control", "False Spring")) +
  scale_color_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                   "6 weeks",
                                                                   "8 weeks")) +
  scale_fill_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                  "6 weeks",
                                                                  "8 weeks")) +
  scale_shape_manual(name="Chill Treatment", values=c(15, 16, 17), labels=c("4 weeks",
                                                                            "6 weeks",
                                                                            "8 weeks"))

#ggsave(paste("figures/rootssimple.png",sep=""),width=15, height=12,units="cm",bg = "white",dpi=500, plot=rootsplot)

#### Roots to shoots ratio
chillfrz$rootstoshoots <- chillfrz$roots/chillfrz$shoots
rootstoshoots <- subset(chillfrz, select=c("species", "chill", "rootstoshoots", "tx"))
rootstoshoots <- rootstoshoots[!duplicated(rootstoshoots),]
rootstoshoots <- na.omit(rootstoshoots)

cols <- colorRampPalette(brewer.pal(3,"Dark2"))(3)
rootstoshootsplot <- ggplot(rootstoshoots, aes(x=tx, y=rootstoshoots, shape=as.factor(chill), colour=as.factor(chill), fill=as.factor(chill))) +
  geom_smooth(method="lm", se=FALSE) + geom_jitter(width=0.1) +
  geom_point() +
  theme_classic() + 
  theme(legend.position="none") +
  ggtitle("i)") +
  xlab("") +
  ylab("Root Biomass to \nShoot Biomass ratio (g)") +
  coord_cartesian(expand = TRUE) +
  #scale_y_continuous(breaks = c(0,1), labels=c("No damage", "Damage")) + 
  scale_x_continuous(breaks = c(0,1), labels=c("Control", "False Spring")) +
  scale_color_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                   "6 weeks",
                                                                   "8 weeks")) +
  scale_fill_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                  "6 weeks",
                                                                  "8 weeks")) +
  scale_shape_manual(name="Chill Treatment", values=c(15, 16, 17), labels=c("4 weeks",
                                                                            "6 weeks",
                                                                            "8 weeks"))

#ggsave(paste("figures/rootstoshootssimple.png",sep=""),width=15, height=12,units="cm",bg = "white",dpi=500, plot=rootstoshootsplot)


#### Height 60 days
ht.diff <- subset(chillfrz, select=c("species", "chill", "ht.diff", "tx"))
ht.diff <- ht.diff[!duplicated(ht.diff),]
ht.diff <- na.omit(ht.diff)

cols <- colorRampPalette(brewer.pal(3,"Dark2"))(3)
htdiffplot <- ggplot(ht.diff, aes(x=tx, y=ht.diff, shape=as.factor(chill), colour=as.factor(chill), fill=as.factor(chill))) +
  geom_smooth(method="lm", se=FALSE) + geom_jitter(width=0.1) +
  geom_point() +
  theme_classic() + 
  theme(legend.position="none") +
  ggtitle("g)") +
  xlab("Treatment") +
  ylab("Shoot growth 60 days after budburst (cm)") +
  coord_cartesian(expand = TRUE) +
  #scale_y_continuous(breaks = c(0,1), labels=c("No damage", "Damage")) + 
  scale_x_continuous(breaks = c(0,1), labels=c("Control", "False Spring")) +
  scale_color_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                   "6 weeks",
                                                                   "8 weeks")) +
  scale_fill_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                  "6 weeks",
                                                                  "8 weeks")) +
  scale_shape_manual(name="Chill Treatment", values=c(15, 16, 17), labels=c("4 weeks",
                                                                            "6 weeks",
                                                                            "8 weeks"))

#ggsave(paste("figures/htmidsimple.png",sep=""),width=15, height=12,units="cm",bg = "white",dpi=500, plot=htdiffplot)

#### Chlorophyll
chlavg <- subset(chillfrz, select=c("species", "chill", "chlavg", "tx"))
chlavg <- chlavg[!duplicated(chlavg),]
chlavg <- na.omit(chlavg)
chlavg <- chlavg[!(chlavg$chlavg=="#DIV/0!"),]
chlavg$chlavg <- as.numeric(chlavg$chlavg)

cols <- colorRampPalette(brewer.pal(3,"Dark2"))(3)
chlplot <- ggplot(chlavg, aes(x=tx, y=chlavg, shape=as.factor(chill), colour=as.factor(chill), fill=as.factor(chill))) +
  geom_smooth(method="lm", se=FALSE) + geom_jitter(width=0.1) +
  geom_point() +
  theme_classic() + 
  theme(legend.position="none") +
  xlab("Treatment") +
  ggtitle("c)") +
  ylab("Chlorophyll content (mg/cm^2)") +
  coord_cartesian(expand = TRUE) +
  #scale_y_continuous(breaks = c(0,1), labels=c("No damage", "Damage")) + 
  scale_x_continuous(breaks = c(0,1), labels=c("Control", "False Spring")) +
  scale_color_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                   "6 weeks",
                                                                   "8 weeks")) +
  scale_fill_manual(name="Chill Treatment", values=cols, labels=c("4 weeks",
                                                                  "6 weeks",
                                                                  "8 weeks")) +
  scale_shape_manual(name="Chill Treatment", values=c(15, 16, 17), labels=c("4 weeks",
                                                                            "6 weeks",
                                                                            "8 weeks"))

#ggsave(paste("figures/chlavgsimple.png",sep=""),width=15, height=12,units="cm",bg = "white",dpi=500, plot=chlplot)


quartz()
allmsmts <- grid.arrange(dvrplot, gslengthplot, mylegend,
             chlplot, toughplot, thickplot,
             meriplot, htdiffplot, totbioplot, ncol=3, widths=c(1,1,1))

ggsave(paste("figures/allmsmts.png",sep=""),width=25, height=25,units="cm",bg = "white",dpi=900, plot=allmsmts)
