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

fourweeks <- subset(chill.stan, chill.stan$chill==1)
fourweeks.meas <- fourweeks[!is.na(fourweeks$ht.diff),]
#cols <- colorRampPalette(brewer.pal(8,"Set2"))(8)
height<- ggplot(fourweeks.meas, aes(x=factor(species, levels = species_order), y=ht.diff, alpha=tx)) + geom_boxplot(aes(alpha=as.factor(tx), fill=as.factor(species), col=as.factor(species)), outlier.shape=NA) +
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        axis.title.x = element_blank(),
        legend.position = "none") + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Height Difference (cm)") +
  scale_alpha_manual(name="Treatment", values=c(0.2, 0.7),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_fill_manual(name="Species", values=values,
                    labels=labs) + 
  scale_color_manual(name="Species", values=values,
                     labels=labs) + scale_x_discrete(labels=labs) +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)

chlorophyll<- ggplot(fourweeks.meas, aes(x=factor(species, levels = species_order), y=mg.cm2, alpha=tx)) + geom_boxplot(aes(alpha=as.factor(tx), fill=as.factor(species), col=as.factor(species)), outlier.shape=NA) +
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        axis.title.x = element_blank()) + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Chlorophyll Content\n (mg/cm2)") +
  scale_alpha_manual(name="Treatment", values=c(0.2, 0.7),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_fill_manual(name="Species", values=values,
                    labels=labs) + 
  scale_color_manual(name="Species", values=values,
                     labels=labs) + scale_x_discrete(labels=labs) +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)

sixweeks <- subset(chill.stan, chill.stan$chill==2)
#sixweeks <- sixweeks[!is.na(sixweeks$ht.diff),]

height.six<- ggplot(sixweeks, aes(x=factor(species, levels = species_order), y=ht.diff, alpha=tx)) + geom_boxplot(aes(alpha=as.factor(tx), fill=as.factor(species), col=as.factor(species)), outlier.shape=NA) +
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        axis.title.x = element_blank(),
        legend.position = "none") + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Height Difference (cm)") +
  scale_alpha_manual(name="Treatment", values=c(0.2, 0.7),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_fill_manual(name="Species", values=values,
                    labels=labs) + 
  scale_color_manual(name="Species", values=values,
                     labels=labs) + scale_x_discrete(labels=labs) +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)

chlorophyll.six<- ggplot(sixweeks, aes(x=factor(species, levels = species_order), y=mg.cm2, alpha=tx)) + geom_boxplot(aes(alpha=as.factor(tx), fill=as.factor(species), col=as.factor(species)), outlier.shape=NA) +
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        axis.title.x = element_blank()) + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Chlorophyll Content\n (mg/cm2)") +
  scale_alpha_manual(name="Treatment", values=c(0.2, 0.7),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_fill_manual(name="Species", values=values,
                    labels=labs) + 
  scale_color_manual(name="Species", values=values,
                     labels=labs) + scale_x_discrete(labels=labs) +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)




quartz()
ggarrange(height, chlorophyll, ncol=2)
ggarrange(height.six, chlorophyll.six, ncol=2)


#### More plots on DVR
#fourweeks.dvr <- subset(chill.stan, chill.stan$chill==1)


dvr<- ggplot(fourweeks, aes(x=factor(species, levels = species_order), y=dvr, alpha=tx)) + geom_boxplot(aes(alpha=as.factor(tx), fill=as.factor(species), col=as.factor(species))) +
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, 
        legend.key = element_rect(colour = "transparent", fill = "white"),
        axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
        legend.position = "none") + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 50)) +
  ylab("Duration of \nVegetative Risk") +
  scale_alpha_manual(name="Treatment", values=c(0.2, 0.7),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_fill_manual(name="Species", values=values,
                    labels=labs) + 
  scale_color_manual(name="Species", values=values,
                     labels=labs) + scale_x_discrete(labels=labs) +
  annotate("text", x = 8, y = 47, label = "4 Weeks of Chilling", family="Helvetica", size=3, fontface="bold") +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)

#sixweeks.dvr <- subset(chill.stan, chill.stan$chill==2)
dvr.six<- ggplot(sixweeks, aes(x=factor(species, levels = species_order), y=dvr, alpha=tx)) + geom_boxplot(aes(alpha=as.factor(tx), fill=as.factor(species), col=as.factor(species))) +
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white"),
        axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 50)) +
  ylab("Duration of \nVegetative Risk") +
  scale_alpha_manual(name="Treatment", values=c(0.2, 0.7),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_fill_manual(name="Species", values=values,
                    labels=labs) + 
  scale_color_manual(name="Species", values=values,
                     labels=labs) + scale_x_discrete(labels=labs) +
  annotate("text", x = 8, y = 47, label = "6 Weeks of Chilling", family="Helvetica", size=3, fontface="bold") +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)


eightweeks <- subset(chill.stan, chill.stan$chill==3)
dvr.eight<- ggplot(eightweeks, aes(x=factor(species, levels = species_order), y=dvr, alpha=tx)) + geom_boxplot(aes(alpha=as.factor(tx), fill=as.factor(species), col=as.factor(species))) +
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        axis.title.x = element_blank(),
        legend.position = "none") + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 50)) +
  ylab("Duration of \nVegetative Risk") +
  scale_alpha_manual(name="Treatment", values=c(0.2, 0.7),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_fill_manual(name="Species", values=values,
                    labels=labs) + 
  scale_color_manual(name="Species", values=values,
                     labels=labs) + scale_x_discrete(labels=labs) +
  annotate("text", x = 8, y = 47, label = "8 Weeks of Chilling", family="Helvetica", size=3, fontface="bold") +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)






quartz()
ggarrange(dvr, dvr.six, dvr.eight, nrow=3)


scatter <- ggplot(fourweeks, aes(x=budburst, y=dvr)) + geom_point(aes(col=as.factor(tx))) +
  theme_classic() + geom_smooth(aes(col=as.factor(tx), fill=as.factor(tx)), method="lm") +
  scale_y_continuous(expand = c(0, 0)) + theme(legend.position = "none") +
  ylab("Duration of Vegetative Risk") + xlab("Day of Budburst") +
  scale_alpha_manual(name="Treatment", values=c(0.2, 0.7),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_color_manual(name="Treatment", values=c("grey", "royalblue"),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_fill_manual(name="Treatment", values=c("grey", "royalblue"),
                   labels=c("0"="Control", "1"="False Spring")) +
  coord_cartesian(xlim=c(0,80), ylim=c(0, 60), expand=c(0,0)) 

scatter.six <- ggplot(sixweeks, aes(x=budburst, y=dvr)) + geom_point(aes(col=as.factor(tx))) +
  theme_classic() + geom_smooth(aes(col=as.factor(tx), fill=as.factor(tx)), method="lm") +
  scale_y_continuous(expand = c(0, 0)) + theme(legend.position = "none") +
  ylab("Duration of Vegetative Risk") + xlab("Day of Budburst") +
  scale_alpha_manual(name="Treatment", values=c(0.2, 0.7),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_color_manual(name="Treatment", values=c("grey", "royalblue"),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_fill_manual(name="Treatment", values=c("grey", "royalblue"),
                    labels=c("0"="Control", "1"="False Spring"))+
  coord_cartesian(xlim=c(0,80), ylim=c(0, 60), expand=c(0,0)) 

scatter.eight <- ggplot(eightweeks, aes(x=budburst, y=dvr)) + geom_point(aes(col=as.factor(tx))) +
  theme_classic() + geom_smooth(aes(col=as.factor(tx), fill=as.factor(tx)), method="lm") +
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Duration of Vegetative Risk") + xlab("Day of Budburst") +
  scale_alpha_manual(name="Treatment", values=c(0.2, 0.7),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_color_manual(name="Treatment", values=c("grey", "royalblue"),
                     labels=c("0"="Control", "1"="False Spring")) +
  scale_fill_manual(name="Treatment", values=c("grey", "royalblue"),
                    labels=c("0"="Control", "1"="False Spring"))+
  coord_cartesian(xlim=c(0,80), ylim=c(0, 60), expand=c(0,0)) 

quartz()
ggarrange(scatter, scatter.six, scatter.eight, ncol=3)
