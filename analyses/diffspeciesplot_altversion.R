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

chillfrz$bbtype <- NA
chillfrz$bbtype <- ifelse(chillfrz$species%in%c("SALPUR", "BETPAP", "CORRAC"), "early", chillfrz$bbtype)
chillfrz$bbtype <- ifelse(chillfrz$species%in%c("BETPOP", "ALNRUG"), "mid", chillfrz$bbtype)
chillfrz$bbtype <- ifelse(chillfrz$species%in%c("VIBDEN", "ACESAC", "SORAME"), "late", chillfrz$bbtype)


dvr <- subset(chillfrz, select=c("species", "chill", "dvr", "tx", "bbtype"))
dvr <- dvr[!duplicated(dvr),]

chilldvrfunc <- function (x) {
  
  exp <- dvr[(dvr$chill==x),]
  exp <- na.omit(exp)
  
  cont <- exp[(exp$tx==0),]
  frz <- exp[(exp$tx==1),]
  cont$dvrcontmean <- ave(cont$dvr, cont$species)
  cont$dvrcontsd<-ave(cont$dvr, cont$species, cont$tx, FUN=sd)/sqrt(length(unique(cont$dvrcontmean)))
  frz$dvrfrzmean <- ave(frz$dvr, frz$species)
  frz$dvrfrzsd<-ave(frz$dvr, frz$species, frz$tx, FUN=sd)/sqrt(length(unique(frz$dvrfrzmean)))
  
  cont <- subset(cont , select=c("species", "dvrcontmean", "dvrcontsd"))
  cont <- cont[!duplicated(cont),]
  frz <- subset(frz , select=c("species", "dvrfrzmean", "dvrfrzsd"))
  frz <- frz[!duplicated(frz),]
  
  frzdiff <- full_join(cont, frz)
  
  frzdiff$diff<-frzdiff$dvrfrzmean-frzdiff$dvrcontmean
  frzdiff$diff.sd<-frzdiff$dvrfrzsd-frzdiff$dvrcontsd
  
  frzdiff$xmin <- frzdiff$dvrcontmean-frzdiff$dvrcontsd
  frzdiff$xmax <- frzdiff$dvrcontmean+frzdiff$dvrcontsd
  frzdiff$ymin <- frzdiff$dvrfrzmean-frzdiff$dvrfrzsd
  frzdiff$ymax <- frzdiff$dvrfrzmean+frzdiff$dvrfrzsd
  
  mround <- function(x,base){ 
    base*round(x/base) 
  }
  
  frzdiff$diff.labels <- mround(frzdiff$diff, 0.5)
  
  frzdiff$species.name <- NA
  frzdiff$species.name <- ifelse(frzdiff$species=="ACESAC", "Acer saccharinum", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="ALNRUG", "Alnus rugosa", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="BETPAP", "Betula papyrifera", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="BETPOP", "Betula populifolia", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="CORRAC", "Cornus racemosa", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="SALPUR", "Salix purpurea", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="SORAME", "Sorbus americana", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="VIBDEN", "Viburnum dentatum", frzdiff$species.name)
  
  frzdiff$bbtype <- NA
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("SALPUR", "BETPAP", "CORRAC"), "early", frzdiff$bbtype)
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("BETPOP", "ALNRUG"), "mid", frzdiff$bbtype)
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("VIBDEN", "ACESAC", "SORAME"), "late", frzdiff$bbtype)
  
  return(frzdiff)
  
}

chill4 <- chilldvrfunc(1)
chill6 <- chilldvrfunc(2)
chill8 <- chilldvrfunc(3)

#tt <- full_join(chill4, chill6)
#tt <- full_join(tt, chill8)
#valsize <- c(1:13)
#sizes <- sort(unique(tt$diff.labels))


cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)

dvr4 <- ggplot(chill4, aes(x=dvrcontmean, y=dvrfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=dvrcontmean, y=dvrfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.825, 0.125),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=7),
        legend.text = element_text(size=6),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.key.size = unit(0.3, "cm")) +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Duration of vegetative risk (control)") + 
  ylab("Duration of vegetative \nrisk (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chill4$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
                    #labels=sizes) +
  #scale_size_continuous(name=expression(Delta*" in false spring risk")) + 
  scale_y_continuous(breaks=seq(8,28,2)) +
  scale_x_continuous(breaks=seq(8,28,2)) +
  coord_cartesian(xlim=c(8,28), ylim=c(8,28), expand=TRUE) + #guides(col=FALSE) +
  ggtitle("A. Four weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")

dvr6 <- ggplot(chill6, aes(x=dvrcontmean, y=dvrfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=dvrcontmean, y=dvrfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = "none",
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Duration of vegetative risk (control)") + 
  ylab("Duration of vegetative \nrisk (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chill6$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
   #                 labels=sizes) +
  #scale_size_continuous(name=expression(Delta*" in false spring risk")) + 
  scale_y_continuous(breaks=seq(8,28,2)) +
  scale_x_continuous(breaks=seq(8,28,2)) +
  coord_cartesian(xlim=c(8,28), ylim=c(8,28), expand=TRUE) + #guides(col=FALSE) +
  ggtitle("B. Six weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")

dvr8 <- ggplot(chill8, aes(x=dvrcontmean, y=dvrfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=dvrcontmean, y=dvrfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position="none",
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.box="horizontal") +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Duration of vegetative risk (control)") + 
  ylab("Duration of vegetative \nrisk (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chill8$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
   #                 labels=sizes) +
  scale_y_continuous(breaks=seq(8,28,2)) +
  scale_x_continuous(breaks=seq(8,28,2)) +
  coord_cartesian(xlim=c(8,28), ylim=c(8,28), expand=TRUE) +
  ggtitle("C. Eight weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")

if(FALSE){
sizelegend <- ggplot(aes(x=dvrcontmean, y=dvrfrzmean, col=species.name, size=as.factor(diff.labels)), data=tt) + 
  geom_point(shape=19) +
  scale_size_manual(name=expression("Change in duration of vegetative risk (days)"), values=valsize,
                    labels=sizes, guide = guide_legend(title.position = "top", nrow=1, 
                                                       override.aes = list(shape = 21),
                                                       label.theme = element_text(size=7))) +
  scale_color_manual(name="Species", values=cols, labels=tt$species.name,
                     guide = guide_legend(title.position = "top", nrow=1,
                                          override.aes = list(size = 3),
                                          label.theme = element_text(face = "italic", size=7)))+
  theme(legend.position = "bottom", legend.box="vertical",
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.box.just = "left",
        legend.justification = c("center", "bottom"),
        legend.box.background = element_rect(),
        legend.title = element_text(size=9),
        legend.text.align = 0,
        legend.spacing.y = unit(-0.05, "cm"))  

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(sizelegend)

quartz()
dvrplot<-grid.arrange(dvr4, dvr6, dvr8, mylegend, nrow=4, heights = c(4, 0.55, 0.5, 0.55), 
                      layout_matrix=rbind(c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3),
                                          c(NA),
                                          c(NA, 4, 4, 4, NA),
                                          c(NA)))
}
quartz()
dvrplot<-grid.arrange(dvr4, dvr6, dvr8, ncol=3)



ggsave("figures/dvrspeciesdiff_bbdiffs.png",width=30, height=14,units="cm",bg = "white",dpi=500, plot=dvrplot)


##### Now for growing season length...
gslength <- subset(chillfrz, select=c("species", "chill", "gslength", "tx"))
gslength <- gslength[!duplicated(gslength),]

chillgslengthfunc <- function (x) {
  
  exp <- gslength[(gslength$chill==x),]
  exp <- na.omit(exp)
  
  cont <- exp[(exp$tx==0),]
  frz <- exp[(exp$tx==1),]
  cont$gslengthcontmean <- ave(cont$gslength, cont$species)
  cont$gslengthcontsd<-ave(cont$gslength, cont$species, cont$tx, FUN=sd)/sqrt(length(unique(cont$gslengthcontmean)))
  frz$gslengthfrzmean <- ave(frz$gslength, frz$species)
  frz$gslengthfrzsd<-ave(frz$gslength, frz$species, frz$tx, FUN=sd)/sqrt(length(unique(frz$gslengthfrzmean)))
  
  cont <- subset(cont , select=c("species", "gslengthcontmean", "gslengthcontsd"))
  cont <- cont[!duplicated(cont),]
  frz <- subset(frz , select=c("species", "gslengthfrzmean", "gslengthfrzsd"))
  frz <- frz[!duplicated(frz),]
  
  frzdiff <- full_join(cont, frz)
  
  frzdiff$diff<-frzdiff$gslengthfrzmean-frzdiff$gslengthcontmean
  frzdiff$diff.sd<-frzdiff$gslengthfrzsd-frzdiff$gslengthcontsd
  
  frzdiff$xmin <- frzdiff$gslengthcontmean-frzdiff$gslengthcontsd
  frzdiff$xmax <- frzdiff$gslengthcontmean+frzdiff$gslengthcontsd
  frzdiff$ymin <- frzdiff$gslengthfrzmean-frzdiff$gslengthfrzsd
  frzdiff$ymax <- frzdiff$gslengthfrzmean+frzdiff$gslengthfrzsd
  
  mround <- function(x,base){ 
    base*round(x/base) 
  }
  
  frzdiff$diff.labels <- mround(frzdiff$diff, 0.5)
  
  frzdiff$species.name <- NA
  frzdiff$species.name <- ifelse(frzdiff$species=="ACESAC", "Acer saccharinum", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="ALNRUG", "Alnus rugosa", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="BETPAP", "Betula papyrifera", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="BETPOP", "Betula populifolia", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="CORRAC", "Cornus racemosa", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="SALPUR", "Salix purpurea", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="SORAME", "Sorbus americana", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="VIBDEN", "Viburnum dentatum", frzdiff$species.name)
  
  frzdiff$bbtype <- NA
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("SALPUR", "BETPAP", "CORRAC"), "early", frzdiff$bbtype)
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("BETPOP", "ALNRUG"), "mid", frzdiff$bbtype)
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("VIBDEN", "ACESAC", "SORAME"), "late", frzdiff$bbtype)
  
  return(frzdiff)
  
}

chillgslength4 <- chillgslengthfunc(1)
chillgslength6 <- chillgslengthfunc(2)
chillgslength8 <- chillgslengthfunc(3)

#tt <- full_join(chillgslength4, chillgslength6)
#tt <- full_join(tt, chillgslength8)
#valsize <- c(1:13)
#sizes <- sort(unique(tt$diff.labels))


cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)

gslength4 <- ggplot(chillgslength4, aes(x=gslengthcontmean, y=gslengthfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=gslengthcontmean, y=gslengthfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
              legend.position = c(0.825, .125),
              legend.background = element_blank(),
              legend.box.background = element_rect(colour = "black"),
              legend.title = element_text(size=7),
              legend.text = element_text(size=6),
              legend.key = element_rect(colour = "transparent", fill = "white"),
              legend.key.size = unit(0.3, "cm")) +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Growing season length (control)") + 
  ylab("Growing season length (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chillgslength4$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
   #                 labels=sizes) +
  #scale_size_continuous(name=expression(Delta*" in false spring risk")) + 
  scale_y_continuous(breaks=seq(185,335,20)) +
  scale_x_continuous(breaks=seq(185,335,20)) +
  coord_cartesian(xlim=c(185,335), ylim=c(185,335), expand=TRUE) + #guides(col=FALSE) +
  ggtitle("A. Four weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")

gslength6 <- ggplot(chillgslength6, aes(x=gslengthcontmean, y=gslengthfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=gslengthcontmean, y=gslengthfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = "none",
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Growing season length (control)") + 
  ylab("Growing season length (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chill6$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
   #                 labels=sizes) +
  #scale_size_continuous(name=expression(Delta*" in false spring risk")) + 
  scale_y_continuous(breaks=seq(185,335,20)) +
  scale_x_continuous(breaks=seq(185,335,20)) +
  coord_cartesian(xlim=c(185,335), ylim=c(185,335), expand=TRUE) + guides(col=FALSE) +
  ggtitle("B. Six weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")

gslength8 <- ggplot(chillgslength8, aes(x=gslengthcontmean, y=gslengthfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=gslengthcontmean, y=gslengthfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position="none",
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.box="horizontal") +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Growing season length (control)") + 
  ylab("Growing season length (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chill8$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
   #                 labels=sizes) +
  scale_y_continuous(breaks=seq(185,335,20)) +
  scale_x_continuous(breaks=seq(185,335,20)) +
  coord_cartesian(xlim=c(185,335), ylim=c(185,335), expand=TRUE) + guides(col=FALSE) +
  ggtitle("C. Eight weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")


quartz()
gslengthplot<-grid.arrange(gslength4, gslength6, gslength8, ncol=3)

ggsave("figures/gslengthspeciesdiff_bbdiff.png",width=30, height=14,units="cm",bg = "white",dpi=500, plot=gslengthplot)





############# Now for total biomass...
totbiomass <- subset(chillfrz, select=c("species", "chill", "totbiomass", "tx"))
totbiomass <- totbiomass[!duplicated(totbiomass),]

chilltotbiomassfunc <- function (x) {
  
  exp <- totbiomass[(totbiomass$chill==x),]
  exp <- na.omit(exp)
  
  cont <- exp[(exp$tx==0),]
  frz <- exp[(exp$tx==1),]
  cont$totbiomasscontmean <- ave(cont$totbiomass, cont$species)
  cont$totbiomasscontsd<-ave(cont$totbiomass, cont$species, cont$tx, FUN=sd)/sqrt(length(unique(cont$totbiomasscontmean)))
  frz$totbiomassfrzmean <- ave(frz$totbiomass, frz$species)
  frz$totbiomassfrzsd<-ave(frz$totbiomass, frz$species, frz$tx, FUN=sd)/sqrt(length(unique(frz$totbiomassfrzmean)))
  
  cont <- subset(cont , select=c("species", "totbiomasscontmean", "totbiomasscontsd"))
  cont <- cont[!duplicated(cont),]
  frz <- subset(frz , select=c("species", "totbiomassfrzmean", "totbiomassfrzsd"))
  frz <- frz[!duplicated(frz),]
  
  frzdiff <- full_join(cont, frz)
  
  frzdiff$diff<-frzdiff$totbiomassfrzmean-frzdiff$totbiomasscontmean
  frzdiff$diff.sd<-frzdiff$totbiomassfrzsd-frzdiff$totbiomasscontsd
  
  frzdiff$xmin <- frzdiff$totbiomasscontmean-frzdiff$totbiomasscontsd
  frzdiff$xmax <- frzdiff$totbiomasscontmean+frzdiff$totbiomasscontsd
  frzdiff$ymin <- frzdiff$totbiomassfrzmean-frzdiff$totbiomassfrzsd
  frzdiff$ymax <- frzdiff$totbiomassfrzmean+frzdiff$totbiomassfrzsd
  
  mround <- function(x,base){ 
    base*round(x/base) 
  }
  
  frzdiff$diff.labels <- mround(frzdiff$diff, 0.5)
  
  frzdiff$species.name <- NA
  frzdiff$species.name <- ifelse(frzdiff$species=="ACESAC", "Acer saccharinum", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="ALNRUG", "Alnus rugosa", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="BETPAP", "Betula papyrifera", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="BETPOP", "Betula populifolia", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="CORRAC", "Cornus racemosa", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="SALPUR", "Salix purpurea", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="SORAME", "Sorbus americana", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="VIBDEN", "Viburnum dentatum", frzdiff$species.name)
  
  frzdiff$bbtype <- NA
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("SALPUR", "BETPAP", "CORRAC"), "early", frzdiff$bbtype)
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("BETPOP", "ALNRUG"), "mid", frzdiff$bbtype)
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("VIBDEN", "ACESAC", "SORAME"), "late", frzdiff$bbtype)
  
  return(frzdiff)
  
}

chilltotbiomass4 <- chilltotbiomassfunc(1)
chilltotbiomass6 <- chilltotbiomassfunc(2)
chilltotbiomass8 <- chilltotbiomassfunc(3)

#tt <- full_join(chilltotbiomass4, chilltotbiomass6)
#tt <- full_join(tt, chilltotbiomass8)
#valsize <- c(1:13)
#sizes <- sort(unique(tt$diff.labels))


cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)

totbiomass4 <- ggplot(chilltotbiomass4, aes(x=totbiomasscontmean, y=totbiomassfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=totbiomasscontmean, y=totbiomassfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.825, .125),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=7),
        legend.text = element_text(size=6),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.key.size = unit(0.3, "cm")) +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Total Biomass (control)") + 
  ylab("Total Biomass (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chilltotbiomass4$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
   #                 labels=sizes) +
  #scale_size_continuous(name=expression(Delta*" in false spring risk")) + 
  scale_y_continuous(breaks=seq(0,120,20)) +
  scale_x_continuous(breaks=seq(0,120,20)) +
  coord_cartesian(xlim=c(0,120), ylim=c(0,120), expand=TRUE) + #guides(col=FALSE) +
  ggtitle("A. Four weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")

totbiomass6 <- ggplot(chilltotbiomass6, aes(x=totbiomasscontmean, y=totbiomassfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=totbiomasscontmean, y=totbiomassfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = "none",
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Total Biomass (control)") + 
  ylab("Total Biomass (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chilltotbiomass6$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
   #                 labels=sizes) +
  #scale_size_continuous(name=expression(Delta*" in false spring risk")) + 
  scale_y_continuous(breaks=seq(0,120,20)) +
  scale_x_continuous(breaks=seq(0,120,20)) +
  coord_cartesian(xlim=c(0,120), ylim=c(0,120), expand=TRUE) + guides(col=FALSE) +
  ggtitle("B. Six weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")

totbiomass8 <- ggplot(chilltotbiomass8, aes(x=totbiomasscontmean, y=totbiomassfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=totbiomasscontmean, y=totbiomassfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position="none",
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.box="horizontal") +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Total Biomass (control)") + 
  ylab("Total Biomass (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chilltotbiomass8$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
   #                 labels=sizes) +
  scale_y_continuous(breaks=seq(0,120,20)) +
  scale_x_continuous(breaks=seq(0,120,20)) +
  coord_cartesian(xlim=c(0,120), ylim=c(0,120), expand=TRUE) + guides(col=FALSE) +
  ggtitle("C. Eight weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")


quartz()
totbiomassplot<-grid.arrange(totbiomass4, totbiomass6, totbiomass8, ncol=3)

ggsave("figures/totbiomassspeciesdiff_bbdiff.png",width=30, height=14,units="cm",bg = "white",dpi=500, plot=totbiomassplot)


############# Now for shoots...
shoots <- subset(chillfrz, select=c("species", "chill", "shoots", "tx"))
shoots <- shoots[!duplicated(shoots),]

chillshootsfunc <- function (x) {
  
  exp <- shoots[(shoots$chill==x),]
  exp <- na.omit(exp)
  
  cont <- exp[(exp$tx==0),]
  frz <- exp[(exp$tx==1),]
  cont$shootscontmean <- ave(cont$shoots, cont$species)
  cont$shootscontsd<-ave(cont$shoots, cont$species, cont$tx, FUN=sd)/sqrt(length(unique(cont$shootscontmean)))
  frz$shootsfrzmean <- ave(frz$shoots, frz$species)
  frz$shootsfrzsd<-ave(frz$shoots, frz$species, frz$tx, FUN=sd)/sqrt(length(unique(frz$shootsfrzmean)))
  
  cont <- subset(cont , select=c("species", "shootscontmean", "shootscontsd"))
  cont <- cont[!duplicated(cont),]
  frz <- subset(frz , select=c("species", "shootsfrzmean", "shootsfrzsd"))
  frz <- frz[!duplicated(frz),]
  
  frzdiff <- full_join(cont, frz)
  
  frzdiff$diff<-frzdiff$shootsfrzmean-frzdiff$shootscontmean
  frzdiff$diff.sd<-frzdiff$shootsfrzsd-frzdiff$shootscontsd
  
  frzdiff$xmin <- frzdiff$shootscontmean-frzdiff$shootscontsd
  frzdiff$xmax <- frzdiff$shootscontmean+frzdiff$shootscontsd
  frzdiff$ymin <- frzdiff$shootsfrzmean-frzdiff$shootsfrzsd
  frzdiff$ymax <- frzdiff$shootsfrzmean+frzdiff$shootsfrzsd
  
  mround <- function(x,base){ 
    base*round(x/base) 
  }
  
  frzdiff$diff.labels <- mround(frzdiff$diff, 0.5)
  
  frzdiff$species.name <- NA
  frzdiff$species.name <- ifelse(frzdiff$species=="ACESAC", "Acer saccharinum", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="ALNRUG", "Alnus rugosa", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="BETPAP", "Betula papyrifera", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="BETPOP", "Betula populifolia", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="CORRAC", "Cornus racemosa", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="SALPUR", "Salix purpurea", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="SORAME", "Sorbus americana", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="VIBDEN", "Viburnum dentatum", frzdiff$species.name)
  
  frzdiff$bbtype <- NA
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("SALPUR", "BETPAP", "CORRAC"), "early", frzdiff$bbtype)
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("BETPOP", "ALNRUG"), "mid", frzdiff$bbtype)
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("VIBDEN", "ACESAC", "SORAME"), "late", frzdiff$bbtype)
  
  return(frzdiff)
  
}

chillshoots4 <- chillshootsfunc(1)
chillshoots6 <- chillshootsfunc(2)
chillshoots8 <- chillshootsfunc(3)

#tt <- full_join(chillshoots4, chillshoots6)
#tt <- full_join(tt, chillshoots8)
#valsize <- c(1:13)
#sizes <- sort(unique(tt$diff.labels))


cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)

shoots4 <- ggplot(chillshoots4, aes(x=shootscontmean, y=shootsfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=shootscontmean, y=shootsfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.825, .125),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=7),
        legend.text = element_text(size=6),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.key.size = unit(0.3, "cm")) +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Aboveground Biomass (control)") + 
  ylab("Aboveground Biomass (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chillshoots4$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
  #                 labels=sizes) +
  #scale_size_continuous(name=expression(Delta*" in false spring risk")) + 
  scale_y_continuous(breaks=seq(0,65,10)) +
  scale_x_continuous(breaks=seq(0,65,10)) +
  coord_cartesian(xlim=c(0,65), ylim=c(0, 65), expand=TRUE) + #guides(col=FALSE) +
  ggtitle("A. Four weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")

shoots6 <- ggplot(chillshoots6, aes(x=shootscontmean, y=shootsfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=shootscontmean, y=shootsfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = "none",
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Aboveground Biomass (control)") + 
  ylab("Aboveground Biomass (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chillshoots6$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
  #                 labels=sizes) +
  #scale_size_continuous(name=expression(Delta*" in false spring risk")) + 
  scale_y_continuous(breaks=seq(0,65,10)) +
  scale_x_continuous(breaks=seq(0,65,10)) +
  coord_cartesian(xlim=c(0,65), ylim=c(0, 65), expand=TRUE) + #guides(col=FALSE) +
  ggtitle("B. Six weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")

shoots8 <- ggplot(chillshoots8, aes(x=shootscontmean, y=shootsfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=shootscontmean, y=shootsfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position="none",
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.box="horizontal") +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Aboveground Biomass (control)") + 
  ylab("Aboveground Biomass (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chillshoots8$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
  #                 labels=sizes) +
  scale_y_continuous(breaks=seq(0,65,10)) +
  scale_x_continuous(breaks=seq(0,65,10)) +
  coord_cartesian(xlim=c(0,65), ylim=c(0, 65), expand=TRUE) + #guides(col=FALSE) +
  ggtitle("C. Eight weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")


quartz()
shootsplot<-grid.arrange(shoots4, shoots6, shoots8, ncol=3)

ggsave("figures/shootsspeciesdiff_bbdiff.png",width=30, height=14,units="cm",bg = "white",dpi=500, plot=shootsplot)


############# Now for roots...
roots <- subset(chillfrz, select=c("species", "chill", "roots", "tx"))
roots <- roots[!duplicated(roots),]

chillrootsfunc <- function (x) {
  
  exp <- roots[(roots$chill==x),]
  exp <- na.omit(exp)
  
  cont <- exp[(exp$tx==0),]
  frz <- exp[(exp$tx==1),]
  cont$rootscontmean <- ave(cont$roots, cont$species)
  cont$rootscontsd<-ave(cont$roots, cont$species, cont$tx, FUN=sd)/sqrt(length(unique(cont$rootscontmean)))
  frz$rootsfrzmean <- ave(frz$roots, frz$species)
  frz$rootsfrzsd<-ave(frz$roots, frz$species, frz$tx, FUN=sd)/sqrt(length(unique(frz$rootsfrzmean)))
  
  cont <- subset(cont , select=c("species", "rootscontmean", "rootscontsd"))
  cont <- cont[!duplicated(cont),]
  frz <- subset(frz , select=c("species", "rootsfrzmean", "rootsfrzsd"))
  frz <- frz[!duplicated(frz),]
  
  frzdiff <- full_join(cont, frz)
  
  frzdiff$diff<-frzdiff$rootsfrzmean-frzdiff$rootscontmean
  frzdiff$diff.sd<-frzdiff$rootsfrzsd-frzdiff$rootscontsd
  
  frzdiff$xmin <- frzdiff$rootscontmean-frzdiff$rootscontsd
  frzdiff$xmax <- frzdiff$rootscontmean+frzdiff$rootscontsd
  frzdiff$ymin <- frzdiff$rootsfrzmean-frzdiff$rootsfrzsd
  frzdiff$ymax <- frzdiff$rootsfrzmean+frzdiff$rootsfrzsd
  
  mround <- function(x,base){ 
    base*round(x/base) 
  }
  
  frzdiff$diff.labels <- mround(frzdiff$diff, 0.5)
  
  frzdiff$species.name <- NA
  frzdiff$species.name <- ifelse(frzdiff$species=="ACESAC", "Acer saccharinum", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="ALNRUG", "Alnus rugosa", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="BETPAP", "Betula papyrifera", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="BETPOP", "Betula populifolia", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="CORRAC", "Cornus racemosa", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="SALPUR", "Salix purpurea", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="SORAME", "Sorbus americana", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="VIBDEN", "Viburnum dentatum", frzdiff$species.name)
  
  frzdiff$bbtype <- NA
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("SALPUR", "BETPAP", "CORRAC"), "early", frzdiff$bbtype)
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("BETPOP", "ALNRUG"), "mid", frzdiff$bbtype)
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("VIBDEN", "ACESAC", "SORAME"), "late", frzdiff$bbtype)
  
  return(frzdiff)
  
}

chillroots4 <- chillrootsfunc(1)
chillroots6 <- chillrootsfunc(2)
chillroots8 <- chillrootsfunc(3)

#tt <- full_join(chillroots4, chillroots6)
#tt <- full_join(tt, chillroots8)
#valsize <- c(1:13)
#sizes <- sort(unique(tt$diff.labels))


cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)

roots4 <- ggplot(chillroots4, aes(x=rootscontmean, y=rootsfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=rootscontmean, y=rootsfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.825, .125),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=7),
        legend.text = element_text(size=6),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.key.size = unit(0.3, "cm")) +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Belowground Biomass (control)") + 
  ylab("Belowground Biomass (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chillroots4$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
  #                 labels=sizes) +
  #scale_size_continuous(name=expression(Delta*" in false spring risk")) + 
  scale_y_continuous(breaks=seq(0,65,10)) +
  scale_x_continuous(breaks=seq(0,65,10)) +
  coord_cartesian(xlim=c(0,65), ylim=c(0, 65), expand=TRUE) + #guides(col=FALSE) +
  ggtitle("A. Four weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")

roots6 <- ggplot(chillroots6, aes(x=rootscontmean, y=rootsfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=rootscontmean, y=rootsfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = "none",
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Belowground Biomass (control)") + 
  ylab("Belowground Biomass (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chillroots6$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
  #                 labels=sizes) +
  #scale_size_continuous(name=expression(Delta*" in false spring risk")) + 
  scale_y_continuous(breaks=seq(0,65,10)) +
  scale_x_continuous(breaks=seq(0,65,10)) +
  coord_cartesian(xlim=c(0,65), ylim=c(0, 65), expand=TRUE) + #guides(col=FALSE) +
  ggtitle("B. Six weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")

roots8 <- ggplot(chillroots8, aes(x=rootscontmean, y=rootsfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=rootscontmean, y=rootsfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position="none",
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.box="horizontal") +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Belowground Biomass (control)") + 
  ylab("Belowground Biomass (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chillroots8$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
  #                 labels=sizes) +
  scale_y_continuous(breaks=seq(0,65,10)) +
  scale_x_continuous(breaks=seq(0,65,10)) +
  coord_cartesian(xlim=c(0,65), ylim=c(0, 65), expand=TRUE) + #guides(col=FALSE) +
  ggtitle("C. Eight weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")


quartz()
rootsplot<-grid.arrange(roots4, roots6, roots8, ncol=3)

ggsave("figures/rootsspeciesdiff_bbdiff.png",width=30, height=14,units="cm",bg = "white",dpi=500, plot=rootsplot)


############# Now for root to shoot ratio...
chillfrz$rootstoshoots <- chillfrz$roots/chillfrz$shoots
rootstoshoots <- subset(chillfrz, select=c("species", "chill", "rootstoshoots", "tx"))
rootstoshoots <- rootstoshoots[!duplicated(rootstoshoots),]

chillrootstoshootsfunc <- function (x) {
  
  exp <- rootstoshoots[(rootstoshoots$chill==x),]
  exp <- na.omit(exp)
  
  cont <- exp[(exp$tx==0),]
  frz <- exp[(exp$tx==1),]
  cont$rootstoshootscontmean <- ave(cont$rootstoshoots, cont$species)
  cont$rootstoshootscontsd<-ave(cont$rootstoshoots, cont$species, cont$tx, FUN=sd)/sqrt(length(unique(cont$rootstoshootscontmean)))
  frz$rootstoshootsfrzmean <- ave(frz$rootstoshoots, frz$species)
  frz$rootstoshootsfrzsd<-ave(frz$rootstoshoots, frz$species, frz$tx, FUN=sd)/sqrt(length(unique(frz$rootstoshootsfrzmean)))
  
  cont <- subset(cont , select=c("species", "rootstoshootscontmean", "rootstoshootscontsd"))
  cont <- cont[!duplicated(cont),]
  frz <- subset(frz , select=c("species", "rootstoshootsfrzmean", "rootstoshootsfrzsd"))
  frz <- frz[!duplicated(frz),]
  
  frzdiff <- full_join(cont, frz)
  
  frzdiff$diff<-frzdiff$rootstoshootsfrzmean-frzdiff$rootstoshootscontmean
  frzdiff$diff.sd<-frzdiff$rootstoshootsfrzsd-frzdiff$rootstoshootscontsd
  
  frzdiff$xmin <- frzdiff$rootstoshootscontmean-frzdiff$rootstoshootscontsd
  frzdiff$xmax <- frzdiff$rootstoshootscontmean+frzdiff$rootstoshootscontsd
  frzdiff$ymin <- frzdiff$rootstoshootsfrzmean-frzdiff$rootstoshootsfrzsd
  frzdiff$ymax <- frzdiff$rootstoshootsfrzmean+frzdiff$rootstoshootsfrzsd
  
  mround <- function(x,base){ 
    base*round(x/base) 
  }
  
  frzdiff$diff.labels <- mround(frzdiff$diff, 0.5)
  
  frzdiff$species.name <- NA
  frzdiff$species.name <- ifelse(frzdiff$species=="ACESAC", "Acer saccharinum", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="ALNRUG", "Alnus rugosa", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="BETPAP", "Betula papyrifera", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="BETPOP", "Betula populifolia", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="CORRAC", "Cornus racemosa", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="SALPUR", "Salix purpurea", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="SORAME", "Sorbus americana", frzdiff$species.name)
  frzdiff$species.name <- ifelse(frzdiff$species=="VIBDEN", "Viburnum dentatum", frzdiff$species.name)
  
  frzdiff$bbtype <- NA
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("SALPUR", "BETPAP", "CORRAC"), "early", frzdiff$bbtype)
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("BETPOP", "ALNRUG"), "mid", frzdiff$bbtype)
  frzdiff$bbtype <- ifelse(frzdiff$species%in%c("VIBDEN", "ACESAC", "SORAME"), "late", frzdiff$bbtype)
  
  return(frzdiff)
  
}

chillrootstoshoots4 <- chillrootstoshootsfunc(1)
chillrootstoshoots6 <- chillrootstoshootsfunc(2)
chillrootstoshoots8 <- chillrootstoshootsfunc(3)

#tt <- full_join(chillrootstoshoots4, chillrootstoshoots6)
#tt <- full_join(tt, chillrootstoshoots8)
#valsize <- c(1:13)
#sizes <- sort(unique(tt$diff.labels))


cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)

rootstoshoots4 <- ggplot(chillrootstoshoots4, aes(x=rootstoshootscontmean, y=rootstoshootsfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=rootstoshootscontmean, y=rootstoshootsfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.825, .125),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=7),
        legend.text = element_text(size=6),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.key.size = unit(0.3, "cm")) +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Root to shoot ratio (control)") + 
  ylab("Root to shoot ratio (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chillrootstoshoots4$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
  #                 labels=sizes) +
  #scale_size_continuous(name=expression(Delta*" in false spring risk")) + 
  scale_y_continuous(breaks=seq(0, 4,1)) +
  scale_x_continuous(breaks=seq(0, 4,1)) +
  coord_cartesian(xlim=c(0,5), ylim=c(0, 5), expand=TRUE) + #guides(col=FALSE) +
  ggtitle("A. Four weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")

rootstoshoots6 <- ggplot(chillrootstoshoots6, aes(x=rootstoshootscontmean, y=rootstoshootsfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=rootstoshootscontmean, y=rootstoshootsfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = "none",
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Root to shoot ratio (control)") + 
  ylab("Root to shoot ratio (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chillrootstoshoots6$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
  #                 labels=sizes) +
  #scale_size_continuous(name=expression(Delta*" in false spring risk")) + 
  scale_y_continuous(breaks=seq(0, 4,1)) +
  scale_x_continuous(breaks=seq(0, 4,1)) +
  coord_cartesian(xlim=c(0,5), ylim=c(0, 5), expand=TRUE) + #guides(col=FALSE) +
  ggtitle("B. Six weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")

rootstoshoots8 <- ggplot(chillrootstoshoots8, aes(x=rootstoshootscontmean, y=rootstoshootsfrzmean, col=bbtype), alpha=2) + 
  geom_point(aes(x=rootstoshootscontmean, y=rootstoshootsfrzmean), shape=3) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position="none",
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.box="horizontal") +
  #geom_text(aes(label=species.name), vjust=-1, fontface="italic", size=3, position=position_jitter(width=0.5,height=0.5)) + 
  xlab("Root to shoot ratio (control)") + 
  ylab("Root to shoot ratio (treatment)") + 
  scale_color_manual(name="Budburst timing", values=cols,
                     labels=chillrootstoshoots8$bbtype) +
  #scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
  #                 labels=sizes) +
  scale_y_continuous(breaks=seq(0, 4,1)) +
  scale_x_continuous(breaks=seq(0, 4,1)) +
  coord_cartesian(xlim=c(0,5), ylim=c(0, 5), expand=TRUE) + #guides(col=FALSE) +
  ggtitle("C. Eight weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")


quartz()
rootstoshootsplot<-grid.arrange(rootstoshoots4, rootstoshoots6, rootstoshoots8, ncol=3)

ggsave("figures/rootstoshootsspeciesdiff_bbdiff.png",width=30, height=14,units="cm",bg = "white",dpi=500, plot=rootstoshootsplot)


