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



##################### Old Plots ##########################
dvr <- subset(chillfrz, select=c("species", "chill", "dvr", "tx"))
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
  
  return(frzdiff)
  
}

chill4 <- chilldvrfunc(1)
chill6 <- chilldvrfunc(2)
chill8 <- chilldvrfunc(3)

tt <- full_join(chill4, chill6)
tt <- full_join(tt, chill8)
valsize <- c(1:13)
sizes <- sort(unique(tt$diff.labels))


cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)

dvr4 <- ggplot(chill4, aes(x=dvrcontmean, y=dvrfrzmean, col=species.name), alpha=2) + 
  geom_point(aes(x=dvrcontmean, y=dvrfrzmean, size=as.factor(diff.labels)), shape=21) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = "none",
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  #geom_text(aes(label=species), vjust=2) + 
  xlab("Duration of vegetative risk (control)") + 
  ylab("Duration of vegetative \nrisk (treatment)") + 
  scale_color_manual(name="Species", values=cols,
                     labels=chill4$species.name) +
  scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
                    labels=sizes) +
  #scale_size_continuous(name=expression(Delta*" in false spring risk")) + 
  scale_y_continuous(breaks=seq(8,28,2)) +
  scale_x_continuous(breaks=seq(8,28,2)) +
  coord_cartesian(xlim=c(8,28), ylim=c(8,28), expand=TRUE) + guides(col=FALSE) +
  ggtitle("A. Four weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")

dvr6 <- ggplot(chill6, aes(x=dvrcontmean, y=dvrfrzmean, col=species.name), alpha=2) + 
  geom_point(aes(x=dvrcontmean, y=dvrfrzmean, size=as.factor(diff.labels)), shape=21) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = "none",
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  #geom_text(aes(label=species), vjust=2) + 
  xlab("Duration of vegetative risk (control)") + 
  ylab("Duration of vegetative \nrisk (treatment)") + 
  scale_color_manual(name="Species", values=cols,
                     labels=chill6$species.name) +
  scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
                    labels=sizes) +
  #scale_size_continuous(name=expression(Delta*" in false spring risk")) + 
  scale_y_continuous(breaks=seq(8,28,2)) +
  scale_x_continuous(breaks=seq(8,28,2)) +
  coord_cartesian(xlim=c(8,28), ylim=c(8,28), expand=TRUE) + guides(col=FALSE) +
  ggtitle("B. Six weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")

dvr8 <- ggplot(chill8, aes(x=dvrcontmean, y=dvrfrzmean, col=species.name), alpha=2) + 
  geom_point(aes(x=dvrcontmean, y=dvrfrzmean, size=as.factor(diff.labels)), shape=21) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax), alpha=0.3) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position="none",
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.box="horizontal") +
  #geom_text(aes(label=species.name), vjust=-1, angle=345, fontface="italic") + 
  xlab("Duration of vegetative risk (control)") + 
  ylab("Duration of vegetative \nrisk (treatment)") + 
  scale_color_manual(name="Species", values=cols,
                     labels=chill8$species.name) +
  scale_size_manual(name=expression("Change in duration of \nvegetative risk (days)"), values=valsize,
                    labels=sizes) +
  scale_y_continuous(breaks=seq(8,28,2)) +
  scale_x_continuous(breaks=seq(8,28,2)) +
  coord_cartesian(xlim=c(8,28), ylim=c(8,28), expand=TRUE) +
  ggtitle("C. Eight weeks chilling") + geom_abline(intercept = 0, slope = 1, col="grey")


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



ggsave("figures/dvrspeciesdifftest.png",width=30, height=14,units="cm",bg = "white",dpi=500, plot=dvrplot)
