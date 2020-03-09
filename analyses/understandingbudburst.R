## 6 March 2020 - Cat
# Understanding budburst timing across species

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

acebb <- mean(chillfrz$budburst[chillfrz$species=="ACESAC"], na.rm=TRUE) ### 40.65
acebb.sd <- sd(chillfrz$budburst[chillfrz$species=="ACESAC"], na.rm=TRUE) ### 8.14
acebb.range <- range(chillfrz$budburst[chillfrz$species=="ACESAC"], na.rm=TRUE) ### 28-63

alnbb <- mean(chillfrz$budburst[chillfrz$species=="ALNRUG"], na.rm=TRUE) ### 24.02
alnbb.sd <- sd(chillfrz$budburst[chillfrz$species=="ALNRUG"], na.rm=TRUE) ### 6.6
alnbb.range <- range(chillfrz$budburst[chillfrz$species=="ALNRUG"], na.rm=TRUE) ### 14-46

bpapbb <- mean(chillfrz$budburst[chillfrz$species=="BETPAP"], na.rm=TRUE) ### 14.21
bpapbb.sd <- sd(chillfrz$budburst[chillfrz$species=="BETPAP"], na.rm=TRUE) ### 4.18
bpapbb.range <- range(chillfrz$budburst[chillfrz$species=="BETPAP"], na.rm=TRUE) ### 7-26

bpopbb <- mean(chillfrz$budburst[chillfrz$species=="BETPOP"], na.rm=TRUE) ### 18.35
bpopbb.sd <- sd(chillfrz$budburst[chillfrz$species=="BETPOP"], na.rm=TRUE) ### 3.5
bpopbb.range <- range(chillfrz$budburst[chillfrz$species=="BETPOP"], na.rm=TRUE) ### 14-27

corbb <- mean(chillfrz$budburst[chillfrz$species=="CORRAC"], na.rm=TRUE) ### 14.83
corbb.sd <- sd(chillfrz$budburst[chillfrz$species=="CORRAC"], na.rm=TRUE) ### 5.79
corbb.range <- range(chillfrz$budburst[chillfrz$species=="CORRAC"], na.rm=TRUE) ### 7-29

salbb <- mean(chillfrz$budburst[chillfrz$species=="SALPUR"], na.rm=TRUE) ### 9.94
salbb.sd <- sd(chillfrz$budburst[chillfrz$species=="SALPUR"], na.rm=TRUE) ### 2.31
salbb.range <- range(chillfrz$budburst[chillfrz$species=="SALPUR"], na.rm=TRUE) ### 4-14

sorbb <- mean(chillfrz$budburst[chillfrz$species=="SORAME"], na.rm=TRUE) ### 35.5
sorbb.sd <- sd(chillfrz$budburst[chillfrz$species=="SORAME"], na.rm=TRUE) ### 7.4
sorbb.range <- range(chillfrz$budburst[chillfrz$species=="SORAME"], na.rm=TRUE) ### 23-59

vibbb <- mean(chillfrz$budburst[chillfrz$species=="VIBDEN"], na.rm=TRUE) ### 29.91
vibbb.sd <- sd(chillfrz$budburst[chillfrz$species=="VIBDEN"], na.rm=TRUE) ### 9.59
vibbb.range <- range(chillfrz$budburst[chillfrz$species=="VIBDEN"], na.rm=TRUE) ### 16-56


my.pal <- rep(brewer.pal(n = 8, name = "Dark2"), 2)
#my.pch <- rep(15:16, each=10)

chillfrz$species.name <- NA
chillfrz$species.name <- ifelse(chillfrz$species=="ACESAC", "Acer saccharinum", chillfrz$species.name)
chillfrz$species.name <- ifelse(chillfrz$species=="ALNRUG", "Alnus rugosa", chillfrz$species.name)
chillfrz$species.name <- ifelse(chillfrz$species=="BETPAP", "Betula papyrifera", chillfrz$species.name)
chillfrz$species.name <- ifelse(chillfrz$species=="BETPOP", "Betula populifolia", chillfrz$species.name)
chillfrz$species.name <- ifelse(chillfrz$species=="CORRAC", "Cornus racemosa", chillfrz$species.name)
chillfrz$species.name <- ifelse(chillfrz$species=="SALPUR", "Salix purpurea", chillfrz$species.name)
chillfrz$species.name <- ifelse(chillfrz$species=="SORAME", "Sorbus americana", chillfrz$species.name)
chillfrz$species.name <- ifelse(chillfrz$species=="VIBDEN", "Viburnum dentatum", chillfrz$species.name)

hist<-ggplot(chillfrz, aes(x=budburst)) + geom_histogram(aes(fill=species.name), size=0.3) +
  xlab("Day of budburst") + ylab("Number of individuals") + scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=my.pal, name="Species") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.text = element_text(size=8, face="italic"),legend.key.size = unit(0.5, "cm"),
        axis.title=element_text(size=12), legend.title = element_text(size=8), axis.text=element_text(size=10), legend.text.align = 0)

quartz()
hist

ggsave("figures/budbursthist.png",width=14, height=14,units="cm",bg = "white",dpi=500, plot=hist)


acebb
alnbb
bpapbb
bpopbb
corbb
salbb
sorbb
vibbb

salbb #9.94, early
bpapbb #14.21, early
corbb #14.83, early
bpopbb #18.35, mid
alnbb #24.0, mid
vibbb #29.9, late
sorbb #35.5, late
acebb #40.65 late



