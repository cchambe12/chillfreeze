## Started 17 December 2018 ##
## By Cat ##

## Making species list for observations ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


library(dplyr)
library(tidyr)


# Setting working directory
setwd("~/Documents/git/chillfreeze/data")


df<-data.frame(Individ=c(rep("ACESAC", 48), rep("ALNRUG", 48), rep("BETPAP", 48),
                         rep("BETPOP", 48), rep("CORRAC", 48), rep("FAGGRA", 48),
                         rep("NYSSYL", 48), rep("SALPUR", 48), rep("SORAME", 48),
                         rep("VIBDEN", 48)),
               Num=c(seq(14, 164, by=10), seq(16, 166, by=10),
                     seq(18, 168, by=10)))


df$ID<-paste(df$Individ, df$Num, sep="_")

write.csv(df, file="observation_datasheet.csv", row.names = FALSE)


