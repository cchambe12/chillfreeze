## Started 5 April 2019 ##
## Build a figure that shows individuals undergoing False spring

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(ggplot2)
library(viridis)
library(RColorBrewer)

my.pal <- viridis_pal(option="viridis")(4)
my.pal <- brewer.pal(4, "Dark2")


df <- data.frame(temp =c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 7, 7, 3, 3, 0, 0, -3, -3, -3, 0, 3, 3, 7, 7, 10),
                 time =c("8:00", "9:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00",
                         "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "0:00", "1:00", "2:00", "3:00",
                         "4:00", "5:00", "6:00", "7:00", "8:01"))
growthchamber <- ggplot(df, aes(x=factor(df$time,levels = c("8:00", "9:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00",
                                                            "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "0:00", "1:00", "2:00", "3:00", "4:00",
                                                            "5:00", "6:00", "7:00", "8:01")), y=temp, group=1)) +  
  geom_line(aes(x=factor(df$time,levels = c("8:00", "9:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00",
                                            "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "0:00", "1:00", "2:00", "3:00", "4:00",
                                            "5:00", "6:00", "7:00", "8:01")))) + theme_classic() +
  scale_x_discrete(breaks =c("8:00", "10:00", "12:00", "14:00", "16:00",
                             "18:00", "20:00", "22:00", "0:00", "2:00", "4:00",
                             "6:00", "8:01"),
    labels= c("8:00", "10:00", "12:00", "14:00", "16:00",
                                       "18:00", "20:00", "22:00", "0:00", "2:00", "4:00",
                                       "6:00", "8:00")) +
  scale_y_continuous(breaks = sort(c(seq(-5, 10, by=5), -1.7, -2.2, -7.4))) +
  geom_hline(yintercept=-1.7, col=my.pal[1], size=1.5) +
  annotate("text", x=8, y=-1.2, label = "Soft freeze (Augspurger, 2013)") +
  geom_hline(yintercept=-2.2, col=my.pal[2], size=1.5) +
  annotate("text", x=8, y=-2.6, label = "Hard freeze (Schwartz, 1993)") +
  geom_hline(yintercept=-7.4, col=my.pal[3], size=1.5) +
  annotate("text", x=8, y=-7, label = expression(paste(italic("Sorbus aucuparia"),  " (Lenz et al., 2016)"))) +
  geom_hline(yintercept=-4.8, col=my.pal[4], size=1.5) +
  annotate("text", x=8, y=-4.3, label = expression(paste(italic("Fagus sylvatica"),  " (Lenz et al., 2016)"))) +
  xlab("Time (hr)") + ylab("Temperature (°C)")

quartz()
growthchamber
