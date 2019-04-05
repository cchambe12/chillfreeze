## Started 5 April 2019 ##
## Build a figure that shows individuals undergoing False spring

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(ggplot2)


df <- data.frame(temp =c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 7, 7, 3, 3, 0, 0, -3, -3, -3, 0, 3, 3, 7, 7, 10),
                 time =c("8:00", "9:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00",
                         "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "0:00", "1:00", "2:00", "3:00", "4:00",
                         "5:00", "6:00", "7:00", "8:01"))
growthchamber <- ggplot(df, aes(x=factor(df$time,levels = c("8:00", "9:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00",
                                                            "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "0:00", "1:00", "2:00", "3:00", "4:00",
                                                            "5:00", "6:00", "7:00", "8:01")), y=temp, group=1)) +  
  geom_line(aes(x=factor(df$time,levels = c("8:00", "9:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00",
                                            "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "0:00", "1:00", "2:00", "3:00", "4:00",
                                            "5:00", "6:00", "7:00", "8:01")))) + theme_classic() +
  xlab("Time (hr)") + ylab("Temperature (°C)")

quartz()
growthchamber
