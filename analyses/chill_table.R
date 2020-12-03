## Started 3 December 2020 by Cat
# Add in Chilling table for supplement

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries

library(chillR)
library(Interpol.T)

allweeks <- data.frame(date = rep(seq.Date(as.Date("2019-01-01"), as.Date("2019-02-25"), by="day"), each=24),
                        Temp = 4,
                        Year = 2019)

hrly = vector()
hrly = rbind(hrly,
               data.frame(
                 date = allweeks$date,
                 Temp = allweeks$Temp,
                 Year = allweeks$Year, 
                 JDay = as.numeric(lubridate::yday(as.Date(allweeks$date))),
                 month = substr(allweeks$date, 6, 7),
                 day = substr(allweeks$date, 9, 10),
                 Hour = rep(1:24, times=56)
               )
  )
  

four <- hrly[hrly$date >= "2019-01-01" && hrly$date <= "2019-01-28",] 
six <- hrly[hrly$date >= "2019-01-01" & hrly$date <= "2019-02-11",] 
eight <- hrly[hrly$date >= "2019-01-01" & hrly$date <= "2019-02-25",] 

chillcalc.4 <- chilling(four, 1, 28)
chillcalc.6 <- chilling(six, 1, 42)
chillcalc.8 <- chilling(eight, 1, 56)

colz = c("Chilling_Hours","Utah_Model","Chill_portions")


allcalc <- rbind(chillcalc.8[colz], chillcalc.6[colz], chillcalc.4[colz])


allcalc <- data.frame(Duration = c("8 weeks", "6 weeks","4 weeks"), allcalc)
rownames(allcalc)=NULL

