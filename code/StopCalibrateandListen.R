library(terra);library(sf); library(readr); library(assignR); library(dplyr)
#not sure which packages are actually being used here...

ForensicTIsoData <- read_csv("data/ForensicIsoData.csv", 
                             col_types = cols(...1 = col_skip()))
##Oye, not coperating on my computer

testhair <- subset(ForensicTIsoData, Element == 'hair' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value,
        d18O_cal = Calibrate)

#add in SD
testhair$d18O.sd <-0.3

#reftrans can't handle data with no calibration scale
toTrans = data.frame(testhair[!is.na(testhair$d18O_cal),])

#trying the refTrans, needs calibration scale field for the selected marker
e = refTrans(toTrans, marker = "d18O", ref_scale = "VSMOW_O")

#let's compare pre and post trans; sample order is different so we need
#to match IDs to plot pre and post from the same samples
ind = match(toTrans$Data.ID, e$data$Data.ID)

#plot d18O
plot(toTrans$d18O, e$data$d18O[ind])
