library(terra);library(sf); library(readr); library(assignR); library(dplyr)
#not sure which packages are actually being used here...

ForensicTIsoData <- read_csv("data/ForensicIsoData.csv", 
                             col_types = cols(...1 = col_skip()))
##Oye, not coperating on my computer

testhair <- subset(ForensicTIsoData, Element == 'hair' & Calibrate == 'OldUT_O_1') %>% 
  rename(d18O  = Iso.Value,
        d18O_cal = Calibrate)

testhair <- as.data.frame(testhair)

#add in SD
testhair$d18O.sd <-0.3

#trying the refTrans, needs calibration scale field for the selected marker
e = refTrans(testhair, marker = "d18O", ref_scale = "VSMOW_O", niter = 5000)




