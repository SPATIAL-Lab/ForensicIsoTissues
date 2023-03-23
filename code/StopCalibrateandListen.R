library(terra);library(sf); library(readr); library(assignR); library(dplyr); library(assignR)
library(raster)
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

#Oxygen isoscape of calibrated data
prpiso = getIsoscapes("GlobalPrecipMA")
prpiso = stack(prpiso$d18o_MA, prpiso$d18o_se_MA)

hspdf =SpatialPointsDataFrame(data.frame(e$data$Lon, e$data$Lat),
                              data.frame(e$data$d18O, e$data$d18O.sd))

proj4string(hspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

hairscape= calRaster(hspdf, prpiso, mask = naMap)

#now isoscape uncalibrated oxygen hairs
regularhair <- subset(ForensicTIsoData, Element == 'hair' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value,
         d18O_cal = Calibrate)

regularhair$d18O.sd <-0.3

#rhspdf =SpatialPointsDataFrame(data.frame(regularhair$data$Lon, regularhair$data$Lat),
#                              data.frame(regularhair$data$d18O, regularhair$data$d18O.sd))

rhspdf =SpatialPointsDataFrame(data.frame(regularhair$Lon, regularhair$Lat),
                               data.frame(regularhair$d18O, regularhair$d18O.sd))

proj4string(rhspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

reghairscape= calRaster(rhspdf, prpiso, mask = naMap)

#teeth time, teeth oxygen isoscape
teeth <- subset(ForensicTIsoData, Element == 'teeth' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value,
         d18O_cal = Calibrate)
teeth$d18O.sd <-0.3

tspdf =SpatialPointsDataFrame(data.frame(teeth$Lon, teeth$Lat),
                              data.frame(teeth$d18O, teeth$d18O.sd))

proj4string(tspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

teethscape= calRaster(tspdf, prpiso, mask = naMap)

#Uncalibrated hairs, known- is a NO-GO and assumed is also a NO-GO
Kregularhair <- subset(ForensicTIsoData, Element == 'hair' & Isotope == 'd18O' & Data.Origin=='known') %>% 
  rename(d18O  = Iso.Value,
         d18O_cal = Calibrate)

Kregularhair$d18O.sd <-0.3

Krhspdf =SpatialPointsDataFrame(data.frame(Kregularhair$Lon, Kregularhair$Lat),
                               data.frame(Kregularhair$d18O, Kregularhair$d18O.sd))

proj4string(Krhspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

Kreghairscape= calRaster(Krhspdf, prpiso, mask = naMap)

Aregularhair <- subset(ForensicTIsoData, Element == 'hair' & Isotope == 'd18O' & Data.Origin=='assumed') %>% 
  rename(d18O  = Iso.Value,
         d18O_cal = Calibrate)

Aregularhair$d18O.sd <-0.3

Arhspdf =SpatialPointsDataFrame(data.frame(Aregularhair$Lon, Aregularhair$Lat),
                                data.frame(Aregularhair$d18O, Aregularhair$d18O.sd))

proj4string(Arhspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

Areghairscape= calRaster(Arhspdf, prpiso, mask = naMap)
