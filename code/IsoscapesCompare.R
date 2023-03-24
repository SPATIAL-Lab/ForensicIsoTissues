# Setup -------------------------------------------------------------------
# libraries

library(terra);library(sf); library(readr); library(tidyterra);
library(ggplot2); library(viridis); library(assignR); library(raster)

# tissue data import and vectorize

ForensicTIsoData <- read_csv("data/ForensicIsoData.csv", 
                             col_types = cols(...1 = col_skip()))
##Oye, not cooperating on my computer
ForensicTIsoData <- ForensicIsoData
df <- vect(ForensicTIsoData, geom=c("Lon", "Lat"), 
           crs="+proj=longlat +datum=WGS84")

# strontium isoscape (from Bataille et al. 2020)

strontium <- rast("shapefiles/rf_plantsoilmammal1.tif")

df <- project(df, strontium)
isoscapeSr <- extract(strontium, df)






# quick check that our points plot over the isoscape map
ggplot() + 
  geom_spatraster(data = strontium) +
  geom_spatvector(data = df, color = 'skyblue') + 
  theme_void()

# oxygen isoscape from waterisotopes.org
oxygen <- rast("shapefiles/d18o_MA.tif")
df <- project(df, oxygen)
isoscapeO <- extract(oxygen, df)

# quick check that our points plot over the isoscape map
ggplot() + 
  geom_spatraster(data = oxygen) +
  geom_spatvector(data = df, color = 'darkorange') + 
  theme_void()

# column binding for isoscape data
df_scape <- cbind(ForensicTIsoData, isoscapeO) %>% 
  cbind(isoscapeSr) %>% 
  select(-c(ID, ID)) %>% 
  rename(srscape = rf_plantsoilmammal1) %>% 
  mutate(Sr = ifelse(Isotope == '87Sr/86Sr', Iso.Value, NA_integer_)) %>% 
  mutate(O = ifelse(Isotope == 'd18O', Iso.Value, NA_integer_)) %>% 
  mutate(deltaSr = Sr - srscape) %>% 
  mutate(deltaO = O - d18o_MA)

#more cowbell, doesn't work, dang it. 
df_scape <- cbind(ForensicTIsoData, prpiso) %>% 
  cbind(Sriso) %>% 
  mutate(Sr = ifelse(Isotope == '87Sr/86Sr', Iso.Value, NA_integer_)) %>% 
  mutate(O = ifelse(Isotope == 'd18O', Iso.Value, NA_integer_)) %>% 
  mutate(deltaSr = Sr - Sriso) %>% 
  mutate(deltaO = O - prpiso)

# Comparing Isoscape Data -------------------------------------------------

# Oxygen

ggplot(data = df_scape, aes(x = Element, y = deltaO)) + 
  geom_violin(aes(fill = Data.Origin)) + 
  theme_classic()

ggplot() + 
  geom_density(data = df_scape, aes(x = deltaO, fill = Element, 
                                    color = Element),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'B') + 
  scale_color_viridis(discrete = T, option = 'B') + 
  theme_dark()

# check Chenery et al. estimated conversation for carbonate

# Strontium

ggplot() + 
  geom_density(data = df_scape, aes(x = deltaSr, fill = Element, 
                                    color = Element),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  theme_dark()
                 
# What's up with that twin tail for bone? 

bone <- subset(df_scape, Element == "bone", Sr =!is.na)
# ohhhhh there's only two samples

# Hair and Teeth only

hairteeth <- subset(df_scape, Element == 'hair' | Element == 'teeth')

ggplot() + 
  geom_density(data = hairteeth, aes(x = deltaSr, fill = Element, 
                                    color = Element),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  theme_dark()

ggplot() + 
  geom_density(data = hairteeth, aes(x = deltaO, fill = Element, 
                                     color = Element),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  theme_dark()

# Tissue Versus Isoscape

ggplot() + 
  geom_point(data = hairteeth, aes(x = deltaO, y = O, color = Element), size = 2) + 
  scale_color_manual(values = c("darkblue", "darkorange")) +  
  theme_classic() 

#subset just hair
hair <- subset(df_scape, Element == 'hair')
#subset smaller hair for testing
testhair <- subset(df_scape, Calibrate == 'OldUT_O_1') %>% 
  rename(d18O  = Iso.Value,
         ref_scale = Calibrate)
#add in SD
testhair$d18O.sd <-"0.3"

#trying the refTrans
e <-refTrans(testhair, marker = "d18O", ref_scale = "VSMOW_O")

#Calibrating the hair oxygen values to make the isoscape
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

#strontium? VICTORY- an isoscape was created
Sr<- raster("shapefiles/GlobalSr/GlobalSr.tif")
Sr.sd<- raster("shapefiles/GlobalSr/GlobalSr_se.tif")

Sriso =stack(Sr, Sr.sd)

teeth <- subset(ForensicTIsoData, Element == 'teeth' & Isotope == '87Sr/86Sr') %>% 
  rename(Sr  = Iso.Value)
teeth$Sr.sd <-0.0003


tSrspdf =SpatialPointsDataFrame(data.frame(teeth$Lon, teeth$Lat),
                              data.frame(teeth$Sr, teeth$Sr.sd))

proj4string(tSrspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

teethSrscape= calRaster(tSrspdf, Sriso, mask = naMap)

#Hair stront
Srhair <- subset(ForensicTIsoData, Element == 'hair' & Isotope == '87Sr/86Sr') %>% 
  rename(Sr  = Iso.Value)
Srhair$Sr.sd <-0.0003


hSrspdf =SpatialPointsDataFrame(data.frame(Srhair$Lon, Srhair$Lat),
                                data.frame(Srhair$Sr, Srhair$Sr.sd))

proj4string(hSrspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

hairSrscape= calRaster(hSrspdf, Sriso, mask = naMap)

#Getting a bit hairy, known and assumed orig hair stront data
KSrhair <- subset(ForensicTIsoData, Element == 'hair' & Isotope == '87Sr/86Sr' & Data.Origin =='known') %>% 
  rename(Sr  = Iso.Value)
KSrhair$Sr.sd <-0.0003


KhSrspdf =SpatialPointsDataFrame(data.frame(KSrhair$Lon, KSrhair$Lat),
                                data.frame(KSrhair$Sr, KSrhair$Sr.sd))

proj4string(KhSrspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

hairKSrscape= calRaster(KhSrspdf, Sriso, mask = naMap)

ASrhair <- subset(ForensicTIsoData, Element == 'hair' & Isotope == '87Sr/86Sr' & Data.Origin =='assumed') %>% 
  rename(Sr  = Iso.Value)
ASrhair$Sr.sd <-0.0003


AhSrspdf =SpatialPointsDataFrame(data.frame(ASrhair$Lon, ASrhair$Lat),
                                 data.frame(ASrhair$Sr, ASrhair$Sr.sd))

proj4string(AhSrspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

hairASrscape= calRaster(AhSrspdf, Sriso, mask = naMap)

#trying out Gabe's magic code stuff
#look at the calRaster data
class(hairscape)
names(hairscape)
#info on the stuff returned by calRaster is available in help...see the 'Value' section
?calRaster

#here are some of the data you might want to look at
plot(hairscape$lm.data$isoscape.iso, hairscape$lm.data$tissue.iso)
View(hairscape$lm.data)

#or maybe plot up the model residuals?
plot(density(hairscape$lm.model$residuals))

#Looking at all the scapes 
plot(reghairscape$lm.data$isoscape.iso, reghairscape$lm.data$tissue.iso)
View(reghairscape$lm.data)
plot(density(reghairscape$lm.model$residuals))

plot(teethscape$lm.data$isoscape.iso, teethscape$lm.data$tissue.iso)
View(teethscape$lm.data)
plot(density(teethscape$lm.model$residuals))

plot(hairSrscape$lm.data$isoscape.iso, hairSrscape$lm.data$tissue.iso)
View(hairSrscape$lm.data)
plot(density(hairSrscape$lm.model$residuals))

plot(teethSrscape$lm.data$isoscape.iso, teethSrscape$lm.data$tissue.iso)
View(teethSrscape$lm.data)
plot(density(teethSrscape$lm.model$residuals))

#trying to be smart. No really sure what this is doing
deltaSrCh = (hairSrscape$lm.data$tissue.iso  - hairSrscape$lm.data$isoscape.iso)
deltaOh = (reghairscape$lm.data$tissue.iso  - reghairscape$lm.data$isoscape.iso)
deltaO1 = (hairscape$lm.data$tissue.iso  - hairscape$lm.data$isoscape.iso)

deltaSrt = (teethSrscape$lm.data$tissue.iso  - teethSrscape$lm.data$isoscape.iso)
deltaOt = (teethscape$lm.data$tissue.iso  - teethscape$lm.data$isoscape.iso)

#BANANA, This doesn't work, the data isn't compatible
hair$deltaO <-deltaOh
Srhair$deltaSr <-deltaSrCh

#Trying to make it pretty?
plot(teethSrscape$lm.data$isoscape.iso, teethSrscape$lm.data$tissue.iso, xlab ="Sr Isoscape", ylab="Sr Teeth Data") 
plot(density(teethSrscape$lm.model$residuals), main ="Density Plot Teeth Sr",)

