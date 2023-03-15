library(terra);library(sf); library(readr); library(tidyterra);
library(ggplot2); library(viridis); library(assignR); library(raster)
#not sure which packages are acutally being used here...

ForensicTIsoData <- read_csv("data/ForensicIsoData.csv", 
                             col_types = cols(...1 = col_skip()))
##Oye, not coperating on my computer
ForensicTIsoData <- ForensicIsoData
df <- vect(ForensicTIsoData, geom=c("Lon", "Lat"), 
           crs="+proj=longlat +datum=WGS84")

# column binding for isoscape data

df_scape <- cbind(ForensicTIsoData, isoscapeO) %>% 
  cbind(isoscapeSr) %>% 
  select(-c(ID, ID)) %>% 
  rename(srscape = rf_plantsoilmammal1) %>% 
  mutate(Sr = ifelse(Isotope == '87Sr/86Sr', Iso.Value, NA_integer_)) %>% 
  mutate(O = ifelse(Isotope == 'd18O', Iso.Value, NA_integer_)) %>% 
  mutate(deltaSr = Sr - srscape) %>% 
  mutate(deltaO = O - d18o_MA)


#subset just hair
hair <- subset(df_scape, Element == 'hair')

#subset smaller hair for testing
testhair <- subset(df_scape, Calibrate == 'OldUT_O_1') %>% 
  rename(d18O  = Iso.Value,
         ref_scale = Calibrate)
#add in SD
testhair$d18O.sd <-"0.3"

#trying the refTrans, needs calibration scale field for the selected marker
e <-refTrans(testhair, marker = "d18O", ref_scale = "VSMOW_O")

#doesn't like this one, says marker.sd unused
e <-refTrans(testhair, marker = "d18O", marker.sd = "d18O.sd", ref_scale = "VSMOW_O")
