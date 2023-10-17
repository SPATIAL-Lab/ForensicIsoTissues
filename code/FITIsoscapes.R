#Isoscapes and QAs
FTID <-FTID2
FTID <-read_csv("data/ForensicTissue.csv")

#Base shapefile in AEA, read in map of North America from shapefile, this is done in previous Mapping script, but if not mapping, run for shapefile
NorAmericamap <-vect("shapefiles/Namap_aea.shp")
NAMAP = aggregate(NorAmericamap)

#Create Base Oxygen and Strontium Isoscapes, pull global precipitation isoscapes from assignR
prpiso = getIsoscapes("GlobalPrecipMA")
prpiso <- c(prpiso$d18o_MA, prpiso$d18o_se_MA)
prpiso1 = crop(prpiso, c( -180, -25, 0, 83.58326))
prpiso1 = terra::project(prpiso1, crs(NAMAP))
prpiso1 = crop(prpiso1, NAMAP)

plot(prpiso1)
Sriso = getIsoscapes("GlobalSr")
Sriso= crop(Sriso, c(-16653815.4396, 0, 0, 8376837.3753))
Sriso = terra::project(Sriso, crs(NAMAP))
Sriso <-terra::mask (Sriso, NAMAP)
Sriso = crop(Sriso, NAMAP)

#Osxygen Hair Isoscapes
#calibrated hairs, using RefTrans
calhair <- subset(FTID, Element == 'hair' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value,
         d18O_cal = Calibrate)
calhair$d18O.sd <-0.3
toTrans = data.frame(calhair[!is.na(calhair$d18O_cal),])
e = refTrans(toTrans, marker = "d18O", ref_scale = "VSMOW_O")
hsp.cal = vect(data.frame("lon" = e$data$Lon, "lat" = e$data$Lat, 
                          "d18O" = e$data$d18O, "d18O.sd" = e$data$d18O.sd), 
               crs = "WGS84")
hairscape.cal = calRaster(hsp.cal, prpiso1)
calhair$residuals = hairscape.cal$lm.model$residuals

#Oxygen Hair Isoscape uncalibrated/regular/no refTrans
regularhair <- subset(FTID, Element == 'hair' & Isotope == 'd18O')%>% 
  rename(d18O  = Iso.Value)
regularhair$d18O.sd <- 0.3
hsp.orig = vect(data.frame("lon" = regularhair$Lon, "lat" = regularhair$Lat, 
                           "d18O" = regularhair$d18O, "d18O.sd" = regularhair$d18O.sd), 
                crs = "WGS84")
hairscape.orig = calRaster(hsp.orig, prpiso1)
regularhair$residuals = hairscape.orig$lm.model$residuals

#Strontium Hair Isoscape
hairSr <- subset(FTID, Element == 'hair' & Isotope == '87Sr/86Sr') %>% 
  rename(Sr  = Iso.Value)
hairSr$Sr.sd <- 0.3
Srhair <- vect(data.frame("lon" = hairSr$Lon, "lat" = hairSr$Lat, 
                          "Sr" = hairSr$Sr, "Sr.sd" = hairSr$Sr.sd), 
               crs = "WGS84")
hairSrscape = calRaster(Srhair, Sriso)
hairSr$residuals = hairSrscape$lm.model$residuals

#Teeth
teethO <- subset(FTID, Element == 'teeth' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value)
teethO$d18O.sd <- 0.3
teethoxy = vect(data.frame("lon" = teethO$Lon, "lat" = teethO$Lat, 
                           "d18O" = teethO$d18O, "d18O.sd" = teethO$d18O.sd),
                crs = "WGS84")
teethOscape = calRaster(teethoxy, prpiso1)
teethO$residuals = teethOscape$lm.model$residuals

#Strontium Teeth Isoscape
teethSr <- subset(FTID, Element == 'teeth' & Isotope == '87Sr/86Sr') %>% 
  rename(Sr  = Iso.Value)
teethSr$Sr.sd <-0.0003
tSr <- vect(data.frame("lon" = teethSr$Lon, "lat" = teethSr$Lat, 
                        "Sr" = teethSr$Sr, "Sr.sd" = teethSr$Sr.sd), 
             crs = "WGS84")
teethSrscape = calRaster(tSr, Sriso)
teethSr$residuals = teethSrscape$lm.model$residuals
