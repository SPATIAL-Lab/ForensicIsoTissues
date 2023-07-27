#Fresh
library(assignR); library(readr); library(dplyr); library(terra)

prpiso = getIsoscapes("GlobalPrecipMA")
#prpiso <-  raster::stack(prpiso$d18o_MA, prpiso$d18o_se_MA)
prpiso <- c(prpiso$d18o_MA, prpiso$d18o_se_MA)


FTID <- read_csv("data/ForensicTissue4.csv", 
                 col_types = cols(...1 = col_skip()))
#isoscape uncalibrated oxygen hairs
regularhair <- subset(FTID, Element == 'hair' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value,
         d18O_cal = Calibrate)
regularhair$d18O.sd <- 0.3

rhair1 <- vect(data.frame("lon" = regularhair$Lon, "lat" = regularhair$Lat, 
                           "d18O" = regularhair$d18O, "d18O.sd" = regularhair$d18O.sd), 
                crs = "WGS84")

# QA1
# This one might be quicker
rhQA1 = QA(rhair1, prpiso, bySite = FALSE, valiStation = 1, valiTime = 50, by = 2, mask = naMap, name = "regular hair")
# this is more like the original code from the vignette, for better or worse
QA1 <- QA(rhair1, prpiso, bySite = FALSE, valiStation = 1, valiTime = 50, 
          recal = TRUE, by = 2, prior = NULL, mask = naMap, setSeed = TRUE, 
          name = "QA1")
plot(rhQA1)
# Add residual mean to original data ("fix" the data)
hsp.orig = vect(data.frame("lon" = regularhair$Lon, "lat" = regularhair$Lat, 
                           "d18O" = regularhair$d18O, "d18O.sd" = regularhair$d18O.sd), 
                crs = "WGS84")

hairscape.orig = calRaster(hsp.orig, prpiso)
fixeddf = cbind(regularhair, hairscape.orig$lm.model$residuals)

fixeddf <- fixeddf %>% 
  rename(Residuals = "hairscape.orig$lm.model$residuals")

# This summarize residuals and allows spot checking 
summaryfixed <- fixeddf %>% 
  group_by(Reference.ID) %>%
  summarize(across(Residuals, list(
    min = min, 
    max = max, 
    mean = mean,
    sd = sd
  )))

fixeddf <- fixeddf %>% 
  group_by(Reference.ID) %>% 
  mutate(Fix = d18O + mean(Residuals))

# QA 2
rhair2 <- vect(data.frame("lon" = fixeddf$Lon, "lat" = fixeddf$Lat, 
                           "d18O" = fixeddf$Fix, "d18O.sd" = fixeddf$d18O.sd), 
                crs = "WGS84")

rhQA2 <- QA(rhair2, prpiso, bySite = FALSE, valiStation = 1, valiTime = 50, 
          recal = TRUE, by = 2, prior = NULL, mask = naMap, setSeed = TRUE, 
          name = "regular hair adjusted")

plot(rhQA1, rhQA2)

#Oxygen teeth
teethO <- subset(FTID, Element == 'teeth' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value,
         d18O_cal = Calibrate)
teethO$d18O.sd <- 0.3

teethoxy = vect(data.frame("lon" = teethO$Lon, "lat" = teethO$Lat, 
                          "d18O" = teethO$d18O, "d18O.sd" = teethO$d18O.sd),
               crs = "WGS84")
teethOscape = calRaster(teethoxy, prpiso)

tOQA1 = QA(teethoxy, prpiso, bySite = FALSE, valiStation = 1,
           valiTime = 50, by = 2, mask = naMap, name = "Oxygen Teeth")
plot(tOQA1)
fixedtO = cbind(teethO, teethOscape$lm.model$residuals)

fixedtO <- fixedtO %>% 
  rename(Residuals = "teethOscape$lm.model$residuals")

# This summarize residuals and allows spot checking 
summaryfixedtO <- fixedtO %>% 
  group_by(Reference.ID) %>%
  summarize(across(Residuals, list(
    min = min, 
    max = max, 
    mean = mean,
    sd = sd
  )))

fixedtO <- fixedtO %>% 
  group_by(Reference.ID) %>% 
  mutate(Fix = d18O + mean(Residuals))

# QA 2
teethoxy2 <- vect(data.frame("lon" = fixedtO$Lon, "lat" = fixedtO$Lat, 
                          "d18O" = fixedtO$Fix, "d18O.sd" = fixedtO$d18O.sd), 
               crs = "WGS84")

tOQA2 <- QA(teethoxy2, prpiso, bySite = FALSE, valiStation = 1, valiTime = 50, 
            recal = TRUE, by = 2, prior = NULL, mask = naMap, setSeed = TRUE, 
            name = "Oxygen Teeth adjusted")

plot(tOQA1, tOQA2)

#Strontium time
Sr<- rast("shapefiles/GlobalSr/GlobalSr.tif")
Sr.sd<- rast("shapefiles/GlobalSr/GlobalSr_se.tif")
Sriso <- c(Sr, Sr.sd)

teethSr <- subset(FTID, Element == 'teeth' & Isotope == '87Sr/86Sr') %>% 
  rename(Sr  = Iso.Value)
teethSr$Sr.sd <-0.0003

tSr1 <- vect(data.frame("lon" = teethSr$Lon, "lat" = teethSr$Lat, 
                          "Sr" = teethSr$Sr, "Sr.sd" = teethSr$Sr.sd), 
               crs = "WGS84")
teethSrscape = calRaster(tSr1, Sriso)

tSrQA1 = QA(tSr1, Sriso, bySite = FALSE, valiStation = 1, valiTime = 50,
           by = 2, mask = naMap, name = "Strontium Teeth")
#Just the one
plot(tSrQA1)


#hair strontium
hairSr <- subset(FTID, Element == 'hair' & Isotope == '87Sr/86Sr') %>% 
  rename(Sr  = Iso.Value)
hairSr$Sr.sd <- 0.3

Srhair1 <- vect(data.frame("lon" = hairSr$Lon, "lat" = hairSr$Lat, 
                          "Sr" = hairSr$Sr, "Sr.sd" = hairSr$Sr.sd), 
               crs = "WGS84")

# QA1- only doing 1, can't adjust Strontium
hSrQA1 = QA(Srhair1, Sriso, bySite = FALSE, valiStation = 1, valiTime = 50, by = 2, mask = naMap, name = "Strontium hair")
plot(hSrQA1)

