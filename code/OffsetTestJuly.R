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

vectqa1 <- vect(data.frame("lon" = regularhair$Lon, "lat" = regularhair$Lat, 
                "d18O" = regularhair$d18O, "d18O.sd" = regularhair$d18O.sd), 
     crs = "WGS84")

# QA1
# This one might be quicker
#test = QA(vect, prpiso, bySite = FALSE, valiStation = 1, valiTime = 50, by = 2, mask = naMap, name = "test")
# this is more like the original code from the vignette, for better or worse
QA1 <- QA(vectqa1, prpiso, bySite = FALSE, valiStation = 1, valiTime = 50, 
            recal = TRUE, by = 2, prior = NULL, mask = naMap, setSeed = TRUE, 
            name = "QA1")

# Add residual mean to original data ("fix" the data)
rhspdf <- SpatialPointsDataFrame(data.frame(regularhair$Lon, regularhair$Lat),
                               data.frame(regularhair$d18O, regularhair$d18O.sd))
proj4string(rhspdf) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

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
vectqa2 <- vect(data.frame("lon" = fixeddf$Lon, "lat" = fixeddf$Lat, 
                        "d18O" = fixeddf$Fix, "d18O.sd" = fixeddf$d18O.sd), 
             crs = "WGS84")

QA2 <- QA(vectqa2, prpiso, bySite = FALSE, valiStation = 1, valiTime = 50, 
          recal = TRUE, by = 2, prior = NULL, mask = naMap, setSeed = TRUE, 
          name = "QA2")

plot(QA1, QA2)
