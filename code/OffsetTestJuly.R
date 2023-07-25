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

vect <- vect(data.frame("lon" = regularhair$Lon, "lat" = regularhair$Lat, 
                "d18O" = regularhair$d18O, "d18O.sd" = regularhair$d18O.sd), 
     crs = "WGS84")

# QA1
# This one might be quicker
test = QA(vect, prpiso2, bySite = FALSE, valiStation = 1, valiTime = 50, by = 2, mask = naMap, name = "test")
# this is more like the original code from the vignette, for better or worse
QA1 <- QA(vect, prpiso2, bySite = FALSE, valiStation = 1, valiTime = 50, 
            recal = TRUE, by = 2, prior = NULL, mask = naMap, setSeed = TRUE, 
            name = "QA1")

# Add residual mean to original data ("fix" the data)


# QA 2
