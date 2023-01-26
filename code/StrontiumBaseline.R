library(raster);library(sf); library(readr)

strontium <- raster("shapefiles/rf_plantsoilmammal1.tif")
plot(strontium)

ForensicTIsoData <- read_csv("data/ForensicIsoDataNew.csv", 
                             col_types = cols(...1 = col_skip()))

df <- SpatialPointsDataFrame(data.frame(ForensicTIsoData$Lon, ForensicTIsoData$Lat), 
                           proj4string=strontium@crs, ForensicTIsoData)

df$Sr <- raster::extract(strontium, df, 
                         buffer = 20000, 
                         fun = mean,
                         na.rm = T)
#Why all the NAs???
summary(strontium)
