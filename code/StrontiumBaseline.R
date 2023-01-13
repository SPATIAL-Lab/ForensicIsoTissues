library(raster);library(sf)

strontium <- raster("shapefiles/rf_plantsoilmammal1.tif")
plot(strontium)

ForensicTIsoData <- read_csv("data/ForensicIsoDataNew.csv", 
                             col_types = cols(...1 = col_skip()))
df =SpatialPointsDataFrame(data.frame(ForensicTIsoData$Lon, ForensicTIsoData$Lat), ForensicTIsoData)
plot(df)
proj4string(df) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
proj4string(strontium) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

df$Sr <- raster::extract(strontium, df, 
                          weights = F, fun = mean, 
                          na.rm = T)
#Why all the NAs???