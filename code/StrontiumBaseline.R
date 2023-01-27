library(raster);library(sf); library(readr)
#let's start using terra instead of raster, as it's meant to be a lighter weight and faster alternative
library(terra)


### Tried a clean slate following Gabe's setup, no dice. 
newcrs = "+proj=longlat +ellps=WGS84 +datum=WGS84"
strontium <- rast("shapefiles/rf_plantsoilmammal1.tif")
plot(strontium)
strontium <- project(strontium, newcrs)
ForensicTIsoData <- read_csv("data/ForensicIsoDataNew.csv", 
                             col_types = cols(...1 = col_skip()))

df <- SpatialPointsDataFrame(data.frame(ForensicTIsoData$Lon, ForensicTIsoData$Lat), 
                             ForensicTIsoData)
df2 <- vect(ForensicTIsoData, type = 'points', crs=newcrs)

dv = vect(df, crs="+proj=longlat +ellps=WGS84 +datum=WGS84")
dv = terra::project(dv, strontium)

de = extract(strontium, dv)

de

#wut
plot(dv)
par(new=TRUE)
plot(strontium)

### Old raster attempts
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


#load terra library (replacement ++ for raster)

library(terra)


#read and look

iso = rast("shapefiles/rf_plantsoilmammal1.tif")
plot(iso)

#I'll use assignR to get a set of points for extraction

library(assignR)

d = subOrigData(dataset = 2)



#Convert the SpatialPointsDataFrame contained in d into a terra spatial vector object

dv = vect(d$data)



#Project it to the map's coordinate system

dv = terra::project(dv, iso)



#Extract and have a look at the results

de = extract(iso, dv)

de