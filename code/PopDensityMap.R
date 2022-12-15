# Let's make a population density map in our favorite colors

#libraries needed
library(tidyverse)
library(censusapi)
library(raster)
library(sf)
library(viridis)
library(tigris, options(tigris_use_cache = T))

#load the counties shapefile with some key census data 
us_counties <- counties(cb = TRUE)

#use censusapi to poll population by county from 2020 ACS Census
Sys.setenv(CENSUS_KEY = "7d9a4b25e4c9d0cced63abc32010591eac577c4e")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

acs_simple <- getCensus(
  name = "acs/acs5",
  vintage = 2020,
  vars = c("B01001_001E"),
  region =  "county:*") %>% 
  rename(pop = B01001_001E) %>% 
  unite(GEOID, c("state", "county"), sep = '')

#merge them, and drop a couple things. 
df <- dplyr::left_join(us_counties, acs_simple, by = "GEOID") %>% 
#drop STATEFP == 60 (Samoa), 66 (Guam), 69 (Northern Mariana Islands), 72 (Puerto Rico), and 78 (Virgin Islands)
  filter(STATEFP < 60) %>%
  dplyr::select(-c("COUNTYNS", "AFFGEOID", "LSAD", "AWATER"))

#calculate population density per sqkm
df$popdensity <- df$pop/(df$ALAND*0.000001)

df$densitybreaks <- cut(df$popdensity, breaks = c(0, 1, 10, 25, 100, 500, 1000, Inf), 
    labels = c("0 - 1", "1.1 - 10", "10.1 - 25",
               "25.1 - 100", "100.1 - 500", "500.1 - 1000", "> 1000")
    )
#write as a shapefile so moving the states around in the following code works
# only need to do this once, so commented out
#st_write(df, "shapefiles/PopDensity.shp")

############################
#fifty states transformation
############################
require(maptools)
require(rgdal)

fixup <- function(usa,alaskaFix,hawaiiFix){
  
  alaska=usa[usa$STATEFP=="02",]
  alaska = fix1(alaska,alaskaFix)
  proj4string(alaska) <- proj4string(usa)
  
  hawaii = usa[usa$STATEFP=="15",]
  hawaii = fix1(hawaii,hawaiiFix)
  proj4string(hawaii) <- proj4string(usa)
  
  usa = usa[! usa$STATEFP %in% c("02","15"),]
  usa = rbind(usa,alaska,hawaii)
  
  return(usa)
  
}

fix1 <- function(object,params){
  r=params[1];scale=params[2];shift=params[3:4]
  object = elide(object,rotate=r)
  size = max(apply(bbox(object),1,diff))/scale
  object = elide(object,scale=size)
  object = elide(object,shift=shift)
  object
}

us = readOGR("./shapefiles", "PopDensity")

usAEA = spTransform(us,CRS("+init=epsg:2163"))
usfix = fixup(usAEA, c(-35,1.8,-4000000,-130000),c(-35,1.4,-4000000,-1600000))
# parameters are rotations, scaling, x and y shift for Alaska and Hawaii respectively

usfix = st_as_sf(usfix) 

usfix$popdensity <- ordered(usfix$dnstybr, 
                            levels = c("0 - 1", "1.1 - 10", "10.1 - 25",
                                       "25.1 - 100", "100.1 - 500", "500.1 - 1000", "> 1000"
                            ))

ggplot() + 
  geom_sf(data = usfix, aes(fill = popdensity), color = 'grey4') + 
  scale_fill_viridis(discrete = T, option = "mako", direction = -1) + 
  labs(fill = expression(paste("Population Density, per km"^2))) + 
  theme_void() + 
  theme(legend.position="bottom")
