library(assignR); library(readr); library(dplyr); library(terra); library(sp); library(rgdal)
library(tidyterra)

FTID <- read_csv("data/ForensicTissue5.csv", 
                 col_types = cols(...1 = col_skip()))
FTID1 <- FTID

Fspdf =SpatialPointsDataFrame(data.frame(FTID$Lon, FTID$Lat), FTID)
proj4string(Fspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#shapefile for maps
namap <-vect("shapefiles/NAmap_aea.shp")
ggmap = terra::project(namap, crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
plot(ggmap)

#FTID1 <- FTID1 %>% 
  dplyr::select(-c("Lat", "Lon")) %>% 
  mutate(Lon = unlist(map(FTID1$geometry,1)),
         Lat = unlist(map(FTID1$geometry,2)))



#Hair Oxygen, map, isoscape, residuals K&A, 
#Map, distribution of oxygen hairs (known and assumed)
ggplot() + 
  geom_sf(data = ggmap) +
  geom_point(data = subset(FTID1, Isotope=="d18O" & Element=="hair" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat, color = "Known")) +
  geom_point(data = subset(FTID1, Isotope=="d18O" & Element=="hair" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  geom_point(data = subset(FTID1, Isotope=="d18O" & Element=="hair" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat, color = "Assumed")) +
  geom_point(data = subset(FTID1, Isotope=="d18O" & Element=="hair" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat), color= "black", shape = 1, size = 2) +
  scale_color_manual(name = "Legend", 
                     values = c(Known = "#FDE725FF", Assumed = "#404788FF")) +
  labs(title = ("Oxygen Hair Samples"))+
  theme_void() + 
  theme(legend.box.background = element_rect(),legend.text = element_text(color = 'black'),
        legend.title = element_text(color = 'black'),
        legend.box.margin=margin(5,5,5,5), 
        legend.position = c(0.15, 0), legend.justification = c(0, 0)) 