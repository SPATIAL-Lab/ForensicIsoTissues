library(ggplot2); library(viridis);library(readr);library(terra); library(tidyterra)

#Maps/Spatial distribution of data

#This script is used after running the FITDataSetup script, 
#FTID can be run straight from the DataSetup as well without reading in the csv
FTID <-read_csv("data/ForensicTissue.csv")

#Base shapefile in AEA, read in map of North America from shapefile
NorAmericamap <-vect("shapefiles/Namap_aea.shp")

#Create spatvector to make maps from
FTID2 = vect(FTID, geom = c("Lon", "Lat"), crs = "WGS84")
FTID2 = terra::project(FTID2, crs(NorAmericamap))

#Create Map, distribution of oxygen hair (known and assumed)
ggplot() + 
  geom_sf(data = NorAmericamap) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="d18O" & FTID2$Element=="hair" & FTID2$Data.Origin == "known"), 
                  aes(color = "Known")) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="d18O" & FTID2$Element=="hair" & FTID2$Data.Origin == "known"), 
                  color = "black", shape = 1, size = 2.5) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="d18O" & FTID2$Element=="hair" & FTID2$Data.Origin == "assumed"), 
                  aes(color = "Assumed")) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="d18O" & FTID2$Element=="hair" & FTID2$Data.Origin == "assumed"), 
                  color= "black", shape = 1, size = 2.5) +
  scale_color_manual(name = "Legend", 
                     values = c(Known ="#7AD151FF", Assumed = "#414788FF")) +
  theme_void() + 
  theme(legend.box.background = element_rect(),legend.text = element_text(color = 'black'),
        legend.title = element_text(color = 'black'),
        legend.box.margin=margin(5,5,5,5), 
        legend.position = c(0.15, 0), legend.justification = c(0, 0)) 
ggsave("figures/Figure2B.png")


#Create Map, distribution of strontium hair (known and assumed)
ggplot() + 
  geom_sf(data = NorAmericamap) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="87Sr/86Sr" & FTID2$Element=="hair" & FTID2$Data.Origin == "known"), 
             aes(color = "Known"), shape = 15, size=2) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="87Sr/86Sr" & FTID2$Element=="hair" & FTID2$Data.Origin == "known"), 
             color = "black", shape = 0, size = 2.5)+
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="87Sr/86Sr" & FTID2$Element=="hair" & FTID2$Data.Origin == "assumed"), 
             aes( color = "Assumed"), shape = 15, size=2.5) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="87Sr/86Sr" & FTID2$Element=="hair" & FTID2$Data.Origin == "assumed"), 
             color = "black", shape = 0, size = 2)+
  scale_color_manual(name = "Legend", 
                     values = c(Known= "#7AD151FF", Assumed = "#414788FF")) +
  theme_void() + 
  theme(legend.box.background=element_rect(),legend.text = element_text(color = 'black'),
        legend.title = element_text(color = 'black'),
        legend.box.margin=margin(5,5,5,5), 
        legend.position = c(0.15, 0),legend.justification = c(0, 0))
ggsave("figures/Figure2A.png")


#Create Map, distribution of oxygen tooth enamel (known and assumed)
ggplot() + 
  geom_sf(data = NorAmericamap)+
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="d18O" & FTID2$Element=="teeth" & FTID2$Data.Origin == "known"), 
             aes( color = "Known")) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="d18O" & FTID2$Element=="teeth" & FTID2$Data.Origin == "known"), 
             color = "black", shape = 1, size = 2.5) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="d18O" & FTID2$Element=="teeth" & FTID2$Data.Origin == "assumed"), 
             aes( color = "Assumed")) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="d18O" & FTID2$Element=="teeth" & FTID2$Data.Origin == "assumed"), 
             color= "black", shape = 1, size = 2.5) +
  scale_color_manual(name = "Legend", 
                     values = c(Known= "#7AD151FF", Assumed = "#414788FF")) +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5),
        legend.position = c(0.15, 0),legend.justification = c(0, 0))
ggsave("figures/Figure2D.png")

#CreateMap, distribution of strontium tooth enamel (known and assumed)
ggplot() + 
  geom_sf(data = NorAmericamap)+
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="87Sr/86Sr" & FTID2$Element=="teeth" & FTID2$Data.Origin == "known"), 
             aes(color = "Known"), shape = 15, size=2.5) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="87Sr/86Sr" & FTID2$Element=="teeth" & FTID2$Data.Origin == "known"), 
             color = "black", shape = 0, size = 2.5)+
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="87Sr/86Sr" & FTID2$Element=="teeth" & FTID2$Data.Origin == "assumed"), 
             aes( color = "Assumed"), shape = 15, size=2.5) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="87Sr/86Sr" & FTID2$Element=="teeth" & FTID2$Data.Origin == "assumed"), 
             color = "black", shape = 0, size = 2.5)+
  scale_color_manual(name = "Legend", 
                     values = c(Known= "#7AD151FF", Assumed = "#414788FF")) +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5),
        legend.position = c(0.15, 0),legend.justification = c(0, 0))
ggsave("figures/Figure2C.png")






