library(ggplot2); library(viridis)
#Maps of data

#This script can be used after running the FITDataSetup script, 
#FTID can be run straight from the DataSetup as well without reading in the csv
FTID <-read_csv("data/ForensicTissue.csv")

FTID2 = vect(FTID, geom = c("Lon", "Lat"), crs = "WGS84")
FTID2 = project(FTID2, crs(NAMAP))

#Base shapefile in AEA, read in map of North America from shapefile
NorAmericamap <-vect("shapefiles/Namap_aea.shp")

#Map, distribution of oxygen hairs (known and assumed)
ggplot() + 
  geom_sf(data = NorAmericamap) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="d18O" & FTID2$Element=="hair" & FTID2$Data.Origin == "known"), 
                  aes(color = "Known")) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="d18O" & FTID2$Element=="hair" & FTID2$Data.Origin == "known"), 
                  color = "black", shape = 1, size = 2) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="d18O" & FTID2$Element=="hair" & FTID2$Data.Origin == "assumed"), 
                  aes(color = "Assumed")) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="d18O" & FTID2$Element=="hair" & FTID2$Data.Origin == "assumed"), 
                  color= "black", shape = 1, size = 2) +
  scale_color_manual(name = "Legend", 
                     values = c(Known = "#FDE725FF", Assumed = "#404788FF")) +
  labs(title = ("Oxygen Hair Samples"))+
  theme_void() + 
  theme(legend.box.background = element_rect(),legend.text = element_text(color = 'black'),
        legend.title = element_text(color = 'black'),
        legend.box.margin=margin(5,5,5,5), 
        legend.position = c(0.15, 0), legend.justification = c(0, 0)) 
ggsave("Map_hairoxygen.tiff")


#Map, distribution of strontium hairs (known and assumed)
ggplot() + 
  geom_sf(data = NorAmericamap) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="87Sr/86Sr" & FTID2$Element=="hair" & FTID2$Data.Origin == "known"), 
             aes(color = "Known"), shape = 15, size=2) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="87Sr/86Sr" & FTID2$Element=="hair" & FTID2$Data.Origin == "known"), 
             color = "black", shape = 0, size = 2)+
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="87Sr/86Sr" & FTID2$Element=="hair" & FTID2$Data.Origin == "assumed"), 
             aes( color = "Assumed"), shape = 15, size=2) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="87Sr/86Sr" & FTID2$Element=="hair" & FTID2$Data.Origin == "assumed"), 
             color = "black", shape = 0, size = 2)+
  scale_color_manual(name = "Legend", 
                     values = c(Known= "#FDE725FF", Assumed= "#404788FF")) +
  labs(title = "Strontium Hair Samples") +
  theme_void() + 
  theme(legend.box.background=element_rect(),legend.text = element_text(color = 'black'),
        legend.title = element_text(color = 'black'),
        legend.box.margin=margin(5,5,5,5), 
        legend.position = c(0.15, 0),legend.justification = c(0, 0))
ggsave("Map_KASrhair.tiff")



#Map, distribution of oxygen teeths (known and assumed)
ggplot() + 
  geom_sf(data = NorAmericamap)+
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="d18O" & FTID2$Element=="teeth" & FTID2$Data.Origin == "known"), 
             aes( color = "Known")) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="d18O" & FTID2$Element=="teeth" & FTID2$Data.Origin == "known"), 
             color = "black", shape = 1, size = 2) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="d18O" & FTID2$Element=="teeth" & FTID2$Data.Origin == "assumed"), 
             aes( color = "Assumed")) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="d18O" & FTID2$Element=="teeth" & FTID2$Data.Origin == "assumed"), 
             color= "black", shape = 1, size = 2) +
  scale_color_manual(name = "Legend", 
                     values = c(Known = "#FDE725FF", Assumed = "#404788FF")) +
  labs(title = "Oxygen Teeth Samples") +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5),
        legend.position = c(0.15, 0),legend.justification = c(0, 0))
ggsave("Map_KAoxygenteeth.tiff")

#Map, distribution of strontium teeths (known and assumed)
ggplot() + 
  geom_sf(data = NorAmericamap)+
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="87Sr/86Sr" & FTID2$Element=="teeth" & FTID2$Data.Origin == "known"), 
             aes(color = "Known"), shape = 15, size=2) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="87Sr/86Sr" & FTID2$Element=="teeth" & FTID2$Data.Origin == "known"), 
             color = "black", shape = 0, size = 2)+
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="87Sr/86Sr" & FTID2$Element=="teeth" & FTID2$Data.Origin == "assumed"), 
             aes( color = "Assumed"), shape = 15, size=2) +
  geom_spatvector(data = subset(FTID2, FTID2$Isotope=="87Sr/86Sr" & FTID2$Element=="teeth" & FTID2$Data.Origin == "assumed"), 
             color = "black", shape = 0, size = 2)+
  scale_color_manual(name = "Legend", 
                     values = c(Known= "#FDE725FF", Assumed= "#404788FF")) +
  labs(title = "Strontium Teeth Samples") +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5),
        legend.position = c(0.15, 0),legend.justification = c(0, 0))
ggsave("Map_KASrteeth.tiff")


#Biplot of teeth and hairs Latitude and Oxygen Isotope values
teethhair <- subset(FTID, Isotope == 'd18O')
ggplot(data = teethhair, aes(x=Iso.Value, y=Lat, color=Country, shape=Element))+geom_point(size=2)+
  scale_color_manual(values= c("#B8De29FF", "#2d708eff", "#481567ff"))+
  labs(y="Latitude", x="Isotopic Value")
ggsave("Biplot_AllOxygen_Latitude.tiff")
