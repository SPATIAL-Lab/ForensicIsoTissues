library(tidyverse);library(viridis);library(raster);library(sf)

ForensicTIsoData <- read_csv("data/ForensicIsoDataNew.csv", 
                             col_types = cols(...1 = col_skip()))
df <- st_as_sf(ForensicTIsoData, coords = c("Lon", "Lat"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +no_defs")

#this map is here just in case I want it. 
namap = st_read("shapefiles/bound_p.shp")
namap = namap1[namap1$COUNTRY %in% c("CAN", "MEX", "USA"), ]
namap <- st_transform(namap, CRS(
  #"+proj=aea +lat_1=29.5 +lat_2=42.5 +lon_0=-95"
  "+proj=longlat +datum=WGS84 +ellps=WGS84 +no_defs"
))

oxygen <- raster("shapefiles/d18o_MA.tif")
raster::crs(oxygen) <-"+proj=longlat +datum=WGS84 +ellps=WGS84 +no_defs"
#df and oxygen raster have the same CRS already, so no need to transform anything
strontium <- raster("shapefiles/rf_plantsoilmammal1.tif")
raster::crs(strontium) <-"+proj=longlat +datum=WGS84 +ellps=WGS84 +no_defs"


#na.rm = T just in case there's any empty data in the .tifs
df$Precip <- raster::extract(oxygen, df, weights = F, fun = mean(), 
                              na.rm = T)
df$Sr <- raster::extract(strontium, df, weights = F, fun = mean(), 
                         na.rm = T)

#BANANA why can't I extract Sr data??
sr <- as.data.frame(strontium)


# Oxygen Comparison -------------------------------------------------------
oxygenCompare <- subset(df, Isotope == 'd18O')

ggplot(data = oxygenCompare, aes(x = Iso.Value, y = Precip, color = Element)) +
  geom_point(size = 3) + 
  scale_color_manual(values= c("#000080", "#800080", "#008000")) + 
  labs(x = "Measured human value",
      y = "Predicted local precipitation"
      )+
  theme(legend.position = "top", 
      legend.text.align = 0, 
      legend.background = element_blank(), 
      legend.key = element_blank(), 
      legend.text = element_text(size = 16, color = "#222222"), 
      axis.title = element_text(size = 22, color = "#222222"), 
      axis.text = element_text(size = 18, color = "#222222"), 
      axis.text.x = element_text(margin = margin(5, b = 10)), 
      axis.ticks = element_blank(), 
      axis.line = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#cbcbcb"), 
      panel.grid.major.x = element_blank(), 
      panel.background = element_blank(),
      strip.background = element_rect(fill = "white"), 
      strip.text = element_text(size = 22, hjust = 0))

linear1 <- oxygenCompare %>% 
  filter(!is.na(Iso.Value)) %>% 
  filter(Element == 'bone')

linear2 <- oxygenCompare %>% 
  filter(!is.na(Iso.Value)) %>% 
  filter(Element == 'hair')

linear3 <- oxygenCompare %>% 
  filter(!is.na(Iso.Value)) %>% 
  filter(Element == 'teeth')

cor.test(linear1$Iso.Value, linear1$Precip, method = "pearson")
cor.test(linear2$Iso.Value, linear2$Precip, method = "pearson")
cor.test(linear3$Iso.Value, linear3$Precip, method = "pearson")


# Strontium Comparison ----------------------------------------------------
strontiumCompare <- subset(df, Isotope == '87Sr/86Sr')

ggplot(data = strontiumCompare, aes(x = Iso.Value, y = Sr, color = Element)) +
  geom_point(size = 3) + 
  scale_color_viridis(discrete = T, option = "mako") + 
  labs(x = "Measured human value",
       y = "Predicted baseline strontium"
  )+
  theme(legend.position = "top", 
        legend.text.align = 0, 
        legend.background = element_blank(), 
        legend.key = element_blank(), 
        legend.text = element_text(size = 16, color = "#222222"), 
        axis.title = element_text(size = 22, color = "#222222"), 
        axis.text = element_text(size = 18, color = "#222222"), 
        axis.text.x = element_text(margin = margin(5, b = 10)), 
        axis.ticks = element_blank(), 
        axis.line = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#cbcbcb"), 
        panel.grid.major.x = element_blank(), 
        panel.background = element_blank(),
        strip.background = element_rect(fill = "white"), 
        strip.text = element_text(size = 22, hjust = 0))

linear1 <- oxygenCompare %>% 
  filter(!is.na(Iso.Value)) %>% 
  filter(Element == 'bone')

linear2 <- oxygenCompare %>% 
  filter(!is.na(Iso.Value)) %>% 
  filter(Element == 'hair')

linear3 <- oxygenCompare %>% 
  filter(!is.na(Iso.Value)) %>% 
  filter(Element == 'teeth')

cor.test(linear1$Iso.Value, linear1$Precip, method = "pearson")
cor.test(linear2$Iso.Value, linear2$Precip, method = "pearson")
cor.test(linear3$Iso.Value, linear3$Precip, method = "pearson")

