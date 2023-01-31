# This is a skeleton script for only the figures and stats we plan to include in the AAFS poster Feb 2023. 
# Setup -------------------------------------------------------------------
library(sp); library(rgdal); library(terra)
library(cartography); library(mapsf); library(sf); library(readr);
library(tidyverse)
# Chris: added line below to get started. 
ForensicTIsoData <- read_csv("data/ForensicIsoDataNew.csv", 
                             col_types = cols(...1 = col_skip()))
Fspdf =SpatialPointsDataFrame(data.frame(ForensicTIsoData$Lon, ForensicTIsoData$Lat), ForensicTIsoData)
plot(Fspdf)
proj4string(Fspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Changed readOGR file path to assume that wd is base folder not /data
namap = st_read("shapefiles/bound_p.shp")
namap = namap[namap$COUNTRY %in% c("CAN", "MEX", "USA"), ]
namap <- st_transform(namap, CRS(
  "+proj=aea +lat_1=29.5 +lat_2=42.5 +lon_0=-95"
  #"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
))
plot(namap)

#making colors for map pretty colors
prettypurple=carto.pal(pal1 = "purple.pal", n1=6)
bluebaby =carto.pal(pal1 = "blue.pal", n1=6)
greenbean =carto.pal(pal1 = "green.pal", n1=6)

# ggplot setup
ggmap = st_as_sf(namap)
df = st_as_sf(Fspdf)
df <- st_transform(df, crs ="+proj=aea +lat_1=29.5 +lat_2=42.5 +lon_0=-95")
df <- df %>% 
  dplyr::select(-c("Lat", "Lon")) %>% 
  mutate(Lon = unlist(map(df$geometry,1)),
         Lat = unlist(map(df$geometry,2)))

# ggplot Maps Hair -------------------------------------------------------------
# Map, all hair oxygen values
# PEACH: This is the only one I've even kind of played with. I added the white outline for 
# the political boundaries which I think will pop on the dark background, and that's honestly it so far. 
breaks <- cut(subset(df, Isotope=="d18O" & Element=="hair")$Iso.Value, 
              breaks = 6, 
              labels = c("-16.4 - -10.7", "10.7 - -5.2", "-5.2 - 0.5", 
                         "0.5 - 6.1", "6.1 - 11.7", "11.7 - 17.3"), 
              include.lowest = T)
ggplot() + 
  geom_sf(data = ggmap, color = 'white') +
  geom_point(data = subset(df, Isotope=="d18O" & Element=="hair"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
 geom_point(data = subset(df, Isotope=="d18O" & Element=="hair"), 
             aes(x = Lon, y = Lat, color = breaks)) +
  scale_color_manual(values = c(prettypurple)) +
  labs(color = expression(paste(delta^18, "O"))) +
  theme_void() + 
  theme(
    #legend box outline
    legend.box.background=element_rect(color = 'white'),
        legend.box.margin=margin(5,5,5,5), 
    #where we want the legend
        legend.position = 'left', 
    #legend text in white so it pops on the blue
        legend.text = element_text(color = 'white'), 
        legend.title = element_text(color = 'white'))

#saves to .png
ggsave("figures/OxygenHair.png")

# Map, all hair strontium values
breaks <- cut(subset(df, Isotope=="87Sr/86Sr" & Element=="hair")$Iso.Value, 
              breaks = 6, 
              labels = c("0.704 - 0.707", "0.707 - 0.709", "0.709 - 0.712", 
                         "0.712 - 0.714", "0.714 - 0.717", "0.717 - 0.720"), 
              include.lowest = T)
ggplot() + 
  geom_sf(data = ggmap) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="hair"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="hair"), 
             aes(x = Lon, y = Lat, color = breaks)) +
  scale_color_manual(values = c(greenbean)) +
  labs(color = expression(paste(""^{87},"Sr/"^86,"Sr"))) +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5))

#Map, distribution of known origin hairs 
ggplot() + 
  geom_sf(data = ggmap) +
  geom_point(data = subset(df, Isotope=="d18O" & Element=="hair" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat, color = "Oxygen")) +
  geom_point(data = subset(df, Isotope=="d18O" & Element=="hair" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="hair" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat, color = "Strontium")) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="hair" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  scale_color_manual(name = "Isotopes Type", 
                     values = c(Oxygen = "purple", Strontium = "#16642A")) +
  labs(title = "Distribution of known origin hair") +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5))

#Map, distribution of oxygen hairs (known and assumed)
ggplot() + 
  geom_sf(data = ggmap, alpha = 0) +
  geom_point(data = subset(df, Isotope=="d18O" & Element=="hair" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat, color = "Known")) +
  geom_point(data = subset(df, Isotope=="d18O" & Element=="hair" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  geom_point(data = subset(df, Isotope=="d18O" & Element=="hair" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat, color = "Assumed"), shape = 15) +
  scale_color_manual(name = "Isotopes Type", 
                     values = c(Known = "#DED5E7", Assumed = "#3C1D62")) +
  labs(title = "Sample Distribution of Hair Oxygen Samples, Known and Assumed") +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5))

#Map, distribution of strontium hairs (known and assumed)
ggplot() + 
  geom_sf(data = ggmap, alpha = 0) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="hair" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat, color = "Known")) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="hair" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="hair" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat, color = "Assumed"), shape = 15) +
  scale_color_manual(name = "Isotopes Type", 
                     values = c(Known = "#97C38B", Assumed = "#16642A")) +
  labs(title = "Sample Distribution of Strontium From Hair, Known and Assumed") +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5))

#Map, distribution of assumed origin hairs
ggplot() + 
  geom_sf(data = ggmap) +
  geom_point(data = subset(df, Isotope=="d18O" & Element=="hair" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat, color = "Oxygen")) +
  geom_point(data = subset(df, Isotope=="d18O" & Element=="hair" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="hair" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat, color = "Strontium")) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="hair" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  scale_color_manual(name = "Isotopes Type", 
                     values = c(Oxygen = "purple", Strontium = "#16642A")) +
  labs(title = "Sample Distribution of Hair, Assumed Origin") +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5))

# ggplot Maps Teeth -------------------------------------------------------
breaks <- cut(subset(df, Isotope=="d18O" & Element=="teeth")$Iso.Value, 
              breaks = 6, 
              labels = c("10.9 - 14.2", "14.2 - 17.5", "17.5 - 20.8", 
                         "20.8 - 24.0", "24.0 - 27.3", "27.3 - 30.6"), 
              include.lowest = T)
ggplot() + 
  geom_sf(data = ggmap) +
  geom_point(data = subset(df, Isotope=="d18O" & Element=="teeth"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  geom_point(data = subset(df, Isotope=="d18O" & Element=="teeth"), 
             aes(x = Lon, y = Lat, color = breaks)) +
  scale_color_manual(values = c(prettypurple)) +
  labs(color = expression(paste(delta^18, "O")), 
       title = "All teeth analyzed for oxygen") +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5))

# Map, all teeth strontium values
breaks <- cut(subset(df, Isotope=="87Sr/86Sr" & Element=="teeth")$Iso.Value, 
              breaks = 6, 
              labels = c("0.704 - 0.707", "0.707 - 0.709", "0.709 - 0.712", 
                         "0.712 - 0.714", "0.714 - 0.717", "0.717 - 0.720"), 
              include.lowest = T)
ggplot() + 
  geom_sf(data = ggmap) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="teeth"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="teeth"), 
             aes(x = Lon, y = Lat, color = breaks)) +
  scale_color_manual(values = c(greenbean)) +
  labs(color = expression(paste(""^{87},"Sr/"^86,"Sr")), 
       title = "All teeth analyzed for strontium") +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5))


#Map, distribution of known origin teeths 
ggplot() + 
  geom_sf(data = ggmap) +
  geom_point(data = subset(df, Isotope=="d18O" & Element=="teeth" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat, color = "Oxygen")) +
  geom_point(data = subset(df, Isotope=="d18O" & Element=="teeth" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="teeth" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat, color = "Strontium")) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="teeth" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  scale_color_manual(name = "Isotopes Type", 
                     values = c(Oxygen = "purple", Strontium = "#16642A")) +
  labs(title = "Distribution of known origin teeth") +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5))

#Map, distribution of oxygen teeths (known and assumed)
# GRAPE there's no assumed origin tooth samples

ggplot() + 
  geom_sf(data = ggmap) +
  geom_point(data = subset(df, Isotope=="d18O" & Element=="teeth" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat, color = "Known")) +
  geom_point(data = subset(df, Isotope=="d18O" & Element=="teeth" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  geom_point(data = subset(df, Isotope=="d18O" & Element=="teeth" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat, color = "Assumed"), shape = 15) +
  scale_color_manual(name = "Isotopes Type", 
                     values = c(Known = "#C4AED0", Assumed = "#3C1D62")) +
  labs(title = "Sample Distribution of Teeth Oxygen Samples, Known and Assumed") +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5))

#Map, distribution of strontium teeths (known and assumed)
# GRAPE there's no assumed origin tooth samples
ggplot() + 
  geom_sf(data = ggmap) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="teeth" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat, color = "Known")) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="teeth" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="teeth" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat, color = "Assumed"), shape = 15) +
  scale_color_manual(name = "Isotopes Type", 
                     values = c(Known = "#97C38B", Assumed = "#16642A")) +
  labs(title = "Sample Distribution of Teeth Strontium Samples, Known and Assumed") +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5))

#Map, distribution of assumed origin teeth
# GRAPE this is a blank map because there's no tooth samples with assumed origin
ggplot() + 
  geom_sf(data = ggmap) +
  geom_point(data = subset(df, Isotope=="d18O" & Element=="teeth" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat, color = "Oxygen")) +
  geom_point(data = subset(df, Isotope=="d18O" & Element=="teeth" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="teeth" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat, color = "Strontium")) +
  geom_point(data = subset(df, Isotope=="87Sr/86Sr" & Element=="teeth" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  scale_color_manual(name = "Isotopes Type", 
                     values = c(Oxygen = "purple", Strontium = "#16642A")) +
  labs(title = "Sample Distribution of Tooth Samples of Assumed Origin") +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5))



# Density Plots -----------------------------------------------------------

#by tissue type
ggplot() + 
  geom_density(data = subset(df, Isotope=="87Sr/86Sr"), 
               aes(x = Iso.Value, fill = Element, color = Element), alpha = 0.8
  )+ 
  scale_color_manual(values= c("#000080", "#800080", "#008000", "#727072")) +
  scale_fill_manual(values= c("#000080", "#800080", "#008000", "#727072")) +
  labs(
    x = expression(paste(""^{87},"Sr/"^86,"Sr")), 
    y = "Density", 
    color = "Tissue", 
    fill = "Tissue"
  ) + 
  theme_classic()

#all together now
ggplot() + 
  geom_density(data = subset(df, Isotope=="87Sr/86Sr"), 
               aes(x = Iso.Value), fill = "#BBDAAD", color = "#008000") + 
  labs(
    x = expression(paste(""^{87},"Sr/"^86,"Sr")), 
    y = "Density"
  ) + 
  theme_classic()
