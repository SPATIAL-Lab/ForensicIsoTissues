# Setup -------------------------------------------------------------------
# libraries

library(terra);library(sf); library(readr); library(tidyterra);
library(ggplot2); library(viridis)

# tissue data import and vectorize

ForensicTIsoData <- read_csv("data/ForensicIsoData.csv", 
                             col_types = cols(...1 = col_skip()))

df <- vect(ForensicTIsoData, geom=c("Lon", "Lat"), 
           crs="+proj=longlat +datum=WGS84")

# strontium isoscape (from Bataille et al. 2020)

strontium <- rast("shapefiles/rf_plantsoilmammal1.tif")

df <- project(df, strontium)
isoscapeSr <- extract(strontium, df)

# quick check that our points plot over the isoscape map
ggplot() + 
  geom_spatraster(data = strontium) +
  geom_spatvector(data = df, color = 'skyblue') + 
  theme_void()

# oxygen isoscape from waterisotopes.org
oxygen <- rast("shapefiles/d18o_MA.tif")
df <- project(df, oxygen)
isoscapeO <- extract(oxygen, df)

# quick check that our points plot over the isoscape map
ggplot() + 
  geom_spatraster(data = oxygen) +
  geom_spatvector(data = df, color = 'darkorange') + 
  theme_void()

# column binding for isoscape data

df_scape <- cbind(ForensicTIsoData, isoscapeO) %>% 
  cbind(isoscapeSr) %>% 
  select(-c(ID, ID)) %>% 
  rename(srscape = rf_plantsoilmammal1) %>% 
  mutate(Sr = ifelse(Isotope == '87Sr/86Sr', Iso.Value, NA_integer_)) %>% 
  mutate(O = ifelse(Isotope == 'd18O', Iso.Value, NA_integer_)) %>% 
  mutate(deltaSr = Sr - srscape) %>% 
  mutate(deltaO = O - d18o_MA)

# Comparing Isoscape Data -------------------------------------------------

# Oxygen

ggplot(data = df_scape, aes(x = Element, y = deltaO)) + 
  geom_violin(aes(fill = Data.Origin)) + 
  theme_classic()

ggplot() + 
  geom_density(data = df_scape, aes(x = deltaO, fill = Element, 
                                    color = Element),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'B') + 
  scale_color_viridis(discrete = T, option = 'B') + 
  theme_dark()

# check Chenery et al. estimated conversation for carbonate

# Strontium

ggplot() + 
  geom_density(data = df_scape, aes(x = deltaSr, fill = Element, 
                                    color = Element),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  theme_dark()
                 
# What's up with that twin tail for bone? 

bone <- subset(df_scape, Element == "bone", Sr =!is.na)
# ohhhhh there's only two samples

# Hair and Teeth only

hairteeth <- subset(df_scape, Element == 'hair' | Element == 'teeth')

ggplot() + 
  geom_density(data = hairteeth, aes(x = deltaSr, fill = Element, 
                                    color = Element),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  theme_dark()

ggplot() + 
  geom_density(data = hairteeth, aes(x = deltaO, fill = Element, 
                                     color = Element),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  theme_dark()

# Tissue Versus Isoscape

ggplot() + 
  geom_point(data = hairteeth, aes(x = deltaO, y = O, color = Element), size = 2) + 
  scale_color_manual(values = c("darkblue", "darkorange")) +  
  theme_classic() 
