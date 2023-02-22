
# Setup -------------------------------------------------------------------
# libraries

library(terra);library(sf); library(readr); library(tidyterra)

# tissue data import and vectorize
ForensicTIsoData <- read_csv("data/ForensicIsoDataNew.csv", 
                             col_types = cols(...1 = col_skip()))


df <- vect(ForensicTIsoData, geom=c("Lon", "Lat"), 
           crs="+proj=longlat +datum=WGS84")

# strontium isoscape (from Bataille et al. 2020)

strontium <- rast("shapefiles/rf_plantsoilmammal1.tif")
plot(strontium)
df <- project(df, strontium)
isoscapeSr <- extract(strontium, df)

ggplot() + 
  geom_spatraster(data = strontium) +
  geom_spatvector(data = df) + 
  theme_void()

# oxygen isoscape from waterisotopes.org

oxygen <- rast("shapefiles/d18o_MA.tif")
plot(oxygen)
df <- project(df, oxygen)
isoscapeO <- extract(oxygen, df)

ggplot() + 
  geom_spatraster(data = oxygen) +
  geom_spatvector(data = df, color = 'white') + 
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
  geom_boxplot(outlier.shape = NA, aes(fill = Data.Origin)) + 
#  geom_jitter(aes(color = Data.Origin)) + 
  theme_classic()

# Strontium

ggplot(data = df_scape, aes(x = Element, y = deltaSr)) + 
  geom_boxplot(outlier.shape = NA, aes(fill = Data.Origin)) + 
  #  geom_jitter(aes(color = Data.Origin)) + 
  theme_classic()
