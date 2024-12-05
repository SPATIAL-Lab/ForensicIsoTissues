library (readr); library(assignR); library(terra); library(ggplot2); library(viridis);
library(dplyr)

#Isoscapes and QAs, includes Density plots of Know and Assumed origin residuals from isoscapes
#This script can be used after running the FITDataSetup script, 
#FTID can be run straight from the DataSetup as well without reading in the csv
FTID <-read_csv("data/ForensicTissue.csv")

#Base shapefile in AEA, read in map of North America from shapefile, this is done in previous Mapping script, but if not mapping, run for shapefile
NorAmericamap <-vect("shapefiles/Namap_aea.shp")
NAMAP = aggregate(NorAmericamap)
BufNAMAP <- buffer(NAMAP, 5e4) 

#Create Base Oxygen and Strontium Isoscapes, pull global precipitation isoscapes from assignR
prpiso = getIsoscapes("GlobalPrecipMA")
prpiso <- c(prpiso$d18o_MA, prpiso$d18o_se_MA)
prpiso1 = crop(prpiso, c( -180, -25, 0, 83.58326))
prpiso1 = terra::project(prpiso1, crs(NAMAP))
prpiso1 = crop(prpiso1, NAMAP)

Sriso = getIsoscapes("GlobalSr")
Sriso= crop(Sriso, c(-16653815.4396, 0, 0, 8376837.3753))
Sriso = terra::project(Sriso, crs(NAMAP))
Sriso <-terra::mask (Sriso, NAMAP)
Sriso = crop(Sriso, NAMAP)

#Osxygen Hair Isoscapes
#calibrated hairs, using RefTrans
calhair <- subset(FTID, Element == 'hair' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value,
         d18O_cal = Calibrate)
calhair$d18O.sd <-0.3
toTrans = data.frame(calhair[!is.na(calhair$d18O_cal),])
e = refTrans(toTrans, marker = "d18O", ref_scale = "VSMOW_O")
hsp.cal = vect(data.frame("lon" = e$data$Lon, "lat" = e$data$Lat, 
                          "d18O" = e$data$d18O, "d18O.sd" = e$data$d18O.sd), 
               crs = "WGS84")
hairscape.cal = calRaster(hsp.cal, prpiso1)
toTrans$residuals = hairscape.cal$lm.model$residuals

#Density plot of Known and Assumed origin residuals
# Calculate counts per Data.Origin group
counts <- toTrans %>%
  group_by(Data.Origin) %>%
  summarise(n = n())
# Create new labels with counts
new_labels <- paste0(counts$Data.Origin, " (n=", counts$n, ")")
names(new_labels) <- counts$Data.Origin
# Plot with updated legend labels that include counts
ggplot() + 
  geom_density(data = toTrans, aes(x = residuals, fill = Data.Origin, color = Data.Origin), 
               alpha = 0.7) +
  scale_fill_viridis(discrete = TRUE, option = 'C', labels = new_labels) + 
  scale_color_viridis(discrete = TRUE, option = 'C', labels = new_labels) + 
  labs(
    x = "Calibrated Oxygen Hair Isoscape Residuals", 
    y = "Density", 
    fill = "Data Origin",  # Legend title for the fill
    color = "Data Origin"  # Legend title for the color
  ) +
  theme(
    panel.background = element_rect(fill = 'white'),
    plot.background = element_rect(fill = 'transparent', color = NA),
    legend.background = element_rect(fill = 'transparent'),
    legend.box.background = element_rect(fill = 'transparent'),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )


ggplot() + 
  geom_density(data = toTrans, aes(x = residuals, fill = Data.Origin, 
                                    color = Data.Origin),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  labs(
    x = "Calibrated Oxygen Hair Isoscape Residuals", 
    y = "Density", )
ggsave("figures/Density_calibratehairresiduals.tiff")


#Oxygen Hair Isoscape uncalibrated/regular/no refTrans
regularhair <- subset(FTID, Element == 'hair' & Isotope == 'd18O')%>% 
  rename(d18O  = Iso.Value)
regularhair$d18O.sd <- 0.3
hsp.orig = vect(data.frame("lon" = regularhair$Lon, "lat" = regularhair$Lat, 
                           "d18O" = regularhair$d18O, "d18O.sd" = regularhair$d18O.sd), 
                crs = "WGS84")
hairscape.orig = calRaster(hsp.orig, prpiso1)
regularhair$residuals = hairscape.orig$lm.model$residuals

knownhair <- subset(FTID, Element == 'hair' & Isotope == 'd18O' & Data.Origin =="known")%>% 
  rename(d18O  = Iso.Value)
knownhair$d18O.sd <- 0.3
hsp.known = vect(data.frame("lon" = knownhair$Lon, "lat" = knownhair$Lat, 
                           "d18O" = knownhair$d18O, "d18O.sd" = knownhair$d18O.sd), 
                crs = "WGS84")
hairscape.known = calRaster(hsp.known, prpiso1)
knownhair$residuals = hairscape.known$lm.model$residuals

#Density plot of Known and Assumed origin residuals
ggplot() + 
  geom_density(data = regularhair, aes(x = residuals, fill = Data.Origin, 
                                       color = Data.Origin),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  labs(
    x = "Oxygen Hair Isoscape Residuals", 
    y = "Density", )
theme_dark()
ggsave("figures/Density_Ohairresiduals.tiff")

#Strontium Hair Isoscape
hairSr <- subset(FTID, Element == 'hair' & Isotope == '87Sr/86Sr') %>% 
  rename(Sr  = Iso.Value)
hairSr$Sr.sd <- 0.3
Srhair <- vect(data.frame("lon" = hairSr$Lon, "lat" = hairSr$Lat, 
                          "Sr" = hairSr$Sr, "Sr.sd" = hairSr$Sr.sd), 
               crs = "WGS84")
hairSrscape = calRaster(Srhair, Sriso)
hairSr$residuals = hairSrscape$lm.model$residuals

#Density plot of Known and Assumed origin residuals 
ggplot() + 
  geom_density(data = hairSr, aes(x = residuals, fill = Data.Origin, 
                                  color = Data.Origin),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  labs(
    x = "Sr Hair Isoscape Residuals", 
    y = "Density", )
theme_dark()
ggsave("figures/Density_Srhairresidual.tiff")

##Teeth
#Oxygen Tooth
teethO <- subset(FTID, Element == 'teeth' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value)
teethO$d18O.sd <- 0.3
teethoxy = vect(data.frame("lon" = teethO$Lon, "lat" = teethO$Lat, 
                           "d18O" = teethO$d18O, "d18O.sd" = teethO$d18O.sd),
                crs = "WGS84")
teethOscape = calRaster(teethoxy, prpiso1)
teethO$residuals = teethOscape$lm.model$residuals
teethO$isoscape.iso = teethOscape$lm.data$isoscape.iso
#Density plot of Known and Assumed origin residuals 
ggplot() + 
  geom_density(data = teethO, aes(x = residuals, fill = Data.Origin, 
                                    color = Data.Origin),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  labs(
    x = "Oxygen Teeth Isoscape Residuals", 
    y = "Density", )
theme_dark()
ggsave("figures/Density_Oteethresiduals.tiff")

#Quality Analysis for Oxygen Teeth, by reference and tooth group/type
#Adjusting isotopic values by reference residuals
teethO <- teethO %>% 
  group_by(Reference.ID) %>% 
  mutate(iso_By_Ref = d18O - mean(residuals))
#Adjusting isotopic values by tooth group by residuals
teethO <- teethO %>% 
  group_by(Tooth.group) %>% 
  mutate(iso_Tooth_group = d18O - mean(residuals))
#Assigning site ID
#SiteIDs for QA, QA can be run without, but improves the QA with
teethOxy.spuni = subset(teethoxy, !(duplicated(geom(teethoxy)[,3:4])))
si = match(geom(teethoxy)[,3] * geom(teethoxy)[,4],
           geom(teethOxy.spuni)[,3] * geom(teethOxy.spuni)[,4])
teethoxy$Site_ID=si
#QA 1
tOQA1 = QA(teethoxy, prpiso1, bySite = TRUE, valiStation = 1,
           valiTime = 50, by = 2, mask = BufNAMAP, name = "Oxygen Teeth")
plot(tOQA1)

# QA 2, add Site IDS adn adjusting isotopic values by residuals based on reference
teethoxy2 <- vect(data.frame("lon" = teethO$Lon, "lat" = teethO$Lat, 
                             "d18O" = teethO$iso_By_Ref, "d18O.sd" = teethO$iso_By_Ref), 
                  crs = "WGS84")
teethOxy.spuni1 = subset(teethoxy2, !(duplicated(geom(teethoxy2)[,3:4])))
si2 = match(geom(teethoxy2)[,3] * geom(teethoxy2)[,4],
            geom(teethOxy.spuni1)[,3] * geom(teethOxy.spuni1)[,4])
teethoxy2$Site_ID=si2

tOQA2 <- QA(teethoxy2, prpiso1, bySite = TRUE, valiStation = 1, valiTime = 50, 
            recal = TRUE, by = 2, prior = NULL, mask = BufNAMAP, setSeed = TRUE, 
            name = "Oxygen Teeth Reference")
# QA 3, add Site Ids, and adjusting isotopic values by residuals based on tooth group
teethoxy3 <- vect(data.frame("lon" = teethO$Lon, "lat" = teethO$Lat, 
                             "d18O" = teethO$iso_Tooth_group, "d18O.sd" = teethO$iso_Tooth_group), 
                  crs = "WGS84")
teethOxy.spuni2 = subset(teethoxy3, !(duplicated(geom(teethoxy3)[,3:4])))
si3 = match(geom(teethoxy3)[,3] * geom(teethoxy3)[,4],
            geom(teethOxy.spuni2)[,3] * geom(teethOxy.spuni2)[,4])
teethoxy3$Site_ID=si3
tOQA3 <- QA(teethoxy3, prpiso1, bySite = TRUE, valiStation = 1, valiTime = 50, 
            recal = TRUE, by = 2, prior = NULL, mask = BufNAMAP, setSeed = TRUE, 
            name = "Oxygen Teeth Tooth Group")
#comparisons between QAs
plot(tOQA1, tOQA2) 
plot(tOQA1, tOQA3)

###Just for now, but I don't know if we'll use#######
ggplot(data = teethO, aes(x=d18O, y=isoscape.iso, shape= Tooth.group, color=Country))+
  geom_point(size=2)+
  scale_color_manual(values= c("#B8De29FF", "#2d708eff", "#481567ff"))+
  labs(y="Isoscape value", x="Oxygen Isotopic Value")
ggsave("biplotisoscapevalueCountry.tiff")

ggplot(data = teethO, aes(x=d18O, y=cr3$lm.data$isoscape.iso, shape= Tooth.group2, color=Reference.ID))+geom_point(size=2)+
  scale_color_viridis(discrete = TRUE, option = 'D')+
  labs(y="Isoscape Value", x="Oxygen Isotopic Value")
ggsave("biplotteethRefIDtoothgroupisoscapevalues.tiff")

ggplot(data = teethO, aes(x=d18O, y=residuals, shape= Tooth.group2, color=Reference.ID))+
  geom_point(size=2)+
  scale_color_viridis(discrete = TRUE, option = 'D')+
  labs(y="Isoscape residual", x="Oxygen Isotopic Value")
ggsave("biplotresiduals.tiff")

#Strontium Teeth Isoscape
teethSr <- subset(FTID, Element == 'teeth' & Isotope == '87Sr/86Sr') %>% 
  rename(Sr  = Iso.Value)
teethSr$Sr.sd <-0.0003
tSr <- vect(data.frame("lon" = teethSr$Lon, "lat" = teethSr$Lat, 
                        "Sr" = teethSr$Sr, "Sr.sd" = teethSr$Sr.sd), 
             crs = "WGS84")
teethSrscape = calRaster(tSr, Sriso)
teethSr$residuals = teethSrscape$lm.model$residuals
#Density plot of Known and Assumed origin residuals 
ggplot() + 
  geom_density(data = teethSr, aes(x = residuals, fill = Data.Origin, 
                                    color = Data.Origin),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  labs(
    x = "Sr Teeth Isoscape Residuals", 
    y = "Density", )
theme_dark()
ggsave("figures/Density_Srteethresiduals.tiff")


