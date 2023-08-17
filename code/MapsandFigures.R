#Let's start at the very beginning
library(terra); library(sp); library(sf); library(readr); library(tidyterra);
library(ggplot2); library(viridis); library(assignR); library(raster);
library(tidyr); library(dplyr);library(cartography):
library(rgdal);library(mapsf);library(tidyverse);library(tidyr);library(ggeasy)


FTID <- read_csv("ForensicTissue4.csv", 
                             col_types = cols(...1 = col_skip()))

FTID1 <- FTID
Fspdf =SpatialPointsDataFrame(data.frame(FTID$Lon, FTID$Lat), FTID)
plot(Fspdf)
proj4string(Fspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
plot(Fspdf)

#I don't know what this one if for
df <- vect(FTID, geom=c("Lon", "Lat"), 
            crs="+proj=longlat +datum=WGS84")

#important stuff for making maps
namap = st_read("~/Documents/R/ForensicIsoTissues/shapefiles/bound_p.shp")
namap = st_read("shapefiles/bound_p.shp")
namap = namap[namap$COUNTRY %in% c("CAN", "MEX", "USA"), ]
namap <- st_transform(namap, CRS(
  "+proj=aea +lat_1=29.5 +lat_2=42.5 +lon_0=-95"
  #"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
))
plot(namap)
ggmap = st_as_sf(namap)
FTID1 = st_as_sf(Fspdf)
FTID1 <- st_transform(FTID1, crs ="+proj=aea +lat_1=29.5 +lat_2=42.5 +lon_0=-95")
FTID1 <- FTID1 %>% 
  dplyr::select(-c("Lat", "Lon")) %>% 
  mutate(Lon = unlist(map(FTID1$geometry,1)),
         Lat = unlist(map(FTID1$geometry,2)))

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
ggsave("mapKAOxygenhair.tiff")


#Map, distribution of strontium hairs (known and assumed)
ggplot() + 
  geom_sf(data = ggmap) +
  geom_point(data = subset(FTID1, Isotope=="87Sr/86Sr" & Element=="hair" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat, color = "Known"), shape = 15, size=2) +
  geom_point(data = subset(FTID1, Isotope=="87Sr/86Sr" & Element=="hair" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat), color = "black", shape = 0, size = 2)+
  geom_point(data = subset(FTID1, Isotope=="87Sr/86Sr" & Element=="hair" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat, color = "Assumed"), shape = 15, size=2) +
  geom_point(data = subset(FTID1, Isotope=="87Sr/86Sr" & Element=="hair" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat), color = "black", shape = 0, size = 2)+
  scale_color_manual(name = "Legend", 
                     values = c(Known= "#FDE725FF", Assumed= "#404788FF")) +
  labs(title = "Strontium Hair Samples") +
  theme_void() + 
  theme(legend.box.background=element_rect(),legend.text = element_text(color = 'black'),
        legend.title = element_text(color = 'black'),
        legend.box.margin=margin(5,5,5,5), 
        legend.position = c(0.15, 0),legend.justification = c(0, 0))
ggsave("mapKASrhair.tiff")

#Map, distribution of oxygen teeths (known and assumed)
ggplot() + 
  geom_sf(data = ggmap)+
  geom_point(data = subset(FTID1, Isotope=="d18O" & Element=="teeth" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat, color = "Known")) +
  geom_point(data = subset(FTID1, Isotope=="d18O" & Element=="teeth" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  geom_point(data = subset(FTID1, Isotope=="d18O" & Element=="teeth" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat, color = "Assumed")) +
  geom_point(data = subset(FTID1, Isotope=="d18O" & Element=="teeth" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat), color= "black", shape = 1, size = 2) +
  scale_color_manual(name = "Legend", 
                     values = c(Known = "#FDE725FF", Assumed = "#404788FF")) +
  labs(title = "Oxygen Teeth Samples") +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5),
        legend.position = c(0.15, 0),legend.justification = c(0, 0))
ggsave("mapKAoxygenteeth.tiff")

#Map, distribution of strontium teeths (known and assumed)
ggplot() + 
  geom_sf(data = ggmap)+
  geom_point(data = subset(FTID1, Isotope=="87Sr/86Sr" & Element=="teeth" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat, color = "Known"), shape = 15, size=2) +
  geom_point(data = subset(FTID1, Isotope=="87Sr/86Sr" & Element=="teeth" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat), color = "black", shape = 0, size = 2)+
  geom_point(data = subset(FTID1, Isotope=="87Sr/86Sr" & Element=="teeth" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat, color = "Assumed"), shape = 15, size=2) +
  geom_point(data = subset(FTID1, Isotope=="87Sr/86Sr" & Element=="teeth" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat), color = "black", shape = 0, size = 2)+
  scale_color_manual(name = "Legend", 
                     values = c(Known= "#FDE725FF", Assumed= "#404788FF")) +
  labs(title = "Strontium Teeth Samples") +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5),
        legend.position = c(0.15, 0),legend.justification = c(0, 0))
ggsave("mapKASrteeth.tiff")


#Biplot of teeth and hairs
teethhair <- subset(FTID, Element == 'hair' | Element == 'teeth')
teethhair <- subset(teethhair, Isotope == 'd18O')
#Latitude and Oxygen Isotope values
ggplot(data = teethhair, aes(x=Iso.Value, y=Lat, color=Country, shape=Element))+geom_point(size=2)+
  scale_color_manual(values= c("#B8De29FF", "#2d708eff", "#481567ff"))+
  labs(y="Latitude", x="Isotopic Value")
ggsave("OxygenLatIsoValueBiplot.tiff")
#Elevation and Oxygen Isotope values
ggplot(data = teethhair, aes(x=Iso.Value, y=Elev, color=Country, shape=Element))+
  geom_point(size=2)+scale_color_manual(values= c("#B8De29FF", "#2d708eff", "#481567ff"))+
  labs(y="Elevation", x="Isotopic Value")
ggsave("BiplotOxygenElevIsoValue.tiff")

#No particularly useful
ggplot(data = teethhair, aes(x=Iso.Value, y=Lat, color=Reference.ID, shape=Element))+geom_point(size=2)+
  scale_color_viridis(discrete = TRUE, option = 'D')+
  labs(y="Latitude", x="Isotopic Value")
ggplot(data = teethhair, aes(x=Iso.Value, y=Elev, color=Reference.ID, shape=Element))+
  geom_point(size=2)+scale_color_viridis(discrete = TRUE, option = 'D')+
  labs(y="Elevation", x="Isotopic Value")

#biplot of hairs, reference/study and calibrate
hairOxy <-subset(FTID, Element=="hair" & Isotope =="d18O")
hairSr <-subset(FTID, Element=="hair" & Isotope =="87Sr/86Sr")

#sidetracked
hairycal <-subset(hairOxy, Calibrate!='CAN_O_6' & Calibrate!='IAEA_O_2' & Calibrate!='VSMOW_O')
ggplot(data = hairycal, aes(x=Iso.Value, y=Elev, color=Calibrate, shape=Reference.ID))+geom_point(size=2)+
  scale_color_manual(values= c("#B8De29FF", "#2d708eff", "#481567ff"))+
  labs(y="Latitude", x="Isotopic Value")
ggsave("biplotoxygentoothgroup.tiff")

ggplot(data = hairOxy, aes(x=Iso.Value, y=Elev, color=Tooth.group2, shape=Reference.ID))+geom_point(size=2)+
  scale_color_viridis(discrete = TRUE, option = 'D')+
  labs(y="Latitude", x="Isotopic Value")
ggsave("biplotoxygentoothgroup.tiff")
ggplot(data = hairSr, aes(x=Iso.Value, y=Lat, color=Tooth.group2, shape=Country))+geom_point(size=2)+
  scale_color_manual(values= c("#B8De29FF", "#2d708eff", "#481567ff"))+
  labs(y="Latitude", x="Isotopic Value")
ggsave("biplotSrtoothgroup.tiff")

#biplot of teeths, in tooth groups and reference/study groups
teethOxy <-subset(FTID, Element=="teeth" & Isotope =="d18O")
teethSr <-subset(FTID, Element=="teeth" & Isotope =="87Sr/86Sr")

ggplot(data = teethOxy, aes(x=Iso.Value, y=Lat, color=Tooth.group2, shape=Country))+geom_point(size=2)+
  scale_color_manual(values= c("#B8De29FF", "#2d708eff", "#481567ff"))+
  labs(y="Latitude", x="Isotopic Value")
ggsave("biplotoxygentoothgroup.tiff")
ggplot(data = teethSr, aes(x=Iso.Value, y=Lat, color=Tooth.group2, shape=Country))+geom_point(size=2)+
  scale_color_manual(values= c("#B8De29FF", "#2d708eff", "#481567ff"))+
  labs(y="Latitude", x="Isotopic Value")
ggsave("biplotSrtoothgroup.tiff")

ggplot(data = teethOxy, aes(x=Iso.Value, y=Lat, color=Reference.ID, shape=Country))+geom_point(size=2)+ 
  scale_color_viridis(discrete = TRUE, option = 'D')+
  labs(y="Latitude", x="Isotopic Value")
ggsave("biplotOxyRefIDcountry.tiff")
ggplot(data = teethOxy, aes(x=Iso.Value, y=Lat, color=Reference.ID, shape=Tooth.group2))+geom_point(size=2)+ 
  scale_color_viridis(discrete = TRUE, option = 'D')+
  labs(y="Latitude", x="Isotopic Value")
ggsave("biplotOxyRefIDteeth.tiff")
ggplot(data = teethSr, aes(x=Iso.Value, y=Lat, color=Reference.ID, shape=Country))+geom_point(size=2)+ 
  scale_color_viridis(discrete = TRUE, option = 'D')+
  labs(y="Latitude", x="Isotopic Value")
ggsave("biplotSrRefIDcountry.tiff")
ggplot(data = teethSr, aes(x=Iso.Value, y=Lat, color=Reference.ID, shape=Tooth.group2))+geom_point(size=2)+ 
  scale_color_viridis(discrete = TRUE, option = 'D')+
  labs(y="Latitude", x="Isotopic Value")
ggsave("biplotSrRefIDteeth.tiff")


#Density plot of tissues/elements
ggplot() + 
  geom_density(data = subset(FTID, Isotope=="87Sr/86Sr"), 
               aes(x = Iso.Value, fill = Element, color = Element), alpha = 0.8)+ 
  scale_color_manual(values= c("#B8De29FF", "#29af7fff", "#2d708eff", "#481567ff")) +
  scale_fill_manual(values= c("#B8De29FF", "#29af7fff", "#2d708eff", "#481567ff")) +
  labs(
    x = expression(paste(""^{87},"Sr/"^86,"Sr")), 
    y = "Density", 
    color = "Tissue", 
    fill = "Tissue") + 
  theme()
ggsave ('densitelementsSr.png')
ggplot() + 
  geom_density(data = subset(FTID, Isotope=="d18O"), 
               aes(x = Iso.Value, fill = Element, color = Element), alpha = 0.8)+ 
  scale_color_manual(values= c("#B8De29FF", "#29af7fff", "#2d708eff", "#481567ff")) +
  scale_fill_manual(values= c("#B8De29FF", "#29af7fff", "#2d708eff", "#481567ff")) +
  labs(
    x = "d18O", 
    y = "Density", 
    color = "Tissue", 
    fill = "Tissue") + 
  theme()
ggsave ('densityOelements.tiff')

#Isoscapes
#Calibrating the hair oxygen values to make the isoscape
calhairs <- subset(FTID, Element == 'hair' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value,
         d18O_cal = Calibrate)
calhairs$d18O.sd <-0.3
calhairs <- calhairs %>% drop_na(d18O_cal)
calhairs<- subset(calhairs, City!="Tofino")
toTrans = data.frame(calhairs[!is.na(calhairs$d18O_cal),])
e = refTrans(toTrans, marker = "d18O", ref_scale = "VSMOW_O")

ind = match(toTrans$Data.ID, e$data$Data.ID)
plot(toTrans$d18O, e$data$d18O[ind])
#Oxygen isoscape of calibrated data
prpiso = getIsoscapes("GlobalPrecipMA")
prpiso = stack(prpiso$d18o_MA, prpiso$d18o_se_MA)

hspdf =SpatialPointsDataFrame(data.frame(e$data$Lon, e$data$Lat),
                              data.frame(e$data$d18O, e$data$d18O.sd))

proj4string(hspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

hairscape= calRaster(hspdf, prpiso, mask = naMap)

#plot(density(hairscape$lm.model$residuals))
#plot(hairscape$lm.data$isoscape.iso, hairscape$lm.data$tissue.iso, xlab ="O Isoscape", ylab="O Teeth Data") 
#plot(density(hairscape$lm.model$residuals), main ="Density Plot- Calibrated Oxygen Hair Data",)

e1 = extract(prpiso[[1]], hspdf)
e = e[!is.na(e1),]

calhairs = calhairs[!is.na(e1),]
hspdf = hspdf[!is.na(e1),]
cr1 = calRaster(hspdf, prpiso)
calhairs = cbind(calhairs, cr1$lm.model$residuals)
calhairs =cbind(calhairs, cr1$lm.data$isoscape.iso)
#BANANA
#calibrated hair (cannot re-align with data to for known and assumed)
ggplot() + 
  geom_density(data = calhairs, aes(x = cr1$lm.model$residuals, fill = Data.Origin, 
                                       color = Data.Origin),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  labs(
    x = "Calibrated Oxygen Hair Isoscape Residuals", 
    y = "Density", )
ggsave("densitycalibratehairresiduals.tiff")

ggplot(data = calhairs, aes(x=d18O, y=cr1$lm.data$isoscape.iso, color=Country))+geom_point(size=2)+
  scale_color_manual(values= c("#B8De29FF", "#2d708eff", "#481567ff"))
ggplot(data = calhairs, aes(x=d18O, y=cr1$lm.data$isoscape.iso, color=Reference.ID))+geom_point(size=2)+
  scale_color_viridis(discrete = TRUE, option = 'D')+
  labs(y="Isoscape Value", x="Oxygen Isotopic Value")
ggsave("biplotcalhairsRefIDisoscapevalues.tiff")

#isoscape uncalibrated oxygen hairs
regularhair <- subset(FTID, Element == 'hair' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value,
         d18O_cal = Calibrate)
regularhair$d18O.sd <-0.3

rhspdf =SpatialPointsDataFrame(data.frame(regularhair$Lon, regularhair$Lat),
                               data.frame(regularhair$d18O, regularhair$d18O.sd))

proj4string(rhspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

reghairscape= calRaster(rhspdf, prpiso, mask = naMap)

e5 = extract(prpiso[[1]], rhspdf)
regularhair = regularhair[!is.na(e5),]
rhspdf = rhspdf[!is.na(e5),]
cr5 = calRaster(rhspdf, prpiso)
regularhair = cbind(regularhair, cr5$lm.model$residuals)
regularhair =cbind(regularhair, cr5$lm.data$isoscape.iso)

ggplot(data = regularhair, aes(x=d18O, y=cr5$lm.data$isoscape.iso, color=Country))+geom_point(size=2)+
  scale_color_manual(values= c("#B8De29FF", "#2d708eff", "#481567ff"))+
  labs(y="Isoscape Value", x="Oxygen Isotopic Value")
ggsave("biplotreghairisoscapevalueCountry.tiff")
ggplot(data = regularhair, aes(x=d18O, y=cr5$lm.data$isoscape.iso, color=Reference.ID))+geom_point(size=2)+
  scale_color_viridis(discrete = TRUE, option = 'D')+
  labs(y="Isoscape Value", x="Oxygen Isotopic Value")
ggsave("biplotreghairsRefIDisoscapevalues.tiff")

#density plot residuals hair oxygen known and assumed
ggplot() + 
  geom_density(data = regularhair, aes(x = cr5$lm.model$residuals, fill = Data.Origin, 
                                       color = Data.Origin),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  labs(
    x = "Oxygen Hair Isoscape Residuals", 
    y = "Density", )
theme_dark()
ggsave("Ohairresiduals.tiff")

#teeth time, teeth oxygen isoscape
teethO <- subset(FTID, Element == 'teeth' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value)
teethO$d18O.sd <-0.3

tspdf=SpatialPointsDataFrame(data.frame(teethO$Lon, teethO$Lat),
                              data.frame(teethO$d18O, teethO$d18O.sd))

proj4string(tspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

teethscape= calRaster(tspdf, prpiso, mask = naMap)

e2 = extract(prpiso[[1]], tspdf)
teethO = teethO[!is.na(e2),]
tspdf = tspdf[!is.na(e2),]
cr3 = calRaster(tspdf, prpiso)
teethO = cbind(teethO, cr3$lm.model$residuals)
teethO =cbind(teethO, cr3$lm.data$isoscape.iso)
#density plot of teeth Oxygen
ggplot() + 
  geom_density(data = teethO, aes(x = cr3$lm.model$residuals, fill = Data.Origin, 
                                 color = Data.Origin),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  labs(
    x = "Oxygen Teeth Isoscape Residuals", 
    y = "Density", )
theme_dark()
ggsave("Oteethresiduals.tiff")

#density plot of teeth Oxygen
ggplot() + 
  geom_density(data = teethO, aes(x = cr3$lm.model$residuals, fill = Tooth.group, 
                                 color = Data.Origin),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'D') + 
  scale_color_viridis(discrete = T, option = 'D') + 
  labs(
    x = "Oxygen Teeth Isoscape Residuals", 
    y = "Density", )
ggsave("OteethgroupKAresiduals.tiff")

ggplot() + 
  geom_density(data = teethO, aes(x = cr3$lm.model$residuals, 
                                    fill = Tooth.group),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'D') + 
  scale_color_viridis(discrete = T, option = 'D') + 
  labs(
    x = "Oxygen Teeth Isoscape Residuals", 
    y = "Density", )
ggsave("densityOteethgroupresiduals.tiff")

ggplot(data = teethO, aes(x=d18O, y=cr3$lm.data$isoscape.iso, shape= Tooth.group2, color=Country))+
  geom_point(size=2)+
  scale_color_manual(values= c("#B8De29FF", "#2d708eff", "#481567ff"))+
  labs(y="Isoscape value", x="Oxygen Isotopic Value")
ggsave("biplotisoscapevalueCountry.tiff")
ggplot(data = teethO, aes(x=d18O, y=cr3$lm.data$isoscape.iso, shape= Tooth.group2, color=Reference.ID))+geom_point(size=2)+
  scale_color_viridis(discrete = TRUE, option = 'D')+
  labs(y="Isoscape Value", x="Oxygen Isotopic Value")
ggsave("biplotteethRefIDtoothgroupisoscapevalues.tiff")

ggplot(data = teethO, aes(x=d18O, y=cr3$lm.model$residuals, shape= Tooth.group2, color=Reference.ID))+
  geom_point(size=2)+
  scale_color_viridis(discrete = TRUE, option = 'D')+
  labs(y="Isoscape residual", x="Oxygen Isotopic Value")
ggsave("biplotresiduals.tiff")

#strontium? VICTORY- an isoscape was created
Sr<- raster("shapefiles/GlobalSr/GlobalSr.tif")
Sr.sd<- raster("shapefiles/GlobalSr/GlobalSr_se.tif")
Sriso =stack(Sr, Sr.sd)

teethSr1 <- subset(FTID, Element == 'teeth' & Isotope == '87Sr/86Sr') %>% 
  rename(Sr  = Iso.Value)
teethSr1$Sr.sd <-0.0003

tSrspdf =SpatialPointsDataFrame(data.frame(teethSr1$Lon, teethSr1$Lat),
                                data.frame(teethSr1$Sr, teethSr1$Sr.sd))

proj4string(tSrspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

teethSrscape= calRaster(tSrspdf, Sriso, mask = naMap)

e4 = extract(Sriso[[1]], tSrspdf)
teethSr1 = teethSr1[!is.na(e4),]
tSrspdf = tSrspdf[!is.na(e4),]
cr4 = calRaster(tSrspdf, Sriso)
teethSr1 = cbind(teethSr1, cr4$lm.model$residuals)
teethSr1 =cbind(teethSr1, cr4$lm.data$isoscape.iso)

#density plot of teeth Sr
ggplot() + 
  geom_density(data = teethSr1, aes(x = cr4$lm.model$residuals, fill = Data.Origin, 
                                   color = Data.Origin),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  labs(
    x = "Sr Teeth Isoscape Residuals", 
    y = "Density", )
theme_dark()
ggsave("Srteethresiduals.tiff")

ggplot() + 
  geom_density(data = teethSr1, aes(x = cr4$lm.model$residuals, fill = Tooth.group, 
                                   color = Data.Origin),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  labs(
    x = "Sr Teeth Isoscape Residuals", 
    y = "Density", )
ggsave("densitytoothgroupKAresiduals.tiff")

ggplot() + 
  geom_density(data = teethSr1, aes(x = cr4$lm.model$residuals, fill = Tooth.group),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'D') + 
  scale_color_viridis(discrete = T, option = 'D') + 
  labs(
    x = "Sr Teeth Isoscape Residuals", 
    y = "Density", )
ggsave("densitytoothgroupresiduals.tiff")

ggplot(data = teethSr1, aes(x=Sr, y=cr4$lm.data$isoscape.iso, shape= Tooth.group2, color=Reference.ID))+
  geom_point(size=2)+
  scale_color_viridis(discrete = TRUE, option = 'D')+
  labs(y="Isoscape value", x="Strontium Isotopic Value")
ggsave("biplotSrtoothgroupRefIDisoscapevalue.tiff")

#Hair stront
Srhair <- subset(FTID, Element == 'hair' & Isotope == '87Sr/86Sr') %>% 
  rename(Sr  = Iso.Value)
Srhair$Sr.sd <-0.0003

hSrspdf =SpatialPointsDataFrame(data.frame(Srhair$Lon, Srhair$Lat),
                                data.frame(Srhair$Sr, Srhair$Sr.sd))

proj4string(hSrspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

hairSrscape= calRaster(hSrspdf, Sriso, mask = naMap)

e6 = extract(Sriso[[1]], hSrspdf)
Srhair = Srhair[!is.na(e6),]
hSrspdf = hSrspdf[!is.na(e6),]
cr2 = calRaster(hSrspdf, Sriso)
Srhair = cbind(Srhair, cr2$lm.model$residuals)

#density plot of residuals hair strontium known and assumed
ggplot() + 
  geom_density(data = Srhair, aes(x = cr2$lm.model$residuals, fill = Data.Origin, 
                                  color = Data.Origin),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  labs(
    x = "Sr Hair Isoscape Residuals", 
    y = "Density", )
theme_dark()
ggsave("Srhairresidual.tiff")




