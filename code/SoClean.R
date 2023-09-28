library(assignR); library(readr); library(dplyr); library(terra);
library(tidyterra); library(ggplot2)

FTID <- read_csv("data/ForensicTissue5.csv", 
                 col_types = cols(...1 = col_skip()))

#shapefile for maps (WGS84)
ggmap <-vect("shapefiles/NAmap_aea.shp")
ggmap = terra::project(ggmap, crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
plot(ggmap)
#shapefile for mask (Isoscapes, QA), shapefile in AEA, and buffered for QA and isoscapes
NorAmericamap <-vect("shapefiles/Namap_aea.shp")
NAMAP = aggregate(NorAmericamap)
BufNAMAP <- buffer(NAMAP, 5e4)
plot(NorAmericamap)
#Build base isoscapes
prpiso1 = getIsoscapes("GlobalPrecipMA")
prpiso1 <- c(prpiso1$d18o_MA, prpiso1$d18o_se_MA)
prpiso2= crop(prpiso1, c( -180, -25, 0, 83.58326))

prpiso3 = terra::project(prpiso2, crs(NAMAP))
plot(prpiso3)

prpiso4 <-terra::mask (prpiso3, NAMAP)
prpiso4 = crop(prpiso3, NAMAP)
plot(prpiso4)

#get assignR Sr shapefile codestuff
Sriso = getIsoscapes("GlobalSr")

#cropped
Sriso= crop(Sriso, c(-16653815.4396, 0, 0, 8376837.3753))
Sriso = terra::project(Sriso, crs(NAMAP))
Sriso1 <-terra::mask (Sriso, NAMAP)
plot(Sriso1)
Sriso3 = crop(Sriso1, NAMAP)
plot(Sriso3)

#Get points into projection
#Make data spatial
FTID = vect(FTID, geom = c("Lon", "Lat"), crs = "WGS84")
FTID = project(FTID, crs(NAMAP))

#Hair Oxygen, map, isoscape, residuals K&A, 
#Map, distribution of oxygen hairs (known and assumed)
ggplot() + 
  geom_sf(data = NorAmericamap) +
  geom_point(data = subset(FTID, FTID$Isotope=="d18O" & FTID$Element=="hair" & FTID$Data.Origin == "known"), 
             aes(color = "Known")) +
  geom_point(data = subset(FTID, Isotope=="d18O" & Element=="hair" & Data.Origin == "known"), 
             color = "black", shape = 1, size = 2) +
  geom_point(data = subset(FTID, Isotope=="d18O" & Element=="hair" & Data.Origin == "assumed"), 
             aes(color = "Assumed")) +
  geom_point(data = subset(FTID, Isotope=="d18O" & Element=="hair" & Data.Origin == "assumed"), 
             color= "black", shape = 1, size = 2) +
  scale_color_manual(name = "Legend", 
                     values = c(Known = "#FDE725FF", Assumed = "#404788FF")) +
  labs(title = ("Oxygen Hair Samples"))+
  theme_void() + 
  theme(legend.box.background = element_rect(),legend.text = element_text(color = 'black'),
        legend.title = element_text(color = 'black'),
        legend.box.margin=margin(5,5,5,5), 
        legend.position = c(0.15, 0), legend.justification = c(0, 0)) 
ggsave("Map_hair_oxygen.tiff")

#calibrated hairs, using RefTrans
calhair <- subset(FTID, FTID$Element == 'hair' & FTID$Isotope == 'd18O')
names(calhair)[names(calhair) == "Iso.Value"] = "d18O"
names(calhair)[names(calhair) == "Calibrate"] = "d18O_cal"
calhair = calhair[!is.na(calhair$d18O_cal)]
calhair$d18O.sd <-0.3
toTrans =data.frame(calhair)
e = refTrans(toTrans, marker = "d18O", ref_scale = "VSMOW_O")
e = data.frame(cbind(e$data, geom(calhair)[, 3:4]))

hsp.cal = vect(e, geom = c("x", "y"), crs(NAMAP))
hsp.cal = hsp.cal[!is.na(extract(prpiso4[[1]], hsp.cal, ID = FALSE, method = "bilinear"))]

ggplot() + 
  geom_sf(data = ggmap) +
  geom_point(data = subset(hsp.cal, Data.Origin == "known"), 
             aes(x = Lon, y = Lat, color = "Known")) +
  geom_point(data = subset(hsp.cal,Data.Origin == "known"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  geom_point(data = subset(hsp.cal, Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat, color = "Assumed")) +
  geom_point(data = subset(hsp.cal, Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat), color= "black", shape = 1, size = 2) +
  scale_color_manual(name = "Legend", 
                     values = c(Known = "#FDE725FF", Assumed = "#404788FF")) +
  labs(title = ("Oxygen Hair Samples"))+
  theme_void() + 
  theme(legend.box.background = element_rect(),legend.text = element_text(color = 'black'),
        legend.title = element_text(color = 'black'),
        legend.box.margin=margin(5,5,5,5), 
        legend.position = c(0.15, 0), legend.justification = c(0, 0)) 

#Version for calRaster

hairscape.cal = calRaster(subset(hsp.cal, !is.na(hsp.cal$Sample.ID), c("d18O", "d18O.sd")), prpiso4)

##add back residuals
hsp.cal$residuals = hairscape.cal$lm.model$residuals

##BANANA. It's not catching everything
calhair1 <- subset(FTID, Element == 'hair' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value,
         d18O_cal = Calibrate)
calhair1$d18O.sd <-0.3
toTrans1 = data.frame(calhair1[!is.na(calhair1$d18O_cal),])
calhairs1 = as.data.frame(toTrans1)
e1 = refTrans(toTrans1, marker = "d18O", ref_scale = "VSMOW_O")
hsp.cal1 = vect(data.frame("lon" = e1$data$Lon, "lat" = e1$data$Lat, 
                          "d18O" = e1$data$d18O, "d18O.sd" = e1$data$d18O.sd), 
               crs = "WGS84")
hairscape.cal1 = calRaster(hsp.cal, prpiso4)
hairscape.cal3 = calRaster(hsp.cal, prpiso3, mask = NAMAP)


e1 = terra::extract(prpiso3[[1]], hsp.cal)
e_test = e1[!is.na(e1),]
calhairs = calhairs[!is.na(e1),]
hsp.cal1 = hsp.cal[!is.na(e1),]

fixedchO = cbind(calhairs, hairscape.cal1$lm.model$residuals)
fixedchO <- fixedchO %>% 
  rename(Residuals = "hairscape.orig$lm.model$residuals")
ggplot() + 
  geom_density(data = fixedchO, aes(x = Residuals, fill = Data.Origin, 
                                   color = Data.Origin),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  labs(
    x = "Calibrated Oxygen Hair Isoscape Residuals", 
    y = "Density", )
ggsave("densitycalibratehairresiduals.tiff")




#isoscape uncalibrated/regular oxygen hairs
##BANANANNANANANANANA not of this is working right
regularhair <- subset(FTID, FTID$Element == 'hair' & FTID$Isotope == 'd18O') 
names(regularhair)[names(regularhair) == "Iso.Value"] = "d18O"
regularhair$d18O.sd <- 0.3
hsp.org1 =data.frame(regularhair)
hsp.org = data.frame(cbind(hsp.org1$data, geom(regularhair)[, 3:4]))
hsp.orig = vect(hsp.org, geom = c("x", "y"), crs(NAMAP))
hsp.orig = hsp.orig[!is.na(extract(prpiso4[[1]], hsp.orig, ID = FALSE, method = "bilinear"))]
hairscape.orig = calRaster(subset(hsp.orig, c("d18O", "d18O.sd")), prpiso4)

#old stuff
hsp.orig = vect(data.frame("lon" = regularhair$Lon, "lat" = regularhair$Lat, 
                           "d18O" = regularhair$d18O, "d18O.sd" = regularhair$d18O.sd), 
                crs = "WGS84")
hsp.orig1 = terra::project(hsp.orig, crs(NAMAP)) 
hairscape.orig = calRaster(hsp.orig1, prpiso4)
hairscape.orig1 = calRaster(hsp.orig1, prpiso4, mask =NAMAP)
fixeddf1 = cbind(regularhair, hairscape.orig$lm.model$residuals)

#fixeddf1 <- subset(fixeddf1, select = -c(as.data.frame))
fixeddf1 <- fixeddf1 %>% 
  rename(Residuals = "hairscape.orig$lm.model$residuals")




#Map, distribution of strontium hairs (known and assumed)
ggplot() + 
  geom_sf(data = ggmap) +
  geom_point(data = subset(FTID, Isotope=="87Sr/86Sr" & Element=="hair" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat, color = "Known"), shape = 15, size=2) +
  geom_point(data = subset(FTID, Isotope=="87Sr/86Sr" & Element=="hair" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat), color = "black", shape = 0, size = 2)+
  geom_point(data = subset(FTID, Isotope=="87Sr/86Sr" & Element=="hair" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat, color = "Assumed"), shape = 15, size=2) +
  geom_point(data = subset(FTID, Isotope=="87Sr/86Sr" & Element=="hair" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat), color = "black", shape = 0, size = 2)+
  scale_color_manual(name = "Legend", 
                     values = c(Known= "#FDE725FF", Assumed= "#404788FF")) +
  labs(title = "Strontium Hair Samples") +
  theme_void() + 
  theme(legend.box.background=element_rect(),legend.text = element_text(color = 'black'),
        legend.title = element_text(color = 'black'),
        legend.box.margin=margin(5,5,5,5), 
        legend.position = c(0.15, 0),legend.justification = c(0, 0))
ggsave("Map_hair_Sr.tiff")


#strontium hair isoscape 
hairSr <- subset(FTID, Element == 'hair' & Isotope == '87Sr/86Sr') %>% 
  rename(Sr  = Iso.Value)
hairSr$Sr.sd <- 0.3
Srhair <- vect(data.frame("lon" = hairSr$Lon, "lat" = hairSr$Lat, 
                           "Sr" = hairSr$Sr, "Sr.sd" = hairSr$Sr.sd), 
                crs = "WGS84")
Srhair = terra::project(Srhair, crs(NAMAP)) 
hairSrscape = calRaster(Srhair, Sriso)
hairSrscape1 = calRaster(Srhair, Sriso, mask = NAMAP)
fixedhSrdf = cbind(hairSr, hairSrscape$lm.model$residuals)
fixedhSrdf <- fixedhSrdf %>% 
  rename(Residuals = "hairSrscape$lm.model$residuals")

#Map, distribution of oxygen teeths (known and assumed)
ggplot() + 
  geom_sf(data = ggmap)+
  geom_point(data = subset(FTID, Isotope=="d18O" & Element=="teeth" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat, color = "Known")) +
  geom_point(data = subset(FTID, Isotope=="d18O" & Element=="teeth" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat), color = "black", shape = 1, size = 2) +
  geom_point(data = subset(FTID, Isotope=="d18O" & Element=="teeth" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat, color = "Assumed")) +
  geom_point(data = subset(FTID, Isotope=="d18O" & Element=="teeth" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat), color= "black", shape = 1, size = 2) +
  scale_color_manual(name = "Legend", 
                     values = c(Known = "#FDE725FF", Assumed = "#404788FF")) +
  labs(title = "Oxygen Teeth Samples") +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5),
        legend.position = c(0.30, 0),legend.justification = c(0, 0))
ggsave("Map_teeth_oxygen.tiff")
#isoscapes
teethO <- subset(FTID, Element == 'teeth' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value)
teethO$d18O.sd <- 0.3
teethoxy = vect(data.frame("lon" = teethO$Lon, "lat" = teethO$Lat, 
                           "d18O" = teethO$d18O, "d18O.sd" = teethO$d18O.sd),
                crs = "WGS84")
teethoxy1 = terra::project(teethoxy, crs(NAMAP)) 
teethOscape = calRaster(teethoxy1, prpiso4)
teethOscape1 = calRaster(teethoxy1, prpiso3, mask=NAMAP)
fixedtO = cbind(teethO, teethOscape$lm.model$residuals)
fixedtO <- fixedtO %>% 
  rename(Residuals = "teethOscape$lm.model$residuals")

#? teethOxygen = values(teethoxy1, as.data.frame = TRUE)

# This summarizes residuals and allows spot checking 
summaryfixedtO <- fixedtO %>% 
  group_by(Reference.ID) %>%
  summarize(across(Residuals, list(
    min = min, 
    max = max, 
    mean = mean,
    sd = sd
  )))

fixedtO <- fixedtO %>% 
  group_by(Reference.ID) %>% 
  mutate(Fix = d18O - mean(Residuals))

ggplot() +
  geom_density(data = fixedtO, aes(x = Residuals, 
                                   color = Tooth.group),
               alpha = 1) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  labs(
    x = "O Teeth Isoscape Residuals", 
    y = "Density", )+
  theme(plot.background = element_rect(fill='white'))
ggsave("Oteethtg.tiff")

fixedtO <- fixedtO %>% 
  group_by(Tooth.group) %>% 
  mutate(Fixtg = d18O - mean(Residuals))


#SiteID for QA
teethOxy.spuni = subset(teethoxy, !(duplicated(geom(teethoxy)[,3:4])))
si = match(geom(teethoxy)[,3] * geom(teethoxy)[,4],
           geom(teethOxy.spuni)[,3] * geom(teethOxy.spuni)[,4])
teethoxy$Site_ID=si
#QA 1
tOQA1 = QA(teethoxy, prpiso3, bySite = TRUE, valiStation = 1,
           valiTime = 50, by = 2, mask = BufNAMAP, name = "Oxygen Teeth")
plot(tOQA1)

# QA 2
teethoxy2 <- vect(data.frame("lon" = fixedtO$Lon, "lat" = fixedtO$Lat, 
                             "d18O" = fixedtO$Fix, "d18O.sd" = fixedtO$d18O.sd), 
                  crs = "WGS84")
teethoxy2 = terra::project(teethoxy2, crs(NAMAP)) 
teethoxy2$Site_ID=si2

teethOxy.spuni2 = subset(teethoxy3, !(duplicated(geom(teethoxy3)[,3:4])))
si3 = match(geom(teethoxy3)[,3] * geom(teethoxy3)[,4],
            geom(teethOxy.spuni2)[,3] * geom(teethOxy.spuni2)[,4])

teethoxy3 <- vect(data.frame("lon" = fixedtO$Lon, "lat" = fixedtO$Lat, 
                             "d18O" = fixedtO$Fixtg, "d18O.sd" = fixedtO$d18O.sd), 
                  crs = "WGS84")
teethoxy3 = terra::project(teethoxy3, crs(NAMAP)) 
teethOxy.spuni1 = subset(teethoxy2, !(duplicated(geom(teethoxy2)[,3:4])))
si2 = match(geom(teethoxy2)[,3] * geom(teethoxy2)[,4],
            geom(teethOxy.spuni1)[,3] * geom(teethOxy.spuni1)[,4])
teethoxy3$Site_ID=si3

tOQA2 <- QA(teethoxy2, prpiso3, bySite = TRUE, valiStation = 1, valiTime = 50, 
            recal = TRUE, by = 2, prior = NULL, mask = BufNAMAP, setSeed = TRUE, 
            name = "Oxygen Teeth Reference")

tOQA3 <- QA(teethoxy3, prpiso3, bySite = TRUE, valiStation = 1, valiTime = 50, 
            recal = TRUE, by = 2, prior = NULL, mask = BufNAMAP, setSeed = TRUE, 
            name = "Oxygen Teeth Tooth Group")

plot(tOQA1, tOQA2)
plot(tOQA1, tOQA3)


#Map, distribution of strontium teeths (known and assumed)
ggplot() + 
  geom_sf(data = ggmap)+
  geom_point(data = subset(FTID, Isotope=="87Sr/86Sr" & Element=="teeth" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat, color = "Known"), shape = 15, size=2) +
  geom_point(data = subset(FTID, Isotope=="87Sr/86Sr" & Element=="teeth" & Data.Origin == "known"), 
             aes(x = Lon, y = Lat), color = "black", shape = 0, size = 2)+
  geom_point(data = subset(FTID, Isotope=="87Sr/86Sr" & Element=="teeth" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat, color = "Assumed"), shape = 15, size=2) +
  geom_point(data = subset(FTID, Isotope=="87Sr/86Sr" & Element=="teeth" & Data.Origin == "assumed"), 
             aes(x = Lon, y = Lat), color = "black", shape = 0, size = 2)+
  scale_color_manual(name = "Legend", 
                     values = c(Known= "#FDE725FF", Assumed= "#404788FF")) +
  labs(title = "Strontium Teeth Samples") +
  theme_void() + 
  theme(legend.box.background=element_rect(),
        legend.box.margin=margin(5,5,5,5),
        legend.position = c(0.15, 0),legend.justification = c(0, 0))
ggsave("Map_teeth_sr.tiff")

#density plot of teeth Oxygen
ggplot() + 
  geom_density(data = fixedtO, aes(x = Residuals, fill = Data.Origin, 
                                  color = Data.Origin),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  labs(
    x = "Oxygen Teeth Isoscape Residuals", 
    y = "Density", )
theme_dark()
ggsave("Density_Oteeth_KA_residuals.tiff")

#density plot of teeth Oxygen
ggplot() + 
  geom_density(data = fixedtO, aes(x = Residuals, fill = Tooth.group, 
                                  color = Data.Origin),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'D') + 
  scale_color_viridis(discrete = T, option = 'D') + 
  labs(
    x = "Oxygen Teeth Isoscape Residuals", 
    y = "Density", )
ggsave("Density_Oteeth_KA_residuals.tiff")

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

#Strontium teeth isoscape
teethSr <- subset(FTID, Element == 'teeth' & Isotope == '87Sr/86Sr') %>% 
  rename(Sr  = Iso.Value)
teethSr$Sr.sd <-0.0003
tSr1 <- vect(data.frame("lon" = teethSr$Lon, "lat" = teethSr$Lat, 
                        "Sr" = teethSr$Sr, "Sr.sd" = teethSr$Sr.sd), 
             crs = "WGS84")
tSr1 = terra::project(tSr1, crs(NAMAP)) 
teethSrscape = calRaster(tSr1, Sriso)
teethSrscape1 = calRaster(tSr1, Sriso, mask = NAMAP)
fixedtSr = cbind(teethSr, teethSrscape$lm.model$residuals)
fixedtSr <- fixedtSr %>% 
  rename(Residuals = "teethSrscape$lm.model$residuals")

ggplot() + 
  geom_density(data = fixedtSr, aes(x = Residuals, fill = Reference.ID, 
                                    color = Reference.ID),
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'C') + 
  scale_color_viridis(discrete = T, option = 'C') + 
  labs(
    x = "Sr Hair Isoscape Residuals", 
    y = "Density", )

#Biplot of teeth and hairs Latitude and Oxygen Isotope values
teethhair <- subset(FTID, Element == 'hair' | Element == 'teeth')
teethhair <- subset(teethhair, Isotope == 'd18O')
ggplot(data = teethhair, aes(x=Iso.Value, y=Lat, color=Country, shape=Element))+geom_point(size=2)+
  scale_color_manual(values= c("#B8De29FF", "#2d708eff", "#481567ff"))+
  labs(y="Latitude", x="Isotopic Value")
ggsave("Biplot_AllOxygen_Latitude.tiff")


