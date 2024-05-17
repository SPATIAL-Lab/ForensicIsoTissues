library (readr); library(assignR); library(terra); library(ggplot2); library(viridis);
library(dplyr); library(tidyterra); library(geodata)

#Isoscapes and QAs, includes Density plots of Know and Assumed origin residuals from isoscapes
#This script can be used after running the FITDataSetup script, 
#FTID can be run straight from the DataSetup as well without reading in the csv
FTID <-read_csv("data/ForensicTissue.csv")

#Shapefile for isoscapes
worldvect <- world(path=tempdir())
namap <- subset(worldvect, worldvect$NAME_0 == "United States" | worldvect$NAME_0 == "Canada" | worldvect$NAME_0 == "Mexico")
namap1 = crop(namap, c(-180, -25, 0, 100))
namap1 = project (namap1, "ESRI:102008")
bb=ext(namap1)
xmin(bb)=-5e6
namap1=crop(namap1, bb)
namap1=aggregate(namap1)
Bufnamap1 <-buffer(namap1, 5e4)
plot(Bufnamap1)

#Create Base Oxygen and Strontium Isoscapes, 
#pull North American tap and Global strontium isoscapes from assignR
#Ideally will re-write this to pull from assignR
NAtapiso <-rast("shapefiles/NAtap.tif")
NAtapiso <-c(NAtapiso$d18o.m, NAtapiso$d18o.se)

Sriso = getIsoscapes("GlobalSr")
Sriso= crop(Sriso, c(-16653815.4396, 0, 0, 8376837.3753))
Sriso = terra::project(Sriso, crs(namap1))
Sriso <-terra::mask (Sriso, namap1)
Sriso = crop(Sriso, namap1)

#Hair
#Oxygen Hair Isoscapes
#calibrated hairs, using RefTrans
calhair <- subset(FTID, Element == 'hair' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value,
         d18O_cal = Calibrate)
calhair$d18O.sd <-0.3
toTrans1 = data.frame(calhair[!is.na(calhair$d18O_cal),])
e = refTrans(toTrans1, marker = "d18O", ref_scale = "VSMOW_O")
hsp.cal = vect(data.frame("lon" = e$data$Lon, "lat" = e$data$Lat, 
                          "d18O" = e$data$d18O, "d18O.sd" = e$data$d18O.sd), 
               crs = "WGS84")
hairscape.cal = calRaster(hsp.cal, NAtapiso)
toTrans1$residuals = hairscape.cal$lm.model$residuals
toTrans1$isoscape.iso = hairscape.cal$lm.data$isoscape.iso
#Calculate standard residual error for isoscape
sd(toTrans1$residuals)

kcalhair <- subset(toTrans1, Data.Origin =="known") 
hsp.cal1 = vect(data.frame("lon" = kcalhair$Lon, "lat" = kcalhair$Lat, 
                          "d18O" = kcalhair$d18O, "d18O.sd" = kcalhair$d18O.sd), 
               crs = "WGS84")
hairscape.cal1 = calRaster(hsp.cal1, NAtapiso)
toTrans2$residuals = hairscape.cal1$lm.model$residuals
toTrans2$isoscape.iso = hairscape.cal1$lm.data$isoscape.iso
#Calculate standard residual error for isoscape
sd(toTrans2$residuals)

#Density plot of Known and Assumed origin residuals
ggplot() + 
  geom_density(data = toTrans1, aes(x = residuals, 
                                   color = Data.Origin),linewidth=1,
               alpha = 0.7) +
  scale_fill_manual(values=c("#404788FF","#7AD151FF")) + 
  scale_color_manual(values= c("#404788FF","#7AD151FF")) + 
  labs(
    x = "Oxygen Hair Isoscape Residuals", 
    y = "Density", )+
  theme(
    panel.background = element_rect(fill='white'),
    plot.background = element_rect(fill='transparent', color=NA),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'),  
    legend.text = element_text(size =12,),
    legend.title = element_text(size =16,),
    axis.title.x = element_text(size =16,),
    axis.title.y = element_text(size =16,),
    axis.text.x = element_text(size =12,),
    axis.text.y = element_text(size =12,))
ggsave("figures/Density_calibratehairresiduals.png")


#Oxygen Hair Isoscape uncalibrated/regular/no refTrans
regularhair <- subset(FTID, Element == 'hair' & Isotope == 'd18O')%>% 
  rename(d18O  = Iso.Value)
regularhair$d18O.sd <- 0.3
hsp.orig = vect(data.frame("lon" = regularhair$Lon, "lat" = regularhair$Lat, 
                           "d18O" = regularhair$d18O, "d18O.sd" = regularhair$d18O.sd), 
                crs = "WGS84")
hairscape.orig = calRaster(hsp.orig, NAtapiso)
regularhair$residuals = hairscape.orig$lm.model$residuals
regularhair$isoscape.iso = hairscape.orig$lm.data$isoscape.iso
#Calculate standard residual error for isoscape
sd(regularhair$residuals)

knownhair <- subset(FTID, Element == 'hair' & Isotope == 'd18O' & Data.Origin =="known")%>% 
  rename(d18O  = Iso.Value)
knownhair$d18O.sd <- 0.3
hsp.known = vect(data.frame("lon" = knownhair$Lon, "lat" = knownhair$Lat, 
                            "d18O" = knownhair$d18O, "d18O.sd" = knownhair$d18O.sd), 
                 crs = "WGS84")
hairscape.known = calRaster(hsp.known, NAtapiso)
knownhair$residuals = hairscape.known$lm.model$residuals
#Calculate standard residual error for isoscape
sd(knownhair$residuals)

#Density plot of Known and Assumed origin residuals
ggplot() + 
  geom_density(data = regularhair, aes(x = residuals, 
                                       color = Data.Origin),linewidth=1,
               alpha = 0.7) +
  scale_fill_manual(values=c("#404788FF","#7AD151FF")) + 
  scale_color_manual(values= c("#404788FF","#7AD151FF")) + 
  labs(
    x = "Oxygen Hair Isoscape Residuals", 
    y = "Density", )+
  theme(
    panel.background = element_rect(fill='white'),
    plot.background = element_rect(fill='transparent', color=NA),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'),  
    legend.text = element_text(size =12,),
    legend.title = element_text(size =16,),
    axis.title.x = element_text(size =16,),
    axis.title.y = element_text(size =16,),
    axis.text.x = element_text(size =12,),
    axis.text.y = element_text(size =12,))
ggsave("figures/Density_Ohairresiduals.png")

#Strontium Hair Isoscape
hairSr <- subset(FTID, Element == 'hair' & Isotope == '87Sr/86Sr') %>% 
  rename(Sr  = Iso.Value)
hairSr$Sr.sd <- 0.3
Srhair <- vect(data.frame("lon" = hairSr$Lon, "lat" = hairSr$Lat, 
                          "Sr" = hairSr$Sr, "Sr.sd" = hairSr$Sr.sd), 
               crs = "WGS84")
hairSrscape = calRaster(Srhair, Sriso)
hairSr$residuals = hairSrscape$lm.model$residuals
#Density plot of Known and Assumed origin residuals not needed 

#Teeth
#Tooth Enamel Oxygen Isoscape
teethO <- subset(FTID, Element == 'teeth' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value)
teethO$d18O.sd <- 0.3
teethoxy = vect(data.frame("lon" = teethO$Lon, "lat" = teethO$Lat, 
                           "d18O" = teethO$d18O, "d18O.sd" = teethO$d18O.sd),
              crs = "WGS84")
teethOscape = calRaster(teethoxy, NAtapiso)
teethO$residuals = teethOscape$lm.model$residuals
teethO$isoscape.iso = teethOscape$lm.data$isoscape.iso
#Calculate standard residual error for isoscape
sd(teethO$residuals)

#known teeths only
kteethO <- subset(FTID, Element == 'teeth' & Isotope == 'd18O' & Data.Origin=="known") %>% 
  rename(d18O  = Iso.Value)
kteethO$d18O.sd <- 0.3
kteethoxy = vect(data.frame("lon" = kteethO$Lon, "lat" = kteethO$Lat, 
                           "d18O" = kteethO$d18O, "d18O.sd" = kteethO$d18O.sd),
                crs = "WGS84")
kteethOscape = calRaster(kteethoxy, NAtapiso)
kteethO$residuals = kteethOscape$lm.model$residuals
kteethO$isoscape.iso = kteethOscape$lm.data$isoscape.iso
#Calculate standard residual error for isoscape
sd(kteethO$residuals)
#Well that didn't improve. 

#Density plot of Known and Assumed origin residuals 
ggplot() + 
  geom_density(data = teethO, aes(x = residuals, 
                                  color = Data.Origin),linewidth=1,
               alpha = 0.7) +
  scale_fill_manual(values=c("#404788FF","#7AD151FF")) + 
  scale_color_manual(values= c("#404788FF","#7AD151FF")) + 
  labs(
    x = "Oxygen Teeth Isoscape Residuals", 
    y = "Density", )+
  theme(
    panel.background = element_rect(fill='white'),
    plot.background = element_rect(fill='transparent', color=NA),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'),  
    legend.text = element_text(size =12,),
    legend.title = element_text(size =16,),
    axis.title.x = element_text(size =16,),
    axis.title.y = element_text(size =16,),
    axis.text.x = element_text(size =12,),
    axis.text.y = element_text(size =12,))
theme_dark()
ggsave("figures/Density_Oteethresiduals.png")

#Density Plots- Reference ID and Tooth Group  
ggplot() + 
  geom_density(data = subset(teethO, !is.na(teethO$Tooth.group)), aes(x = residuals, 
                                                               color = Tooth.group), linewidth=1,
               alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'D') + 
  scale_color_viridis(discrete = T, option = 'D') + 
  labs(
    x = "Oxygen Teeth Isoscape Residuals", 
    y = "Density", )+
  theme(
    panel.background = element_rect(fill='white'),
    plot.background = element_rect(fill='transparent', color=NA),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'),  
    legend.text = element_text(size =12,),
    legend.title = element_text(size =16,),
    axis.title.x = element_text(size =16,),
    axis.title.y = element_text(size =16,),
    axis.text.x = element_text(size =12,),
    axis.text.y = element_text(size =12,))
ggsave("figures/Density_toothgroup.png")

ggplot() + 
  geom_density(data = subset(teethO, !is.na(Reference.ID)), aes(x = residuals, 
                                                            color = Reference.ID), linewidth=1, alpha = 0.7) +
  scale_fill_viridis(discrete = T, option = 'D') + 
  scale_color_viridis(discrete = T, option = 'D') + 
  labs(
    x = "Oxygen Teeth Isoscape Residuals", 
    y = "Density", )+
  theme(
    panel.background = element_rect(fill='white'),
    plot.background = element_rect(fill='transparent', color=NA),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'),  
    legend.text = element_text(size =12,),
    legend.title = element_text(size =16,),
    axis.title.x = element_text(size =16,),
    axis.title.y = element_text(size =16,),
    axis.text.x = element_text(size =12,),)
    ggsave("figures/Density_refID.png")
    
#Quality Analysis for Oxygen Teeth, by reference and tooth group/type
#Adjusting isotopic values by reference residuals
teethO <- teethO %>% 
tidyterra::group_by(Reference.ID) %>% 
mutate(iso_By_Ref = d18O - mean(residuals))
#Adjusting isotopic values by tooth group by residuals
teethO <- teethO %>% 
tidyterra::group_by(Tooth.group) %>% 
mutate(iso_Tooth_group = d18O - mean(residuals))
    
#Assigning site ID
#SiteIDs for QA, QA can be run without
teethOxy.spuni = subset(teethoxy, !(duplicated(geom(teethoxy)[,3:4])))
si = match(geom(teethoxy)[,3] * geom(teethoxy)[,4],
               geom(teethOxy.spuni)[,3] * geom(teethOxy.spuni)[,4])
teethoxy$Site_ID=si

#QA 1 without site ids
tOQA1 = QA(teethoxy, NAtapiso, bySite = FALSE, valiStation = 1,
            valiTime = 500, by = 2, mask = Bufnamap1, name = "Oxygen Teeth")
plot(tOQA1)
#QA 1 with site ids
tOQA1x = QA(teethoxy, NAtapiso, bySite = TRUE, valiStation = 1,
               valiTime = 500, by = 2, mask = Bufnamap1, name = "Oxygen Teeth")
plot(tOQA1x)
    
# QA 2
# QA 2,adjusting isotopic values by residuals based on reference
teethoxy2 <- vect(data.frame("lon" = teethO$Lon, "lat" = teethO$Lat, 
                             "d18O" = teethO$iso_By_Ref, "d18O.sd" = teethO$d18O.sd), 
                  crs = "WGS84")
#SiteIDs for QA, QA can be run without
teethOxy.spuni1 = subset(teethoxy2, !(duplicated(geom(teethoxy2)[,3:4])))
si2 = match(geom(teethoxy2)[,3] * geom(teethoxy2)[,4],
            geom(teethOxy.spuni1)[,3] * geom(teethOxy.spuni1)[,4])
teethoxy2$Site_ID=si2

#QA 2, without site Ids
tOQA2 <- QA(teethoxy2, NAtapiso, bySite = FALSE, valiStation = 1, valiTime = 500, 
             recal = TRUE, by = 2, prior = NULL, mask = Bufnamap1, setSeed = TRUE, 
             name = "Oxygen Teeth Reference")

#QA 2 with site Ids
tOQA2x <- QA(teethoxy2, NAtapiso, bySite = TRUE, valiStation = 1, valiTime = 500, 
                recal = TRUE, by = 2, prior = NULL, mask = Bufnamap1, setSeed = TRUE, 
                name = "Oxygen Teeth Reference")


# QA 3, add Site Ids, and adjusting isotopic values by residuals based on tooth group
teethoxy3 <- vect(data.frame("lon" = teethO$Lon, "lat" = teethO$Lat, 
                             "d18O" = teethO$iso_Tooth_group, "d18O.sd" = teethO$d18O.sd), 
                  crs = "WGS84")
teethOxy.spuni2 = subset(teethoxy3, !(duplicated(geom(teethoxy3)[,3:4])))
si3 = match(geom(teethoxy3)[,3] * geom(teethoxy3)[,4],
            geom(teethOxy.spuni2)[,3] * geom(teethOxy.spuni2)[,4])
teethoxy3$Site_ID=si3
#QA 3 without site Ids
tOQA3 <- QA(teethoxy3, NAtapiso, bySite = FALSE, valiStation = 1, valiTime = 500, 
            recal = TRUE, by = 2, prior = NULL, mask = Bufnamap1, setSeed = TRUE, 
            name = "Oxygen Teeth Tooth Group")

#QA 3 with site ids
tOQA3x <- QA(teethoxy3, NAtapiso, bySite = TRUE, valiStation = 1, valiTime = 500, 
                recal = TRUE, by = 2, prior = NULL, mask = Bufnamap1, setSeed = TRUE, 
                name = "Oxygen Teeth Tooth Group")

#comparisons between QAs
plot(tOQA1, tOQA2) 
plot(tOQA1, tOQA3)
plot(tOQA1, tOQA2, tOQA3)
#With Site Ids is better    
plot(tOQA1x, tOQA2x)
plot(tOQA1x, tOQA2x, tOQA3x)   

#Strontium Teeth Isoscape
teethSr <- subset(FTID, Element == 'teeth' & Isotope == '87Sr/86Sr') %>% 
rename(Sr  = Iso.Value)
teethSr$Sr.sd <-0.0003
tSr <- vect(data.frame("lon" = teethSr$Lon, "lat" = teethSr$Lat, 
                           "Sr" = teethSr$Sr, "Sr.sd" = teethSr$Sr.sd), 
                crs = "WGS84")
teethSrscape = calRaster(tSr, Sriso)
teethSr$residuals = teethSrscape$lm.model$residuals
#Calculate standard residual error for isoscape
sd(teethSr$residuals)
    
   
#Density plot of Known and Assumed origin residuals 
ggplot() + 
geom_density(data = teethSr, aes(x = residuals, 
                                 color = Data.Origin),linewidth=1,
                   alpha = 0.7) +
scale_fill_manual(values=c("#404788FF","#7AD151FF")) + 
scale_color_manual(values= c("#404788FF","#7AD151FF")) + 
labs(
      x = "Sr Teeth Isoscape Residuals", 
      y = "Density", )+
      theme(
      panel.background = element_rect(fill='white'),
      plot.background = element_rect(fill='transparent', color=NA),
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent'),  
      legend.text = element_text(size =12,),
      legend.title = element_text(size =16,),
      axis.title.x = element_text(size =16,),
      axis.title.y = element_text(size =16,),
      axis.text.x = element_text(size =12,),
      axis.text.y = element_text(size =12,))
ggsave("figures/Density_Srteethresiduals1.png")
 

#Additional isoscapes, US Oxygen Teeth    
tapIso <-  getIsoscapes("USTap")
tapIso <- c(tapIso$d18o, tapIso$d18o_sd)
tapIso <- aggregate(tapIso, 4)
tapIso = project (tapIso, "ESRI:102008")

USteethonly <-subset(FTID,Element=="teeth" & Country=="USA" )

USteethOonly <- subset(USteethonly, Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value)
USteethOonly$d18O.sd <- 0.3
USteethoxy = vect(data.frame("lon" = USteethOonly$Lon, "lat" = USteethOonly$Lat, 
                             "d18O" = USteethOonly$d18O, "d18O.sd" = USteethOonly$d18O.sd),
                  crs = "WGS84")
USteethOscape = calRaster(USteethoxy, tapIso)


USteethOK <- subset(USteethonly, Isotope == 'd18O'& Data.Origin=="known") %>% 
  rename(d18O  = Iso.Value)
USteethOK$d18O.sd <- 0.3
USteethoxy = vect(data.frame("lon" = USteethOK$Lon, "lat" = USteethOK$Lat, 
                             "d18O" = USteethOK$d18O, "d18O.sd" = USteethOK$d18O.sd),
                  crs = "WGS84")
USteethOscape = calRaster(USteethoxy, tapIso)


    