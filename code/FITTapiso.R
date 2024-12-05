library (readr); library(assignR); library(terra); library(ggplot2); library(viridis);
library(dplyr); library(tidyterra); library(geodata); library(car)

#Isoscapes, Statistical test and Quality Anaylsis
#This script is used after running the FITDataSetup script, 
#FTID can be run straight from the DataSetup without reading in the csv
FTID <-read_csv("data/ForensicTissue.csv")

#Shapefile for Isoscapes
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
#Pull North American tap and Global strontium isoscapes from assignR
###Ideally will re-write this to pull from assignR
NAtapiso <-rast("shapefiles/NAtap.tif")
NAtapiso <-c(NAtapiso$d18o.m, NAtapiso$d18o.se)

Sriso = getIsoscapes("GlobalSr")
Sriso= crop(Sriso, c(-16653815.4396, 0, 0, 8376837.3753))
Sriso = terra::project(Sriso, crs(namap1))
Sriso <-terra::mask (Sriso, namap1)
Sriso = crop(Sriso, namap1)

#Hair
#Oxygen Hair Isoscapes
#Calibrated hairs, using RefTrans
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
#Pull data out of isoscape
toTrans1$residuals = hairscape.cal$lm.model$residuals
toTrans1$isoscape.iso = hairscape.cal$lm.data$isoscape.iso
toTrans1$tissue.iso = hairscape.cal$lm.data$tissue.iso
#Calculate standard residual error for isoscape
sd(toTrans1$residuals)

# Test for normality
group1_h <- toTrans1$residuals[toTrans1$Data.Origin == "known"]
group2_h <- toTrans1$residuals[toTrans1$Data.Origin == "assumed"]
shapiro_group1_h <- shapiro.test(group1_h)
print(shapiro_group1_h)
shapiro_group2_h <- shapiro.test(group2_h)
print(shapiro_group2_h)
# Perform Levene's Test
levene_test_result_h <- leveneTest(residuals ~ Data.Origin, data = toTrans1)
print(levene_test_result_h)

#Oxygen Hair Isoscape (uncalibrated/regular/no refTrans)
regularhair <- subset(FTID, Element == 'hair' & Isotope == 'd18O')%>% 
  rename(d18O  = Iso.Value)
regularhair$d18O.sd <- 0.3
hsp.orig = vect(data.frame("lon" = regularhair$Lon, "lat" = regularhair$Lat, 
                           "d18O" = regularhair$d18O, "d18O.sd" = regularhair$d18O.sd), 
                crs = "WGS84")
hairscape.orig = calRaster(hsp.orig, NAtapiso)
#Pull data out of isoscape
regularhair$residuals = hairscape.orig$lm.model$residuals
regularhair$isoscape.iso = hairscape.orig$lm.data$isoscape.iso
regularhair$tissue.iso = hairscape.orig$lm.data$tissue.iso
#Calculate standard residual error for isoscape
sd(regularhair$residuals)

#Strontium Hair Isoscape
hairSr <- subset(FTID, Element == 'hair' & Isotope == '87Sr/86Sr') %>% 
  rename(Sr  = Iso.Value)
hairSr$Sr.sd <- 0.3
Srhair <- vect(data.frame("lon" = hairSr$Lon, "lat" = hairSr$Lat, 
                          "Sr" = hairSr$Sr, "Sr.sd" = hairSr$Sr.sd), 
               crs = "WGS84")
hairSrscape = calRaster(Srhair, Sriso)
hairSr$residuals = hairSrscape$lm.model$residuals
hairSr$isoscape.iso = hairSrscape$lm.data$isoscape.iso
hairSr$tissue.iso = hairSrscape$lm.data$tissue.iso
#Calculate standard residual error for isoscape
sd(hairSr$residuals)

#Teeth
#Tooth Enamel Oxygen Isoscape
teethO <- subset(FTID, Element == 'teeth' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value)
teethO$d18O.sd <- 0.3
teethoxy = vect(data.frame("lon" = teethO$Lon, "lat" = teethO$Lat, 
                           "d18O" = teethO$d18O, "d18O.sd" = teethO$d18O.sd),
              crs = "WGS84")
teethOscape = calRaster(teethoxy, NAtapiso)
#Pull data out of isoscape
teethO$residuals = teethOscape$lm.model$residuals
teethO$isoscape.iso = teethOscape$lm.data$isoscape.iso
teethO$tissue.iso = teethOscape$lm.data$tissue.iso
#Calculate standard residual error for isoscape
sd(teethO$residuals)

# Test for normality
group1 <- teethO$residuals[teethO$Data.Origin == "known"]
group2 <- teethO$residuals[teethO$Data.Origin == "assumed"]
shapiro_group1 <- shapiro.test(group1)
print(shapiro_group1)
shapiro_group2 <- shapiro.test(group2)
print(shapiro_group2)
# Perform Levene's Test
levene_test_result <- leveneTest(residuals ~ Data.Origin, data = teethO)
print(levene_test_result)

#Quality Analysis for Oxygen Teeth, by reference and tooth group/type
#Adjusting isotopic values by reference residuals
teethO <- teethO %>% 
tidyterra::group_by(Reference.ID) %>% 
mutate(iso_By_Ref = d18O - mean(residuals))
#Adjusting isotopic values by tooth group by residuals
teethO <- teethO %>% 
tidyterra::group_by(Tooth.group) %>% 
mutate(iso_Tooth_group = d18O - mean(residuals))
    
#Assigning site ID for QA1
teethOxy.spuni = subset(teethoxy, !(duplicated(geom(teethoxy)[,3:4])))
si = match(geom(teethoxy)[,3] * geom(teethoxy)[,4],
               geom(teethOxy.spuni)[,3] * geom(teethOxy.spuni)[,4])
teethoxy$Site_ID=si
#QA 1
tOQA1 = QA(teethoxy, NAtapiso, bySite = TRUE, valiStation = 1,
               valiTime = 500, by = 2, mask = Bufnamap1, name = "Oxygen Teeth")
    
# QA 2- Bias corrected using residual based on Reference ID
teethoxy2 <- vect(data.frame("lon" = teethO$Lon, "lat" = teethO$Lat, 
                             "d18O" = teethO$iso_By_Ref, "d18O.sd" = teethO$d18O.sd), 
                  crs = "WGS84")
#SiteIDs for QA2
teethOxy.spuni1 = subset(teethoxy2, !(duplicated(geom(teethoxy2)[,3:4])))
si2 = match(geom(teethoxy2)[,3] * geom(teethoxy2)[,4],
            geom(teethOxy.spuni1)[,3] * geom(teethOxy.spuni1)[,4])
teethoxy2$Site_ID=si2
#QA 2
tOQA2 <- QA(teethoxy2, NAtapiso, bySite = TRUE, valiStation = 1, valiTime = 500, 
                recal = TRUE, by = 2, prior = NULL, mask = Bufnamap1, setSeed = TRUE, 
                name = "Oxygen Teeth Reference")


# QA 3, Bias correct usind residuals based on tooth group
teethoxy3 <- vect(data.frame("lon" = teethO$Lon, "lat" = teethO$Lat, 
                             "d18O" = teethO$iso_Tooth_group, "d18O.sd" = teethO$d18O.sd), 
                  crs = "WGS84")
#QA 3, assign Site IDs
teethOxy.spuni2 = subset(teethoxy3, !(duplicated(geom(teethoxy3)[,3:4])))
si3 = match(geom(teethoxy3)[,3] * geom(teethoxy3)[,4],
            geom(teethOxy.spuni2)[,3] * geom(teethOxy.spuni2)[,4])
teethoxy3$Site_ID=si3
#QA 3
tOQA3 <- QA(teethoxy3, NAtapiso, bySite = TRUE, valiStation = 1, valiTime = 500, 
                recal = TRUE, by = 2, prior = NULL, mask = Bufnamap1, setSeed = TRUE, 
                name = "Oxygen Teeth Tooth Group")
#Plot the QAs  
plot.QA(tOQA1, tOQA2, tOQA3)

#Strontium Teeth Isoscape
teethSr <- subset(FTID, Element == 'teeth' & Isotope == '87Sr/86Sr') %>% 
rename(Sr  = Iso.Value)
teethSr$Sr.sd <-0.0003
tSr <- vect(data.frame("lon" = teethSr$Lon, "lat" = teethSr$Lat, 
                           "Sr" = teethSr$Sr, "Sr.sd" = teethSr$Sr.sd), 
                crs = "WGS84")
teethSrscape = calRaster(tSr, Sriso)
#Pull data out of isoscape
teethSr$residuals = teethSrscape$lm.model$residuals
teethSr$isoscape.iso = teethSrscape$lm.data$isoscape.iso
teethSr$tissue.iso = teethSrscape$lm.data$tissue.iso
#Calculate standard residual error for isoscape
sd(teethSr$residuals)

# Test for normality
group1_t <- teethSr$residuals[teethSr$Data.Origin == "known"]
group2_t <- teethSr$residuals[teethSr$Data.Origin == "assumed"]
shapiro_group1_t <- shapiro.test(group1_t)
print(shapiro_group1_t)
shapiro_group2_t <- shapiro.test(group2_t)
print(shapiro_group2_t)

# Perform Levene's Test
levene_test_result_t <- leveneTest(residuals ~ Data.Origin, data = teethSr)
print(levene_test_result_t)




    