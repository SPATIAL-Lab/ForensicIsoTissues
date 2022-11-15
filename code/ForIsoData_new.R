#Re-load and Re-run of descriptive statistics after addition of Tipple Data
ForIsoData <-read_xlsx("DataComp_22_11.xlsx")
FData1 <-read_xlsx("DataComp_22_11.xlsx", sheet = "Individual")
FData2 <-read_xlsx("DataComp_22_11.xlsx", sheet = "Site")
FData3 <-read_xlsx("DataComp_22_11.xlsx", sheet = "Sample")
FData4 <-read_xlsx("DataComp_22_11.xlsx", sheet = "Data")
Comp1 <-merge(FData2,FData3,by= "Site.ID")
Comp2 <-merge(Comp1,FData1,by= "Ind.ID")
Comp3 <-merge(Comp2,FData4,by= "Sample.ID")
#convert lat and long to numeric
Comp3$Lat= as.numeric(Comp3$Lat)
Comp3$Lon= as.numeric(Comp3$Lon)
ForensicTIsoData <-select(Comp3,1,2,3,5,6,7,8,9,10,11,12,14,15,16,22,23,24,28)
#get rid of NAs
ForensicTIsoData =ForensicTIsoData[!is.na(ForensicTIsoData$Lat),]
ForensicTIsoData =ForensicTIsoData[!is.na(ForensicTIsoData$Lon),]

Fspdf =SpatialPointsDataFrame(data.frame(ForensicTIsoData$Lon, ForensicTIsoData$Lat), ForensicTIsoData)
plot(Fspdf)
proj4string(Fspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

ForensicTIsoData$Cind = numeric(nrow(ForensicTIsoData))
ForensicTIsoData$Cind[ForensicTIsoData$Country == "Canada"] = 1
ForensicTIsoData$Cind[ForensicTIsoData$Country == "USA"] = 2
ForensicTIsoData$Cind[ForensicTIsoData$Country == "Mexico"] = 3

#Hair and Keratin Stats re-run, only USA, overall, known and assumed 
#some stats now? Descriptive Stats Take 1 Keratin/Oxygen
KeratinStats=ForensicTIsoData[ForensicTIsoData$Analyte=="keratin",]
KerOxy=KeratinStats[KeratinStats$Isotope=="d18O",]
KerOxy1=KerOxy$Iso.Value
mean(KerOxy1)
sd(KerOxy1)
range(KerOxy1)

KerOxyK=KerOxy[KerOxy$Data.Origin=="known",]
KerOxyK1=KerOxyK$Iso.Value
mean(KerOxyK1)
sd(KerOxyK1)
range(KerOxyK1)
KerSrK=KerSr[KerSr$Data.Origin=="known",]
KerSrK1=KerSrK$Iso.Value
mean(KerSrK1)
sd(KerSrK1)
range(KerSrK1)
KerOxyA=KerOxy[KerOxy$Data.Origin=="assumed",]
KerOxyA1=KerOxyA$Iso.Value
mean(KerOxyA1)
sd(KerOxyA1)
range(KerOxyA1)
KerSrA=KerSr[KerSr$Data.Origin=="assumed",]
KerSrA1=KerSrA$Iso.Value
mean(KerSrA1)
sd(KerSrA1)
range(KerSrA1)

HairStats=ForensicTIsoData[ForensicTIsoData$Element=="hair",]
HairOxy=HairStats[HairStats$Isotope=="d18O",]
HairOxy1=HairOxy$Iso.Value
mean(HairOxy1)
sd(HairOxy1)
range(HairOxy1)
HairSr=HairStats[HairStats$Isotope=="87Sr/86Sr",]
HairSr1=HairSr$Iso.Value
mean(HairSr1)
sd(HairSr1)
range(HairSr1)

HairOxyK=HairOxy[HairOxy$Data.Origin=="known",]
HairOxyK1=HairOxyK$Iso.Value
mean(HairOxyK1)
sd(HairOxyK1)
range(HairOxyK1)
HairSrK=HairSr[HairSr$Data.Origin=="known",]
HairSrK1=HairSrK$Iso.Value
mean(HairSrK1)
sd(HairSrK1)
range(HairSrK1)
HairOxyA=HairOxy[HairOxy$Data.Origin=="assumed",]
HairOxyA1=HairOxyA$Iso.Value
mean(HairOxyA1)
sd(HairOxyA1)
range(HairOxyA1)
HairSrA=HairSr[HairSr$Data.Origin=="assumed",]
HairSrA1=HairSrA$Iso.Value
mean(HairSrA1)
sd(HairSrA1)
range(HairSrA1)

NailStats=ForensicTIsoData[ForensicTIsoData$Element=="fingernail",]
HairOxy=HairStats[HairStats$Isotope=="d18O",]
HairOxy1=HairOxy$Iso.Value
mean(HairOxy1)
sd(HairOxy1)
range(HairOxy1)
HairSr=HairStats[HairStats$Isotope=="87Sr/86Sr",]
HairSr1=HairSr$Iso.Value
mean(HairSr1)
sd(HairSr1)
range(HairSr1)

HairUSA= HairStats[HairStats$Country=="USA",]
HairUSAOxy=HairUSA[HairUSA$Isotope=="d18O",]
HairUSASr=HairUSA[HairUSA$Isotope=="87Sr/86Sr",]

HairUSAOxy1=HairUSAOxy$Iso.Value
mean(HairUSAOxy1)
sd(HairUSAOxy1)
range(HairUSAOxy1)
HairUSASr1=HairUSASr$Iso.Value
mean(HairUSASr1)
sd(HairUSASr1)
range(HairUSASr1)
#known data is same
HairUSAOxyK=HairUSAOxy[HairUSAOxy$Data.Origin=="known",]
HairUSASrK=HairUSASr[HairUSASr$Data.Origin=="known",]

HairUSAOxyA=HairUSAOxy[HairUSAOxy$Data.Origin=="assumed",]
HairUSAOxyA1=HairUSAOxyA$Iso.Value
mean(HairUSAOxyA1)
sd(HairUSAOxyA1)
range(HairUSAOxyA1)
HairUSASrA=HairUSASr[HairUSASr$Data.Origin=="assumed",]
HairUSASrA1=HairUSASrA$Iso.Value
mean(HairUSASrA1)
sd(HairUSASrA1)
range(HairUSASrA1)

library(sf)
library(terra)
#Make some maps with new Hairs data
namap = readOGR("PoliticalBoundaries_Shapefiles/boundary_l_v2.shp")
plot(namap)
Fspdf=spTransform(Fspdf,crs(namap))

#subset map to get of weird lines, maybe, it worked but still a few weird lines
namap=namap[namap$COUNTRY %in% c("USA", "CAN USA", "MEX USA", "MEX", "CAN"),]
plot(namap)


plot(Fspdf[Fspdf$Isotope=="d18O" & Fspdf$Element=="hair",],pch=21, col="purple")
plot(namap, add=TRUE)
#color scale
colvals = palette.colors(7, "R4")
colind = Fspdf$Iso.Value[Fspdf$Isotope == "d18O" & Fspdf$Element == "hair"]
colind = ceiling((colind - min(colind)) / diff(range(colind)) * 6) + 1
plot(Fspdf[Fspdf$Isotope=="d18O" & Fspdf$Element=="hair",], pch=21, col="purple",
     bg = colvals[colind])
plot(namap, add=TRUE)
plot(Fspdf[Fspdf$Isotope=="d18O" & Fspdf$Element=="hair",], pch=21, col="purple",
     bg = colvals[colind], add = TRUE)
#making colors for map pretty colors
prettypurple=carto.pal(pal1 = "purple.pal", n1=6)
bluebaby =carto.pal(pal1 = "blue.pal", n1=6)
greenbean =carto.pal(pal1 = "green.pal", n1=6)

#MAP Sr for hairs, NEED CALCULATIONS for legend
colind = Fspdf$Iso.Value[Fspdf$Isotope == "87Sr/86Sr" & Fspdf$Element == "hair"]
colind = ceiling((colind - min(colind)) / diff(range(colind)) * 6) + 1
plot(Fspdf[Fspdf$Isotope=="87Sr/86Sr" & Fspdf$Element=="hair",], pch=21, col="darkgreen",
     bg = greenbean[colind])
plot(namap, add=TRUE)
plot(Fspdf[Fspdf$Isotope=="87Sr/86Sr" & Fspdf$Element=="hair",], pch=21, col="darkgreen",
     bg = greenbean[colind], add = TRUE)
#This needs some help
#Legend take2, this one is better, can put ranges in for legend, can move position
legendTypo(pos = "bottomleft", title.txt = "87Sr/86Sr Hair Values",
           col = carto.pal("green.pal", 6),
           categ = c("0.704243", "type 2", "type 3", "type 4", "0.71962"),
           nodata = FALSE)

#MAP Oxygen for hairs,  NEED CALCULATONS for legend
colind = Fspdf$Iso.Value[Fspdf$Isotope == "d18O" & Fspdf$Element == "hair"]
colind = ceiling((colind - min(colind)) / diff(range(colind)) * 6) + 1
plot(Fspdf[Fspdf$Isotope=="d18O" & Fspdf$Element=="hair",], pch=21, col="purple",
     bg = prettypurple[colind])
plot(namap, add=TRUE)
plot(Fspdf[Fspdf$Isotope=="87Sr/86Sr" & Fspdf$Element=="hair",], pch=21, col="purple",
     bg = prettypurple[colind], add = TRUE)
#This needs some help
#Legend take2, this one is better, can put ranges in for legend, can move position
legendTypo(pos = "bottomright", title.txt = "87Sr/86Sr Hair Values",
           col = carto.pal("purple.pal", 6),
           categ = c("number", "type 2", "type 3", "type 4", "number"),
           nodata = FALSE)
