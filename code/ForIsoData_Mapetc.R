
library(maptools)
library(sp)
library(assignR)
library(rgdal)
library(raster)
library(dplyr)
library(assignR)
library(raster)
library(rgeos)
library(lattice)
library(rgbif)
library(cartography)
library(sf)
library(mapsf)
library(ggplot2)
read_excel(Coords.xlsx)
## from the rgbif package, pull Elevations from Geonames, some got a little hinky, but overall it worked
CoordsNoE <- Coords %>%
  filter(is.na(Elevation)) %>%
  mutate(
    decimalLatitude = Lat,
    decimalLongitude = Lon)
## VICTORY it worked to pull the elevations!!!
missingElevations<- elevation(CoordsNoE,username = "kverostick")

## get the elevations out?!?!?! WINNER WINNER CHICKEN DINNER!
write.csv(missingElevations, file = '/Users/kirstenverostick/Documents/R/IsoReviewData/CoordsE.csv')

#North America Shapefile, IGNORE THIS- not the new one
NorthA <- readOGR(dsn=path.expand("/Users/kirstenverostick/Documents/R/IsoReviewData"), layer= "NorthAmerica.shp")
NorthA <- st_read("NorthAmerica.shp")

#merging data comp sheets, don't really know why this is here
Data11 <-read_xlsx("DataComp_22_10.xlsx", sheet = "Individual")
Data21 <-read_xlsx("DataComp_22_10.xlsx", sheet = "Site")
Data31 <-read_xlsx("DataComp_22_10.xlsx", sheet = "Sample")
Data41 <-read_xlsx("DataComp_22_10.xlsx", sheet = "Data")
Comp11 <-merge(Data21,Data31,by= "Site.ID")
Comp21 <-merge(Comp11,Data11,by= "Ind.ID")
Comp31 <-merge(Comp21,Data41,by= "Sample.ID")
Comp31$Lat= as.numeric(Comp31$Lat)
Comp31$Lon= as.numeric(Comp31$Lon)

IsoData5 <-Comp31[!is.na(IsoData4$Lat),]
IsoData5 =Comp31[!is.na(IsoData4$Lon),]
write_csv(IsoData5, file = '/Users/kirstenverostick/Documents/R/IsoReviewData/IsoData5.csv')
IsoData6 <-read_csv("IsoData6.csv")
IsoData6 =IsoData6[!is.na(IsoData6$Lat),]
IsoData6 =IsoData6[!is.na(IsoData6$Lon),]
view(IsoData6)

write.csv(IsoData6, "ForensicIsoData.csv")

AllOData1=IsoData6[IsoData6$Isotope=="d18O",]
AllSrData1=IsoData6[IsoData6$Isotope=="87Sr/86Sr",]

spdf3 =SpatialPointsDataFrame(data.frame(IsoData6$Lon, IsoData6$Lat), IsoData6)
plot(spdf3)
proj4string(spdf3) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
plot(spdf3)

plot(naMap)

plot(spdf3, add=TRUE)
#do a selection of the data to access part of it
#making some maps,OLD NA PROJECTION FROM ASSIGNR
plot(spdf3,pch=21, col="gray")
plot(naMap, add=TRUE)

plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="teeth",],pch=21)
plot(naMap, add=TRUE)

#Teeth
plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="teeth",],pch=21, col="purple")
plot(naMap, add=TRUE)
plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="teeth",],pch=21, col="green")
plot(naMap, add=TRUE)
#Hair
plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="hair",],pch=21, col="purple")
plot(naMap, add=TRUE)
plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="hair",],pch=21, col="green")
plot(naMap, add=TRUE)
#Bone
plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="bone",],pch=21, col="purple")
plot(naMap, add=TRUE)
##not useful,only about two points
plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="bone",],pch=21, col="green")
plot(naMap, add=TRUE)
#Nail Ugh, just SLC....
plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="fingernail",],pch=21, col="green")
plot(naMap, add=TRUE)

plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="hair",],pch=21, col="red")
plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="hair",],pch=21, col="blue",)
plot(naMap, add=TRUE)

##Make some plots
AllOData1=IsoData6[IsoData6$Isotope=="d18O",]
AllSrData1=IsoData6[IsoData6$Isotope=="87Sr/86Sr",]
#putting the countries in order by N-S
AllSrData1$Cind = numeric(nrow(AllSrData1))
AllSrData1$Cind[AllSrData1$Country == "Canada"] = 1
AllSrData1$Cind[AllSrData1$Country == "USA"] = 2
AllSrData1$Cind[AllSrData1$Country == "Mexico"] = 3

AllOData1$Cind = numeric(nrow(AllOData1))
AllOData1$Cind[AllOData1$Country == "Canada"] = 1
AllOData1$Cind[AllOData1$Country == "USA"] = 2
AllOData1$Cind[AllOData1$Country == "Mexico"] = 3


ggplot(data = AllOData1, aes(x=Iso.Value, y=Lat, color=Country, shape=Element))+geom_point()
ggplot(data = AllOData1, aes(x=Iso.Value, y=Elev, color=Country, shape=Element))+geom_point()

ggplot(data = AllSrData1, aes(x=Iso.Value, y=Lat, color=Cind, shape=Element))+geom_point()
ggplot(data = AllSrData1, aes(x=Iso.Value, y=Elev, color=Country, shape=Element))+geom_point()

USAOdata=AllOData1[AllOData1$Country=="USA",]
MexOdata=AllOData1[AllOData1$Country=="Mexico",]
CanOdata=AllOData1[AllOData1$Country=="Canada",]
##Bi-plots of data
#Very interesting
ggplot(data = USAOdata, aes(x=Iso.Value, y=Lat, color=State, shape=Element))+geom_point()
ggplot(data = USAOdata, aes(x=Iso.Value, y=Elev, color=Element))+geom_point()
ggplot(data = USAOdata, aes(x=Iso.Value, y=Lat, color=Location.Quality, shape=Element))+geom_point()
ggplot(data = USAOdata, aes(x=Iso.Value, y=Lat, color=Biomolecule, shape=Element))+geom_point()

ggplot(data = MexOdata, aes(x=Iso.Value, y=Lat, color=State, shape=Element))+geom_point()
ggplot(data = MexOdata, aes(x=Iso.Value, y=Elev, color=Element))+geom_point()

ggplot(data = CanOdata, aes(x=Iso.Value, y=Lat, color=State, shape=Element))+geom_point()
ggplot(data = CanOdata, aes(x=Iso.Value, y=Elev, color=Element))+geom_point()

USASrdata=AllSrData1[AllSrData1$Country=="USA",]
MexSrdata=AllSrData1[AllSrData1$Country=="Mexico",]
CanSrdata=AllSrData1[AllSrData1$Country=="Canada",]

#Somewhat interesting
ggplot(data = USASrdata, aes(x=Iso.Value, y=Lat, color=State, shape=Element))+geom_point()
ggplot(data = USASrdata, aes(x=Iso.Value, y=Elev, color=Element))+geom_point()

#Somewhat interesting
ggplot(data = MexSrdata, aes(x=Iso.Value, y=Lat, color=State, shape=Element))+geom_point()
ggplot(data = MexSrdata, aes(x=Iso.Value, y=Elev, color=Element))+geom_point()

#Not really useful
ggplot(data = CanSrdata, aes(x=Iso.Value, y=Lat, color=Element))+geom_point()
ggplot(data = CanSrdata, aes(x=Iso.Value, y=Elev, color=Element))+geom_point()

##more bi-plots
ggplot(data = MexO, aes(x=Iso.Value, y=Lat, color=Element))+geom_point()
ggplot(data = MexSr, aes(x=Iso.Value, y=Lat, color=Element, shape=Data.Origin))+geom_point()

ggplot(data = OHair, aes(x=Iso.Value, y=Lat, color=Country))+geom_point()
ggplot(data = OHair, aes(x=Iso.Value, y=Lat, color=Location.Quality, shape=Country))+geom_point()

#Suddenly this isn't work anymore? WTF. 
#IsoData5 <-select(Comp31,1,2,3,5,6,7,8,9,10,11,15,16,17,23,24,25,26,29)
#IsoData5 <- (1,2,3,5,6,7,8,9,10,11,15,16,17,23,24,25,26,29)
#IsoData5 %>% select_all(1,2,3,5,6,7,8,9,10,11,15,16,17,23,24,25,26,29)


#New North America Shape file
namap = readOGR("PoliticalBoundaries_Shapefiles/boundary_l_v2.shp")
plot(namap)
spdf3=spTransform(spdf3,crs(namap))
plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="hair",],pch=21, col="purple")
plot(namap, add=TRUE)
#subset map to get of weird lines, maybe, it worked but still a few weird lines
namap=namap[namap$COUNTRY %in% c("USA", "CAN USA", "MEX USA", "MEX", "CAN"),]

#color scale
colvals = palette.colors(7, "R4")
colind = spdf3$Iso.Value[spdf3$Isotope == "d18O" & spdf3$Element == "hair"]
colind = ceiling((colind - min(colind)) / diff(range(colind)) * 6) + 1
plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="hair",], pch=21, col="purple",
     bg = colvals[colind])
plot(namap, add=TRUE)
plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="hair",], pch=21, col="purple",
     bg = colvals[colind], add = TRUE)

#test run
colvals = palette.colors(7, "Tableau 10")
colind = spdf3$Iso.Value[spdf3$Isotope == "87Sr/86Sr" & spdf3$Element == "hair"]
colind = ceiling((colind - min(colind)) / diff(range(colind)) * 6) + 1
plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="hair",], pch=21, col="green",
     bg = colvals[colind])
plot(namap, add=TRUE)
plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="hair",], pch=21, col="green",
     bg = colvals[colind], add = TRUE)
#making colors for map pretty colors
prettypurple=carto.pal(pal1 = "purple.pal", n1=6)
bluebaby =carto.pal(pal1 = "blue.pal", n1=6)
greenbean =carto.pal(pal1 = "green.pal", n1=6)

##MAKING MAPS Y'ALL
colvals = palette.colors(7, "R4")
colind = spdf3$Iso.Value[spdf3$Isotope == "d18O" & spdf3$Element == "hair"]
colind = ceiling((colind - min(colind)) / diff(range(colind)) * 6) + 1
plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="hair",], pch=21, col="purple",
     bg = bluebaby[colind])
plot(namap, add=TRUE)
plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="hair",], pch=21, col="purple",
     bg = bluebaby[colind], add = TRUE)
#Oh look a Legend...NEED TO CALCULATE THE COLOR RANGES FOR DATA, other legend is better below
legendChoro(pos = "topleft",
            title.txt = "d18O Isotopic Value",
            breaks = c(2.15,20,40,60,80,17.25),
            col = carto.pal("blue.pal", 6),
            nodata = TRUE, nodata.txt = "No Data")

#MAP for hairs, need calculations for legend
colind = spdf3$Iso.Value[spdf3$Isotope == "87Sr/86Sr" & spdf3$Element == "hair"]
colind = ceiling((colind - min(colind)) / diff(range(colind)) * 6) + 1
plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="hair",], pch=21, col="darkgreen",
     bg = greenbean[colind])
plot(namap, add=TRUE)
plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="hair",], pch=21, col="darkgreen",
     bg = greenbean[colind], add = TRUE)
#This needs some help
#Legend take2, this one is better, can put ranges in for legend, can move position
legendTypo(pos = "bottomleft", title.txt = "87Sr/86Sr Hair Values",
           col = carto.pal("green.pal", 6),
           categ = c("0.704243", "type 2", "type 3", "type 4", "0.71962"),
           nodata = FALSE)

colind = spdf3$Iso.Value[spdf3$Isotope == "87Sr/86Sr" & spdf3$Element == "hair" & spdf3$Data.Origin=="known"]
colind = ceiling((colind - min(colind)) / diff(range(colind)) * 6) + 1
plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="hair"& spdf3$Data.Origin=="known",], pch=21, col="green",
     bg = greenbean[colind])
plot(namap, add=TRUE)
plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="hair"& spdf3$Data.Origin=="known",], pch=21, col="green",
     bg = greenbean[colind], add = TRUE)
legendTypo(pos = "bottomleft", title.txt = "87Sr/86Sr Hair Values",
           col = carto.pal("green.pal", 6),
           categ = c("0.704243", "type 2", "type 3", "type 4", "0.71962"),
           nodata = FALSE)

#MAP for teeths, need to do calculations for legend
colind = spdf3$Iso.Value[spdf3$Isotope == "d18O" & spdf3$Element == "teeth"]
colind = ceiling((colind - min(colind)) / diff(range(colind)) * 6) + 1
plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="teeth",], pch=21, col="purple",
     bg = bluebaby[colind])
plot(namap, add=TRUE)
plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="teeth",], pch=21, col="purple",
     bg = bluebaby[colind], add = TRUE)
legendTypo(pos = "topleft", title.txt = "d18O Teeth Values",
           col = carto.pal("purple.pal", 6),
           categ = c("2", "type 2", "type 3", "type 4", "18"),
           nodata = FALSE)

colind = spdf3$Iso.Value[spdf3$Isotope == "87Sr/86Sr" & spdf3$Element == "teeth"]
colind = ceiling((colind - min(colind)) / diff(range(colind)) * 6) + 1
plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="teeth",], pch=21, col="green",
     bg = greenbean[colind])
plot(namap, add=TRUE)
plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="teeth",], pch=21, col="green",
     bg = greenbean[colind], add = TRUE)

#MAP for bone, add values to legend??
colind = spdf3$Iso.Value[spdf3$Isotope == "d18O" & spdf3$Element == "bone"]
colind = ceiling((colind - min(colind)) / diff(range(colind)) * 6) + 1
plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="bone",], pch=21, col="darkblue",
     bg = bluebaby[colind])
plot(namap, add=TRUE)
plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="bone",], pch=21, col="darkblue",
     bg = bluebaby[colind], add = TRUE)
legendTypo(pos = "bottomleft", title.txt = "d18O Bone Values",
           col = carto.pal("blue.pal", 6),
           categ = c("2", "type 2", "type 3", "type 4", "18"),
           nodata = FALSE)

colind = spdf3$Iso.Value[spdf3$Isotope == "87Sr/86Sr" & spdf3$Element == "bone"]
colind = ceiling((colind - min(colind)) / diff(range(colind)) * 6) + 1
plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="bone",], pch=21, col="green",
     bg = greenbean[colind])
plot(namap, add=TRUE)
plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="bone",], pch=21, col="green",
     bg = greenbean[colind], add = TRUE)

# A bunch of other Maps
plot(spdf3,pch=21, col="lavender")
plot(namap, add=TRUE)
#Strontium Samples
plot(spdf3[spdf3$Isotope=="87Sr/86Sr",],pch=21, col="green")
plot(namap, add=TRUE)
#Oxygen
plot(spdf3[spdf3$Isotope=="d18O",],pch=21, col="purple")
plot(namap, add=TRUE)
#Teeth
plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="teeth",],pch=21, col="purple")
plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="teeth",],pch=21, col="green", add=TRUE)

plot(namap, add=TRUE)

plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="teeth",],pch=21, col="green")
plot(namap, add=TRUE)

plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="teeth",],pch=21, col="purple")
plot(namap, add=TRUE)
#Hair
plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="hair" & spdf3$Data.Origin=="known",],pch=21, col="purple")
plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="hair"& spdf3$Data.Origin=="known",],pch=21, col="green", add=TRUE)
plot(namap, add=TRUE)

plot(spdf3[spdf3$Data.Origin=="known" & spdf3$Isotope=="d18O" & spdf3$Element=="hair",], pch=17)
plot(spdf3[spdf3$Data.Origin=="assumed" & spdf3$Isotope=="d18O" & spdf3$Element=="hair",], pch=17, col="blue", add=TRUE)
plot(namap, add=TRUE)

plot(spdf3[spdf3$Data.Origin=="known" & spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="hair",], pch=17)
plot(namap, add=TRUE)
plot(spdf3[spdf3$Data.Origin=="assumed" & spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="hair",], pch=17, col="red")
plot(spdf3[spdf3$Data.Origin=="known" & spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="hair",], pch=17, add=TRUE)
plot(namap, add=TRUE)

plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Data.Origin=="assumed" & spdf3$Element=="hair" ,],pch=21, col="purple")
plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Data.Origin=="assumed" & spdf3$Element=="hair",],pch=21, col="green", add=TRUE)
plot(namap, add=TRUE)

plot(spdf3[spdf3$Isotope=="87Sr/86Sr" & spdf3$Element=="hair",],pch=21, col="green")
plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="hair",],pch=21, col="purple")
plot(namap, add=TRUE)

#Bone
plot(spdf3[spdf3$Isotope=="d18O" & spdf3$Element=="bone",],pch=21, col="purple")
plot(namap, add=TRUE)

