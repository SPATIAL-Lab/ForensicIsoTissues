library(sp); library(rgdal); library(dplyr); library(terra)
library(cartography); library(mapsf) ;library(ggplot2); library(RColorBrewer)
library(wesanderson); library(sf) ;library(readxl) ;library(tidyverse) ;library(ggplot2)


ForensicTIsoData = ForensicIsoDataNew

Fspdf =SpatialPointsDataFrame(data.frame(ForensicTIsoData$Lon, ForensicTIsoData$Lat), ForensicTIsoData)
plot(Fspdf)
proj4string(Fspdf) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#Hair and Keratin Stats re-run, only USA, overall, known and assumed 
#some stats now? Descriptive Stats Take 1 Keratin/Oxygen
KeratinStats=ForensicTIsoData[ForensicTIsoData$Analyte=="keratin",]
KerOxy=KeratinStats[KeratinStats$Isotope=="d18O",]
KerOxy1=KerOxy$Iso.Value
mean(KerOxy1)
sd(KerOxy1)
range(KerOxy1)

KerSr=KeratinStats[KeratinStats$Isotope=="87Sr/86Sr",]
KerSr1=KerSr$Iso.Value
mean(KerSr1)
sd(KerSr1)
range(KerSr1)

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


#Make some maps with new Hairs data
namap = readOGR("PoliticalBoundaries_Shapefiles/boundary_l_v2.shp")
plot(namap)
Fspdf=spTransform(Fspdf,crs(namap))

#subset map to get of weird lines, maybe, it worked but still a few weird lines
namap=namap[namap$COUNTRY %in% c("USA", "CAN USA", "MEX USA", "MEX", "CAN"),]
plot(namap)

#Test map- PEACH
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

#Map, distribution of known origin hairs 
plot(Fspdf[Fspdf$Isotope=="d18O" & Fspdf$Element=="hair" & Fspdf$Data.Origin=="known",],pch=21, col="purple")
plot(Fspdf[Fspdf$Isotope=="87Sr/86Sr" & Fspdf$Element=="hair"& Fspdf$Data.Origin=="known",],pch=21, col="green", add=TRUE)
plot(namap, add=TRUE)

#Map, distribution of Oxygen hairs
plot(Fspdf[Fspdf$Data.Origin=="known" & Fspdf$Isotope=="d18O" & Fspdf$Element=="hair",], pch=17)
plot(Fspdf[Fspdf$Data.Origin=="assumed" & Fspdf$Isotope=="d18O" & Fspdf$Element=="hair",], pch=17, col="blue", add=TRUE)
plot(namap, add=TRUE)
#Map, distribution of Stront hairs
plot(Fspdf[Fspdf$Data.Origin=="assumed" & Fspdf$Isotope=="87Sr/86Sr" & Fspdf$Element=="hair",], pch=17, col="red")
plot(Fspdf[Fspdf$Data.Origin=="known" & Fspdf$Isotope=="87Sr/86Sr" & Fspdf$Element=="hair",], pch=17, add=TRUE)
plot(namap, add=TRUE)

#Map, distribution of assumed origin hairs
plot(Fspdf[Fspdf$Isotope=="d18O" & Fspdf$Element=="hair" & Fspdf$Data.Origin=="assumed",],pch=21, col="purple")
plot(Fspdf[Fspdf$Isotope=="87Sr/86Sr" & Fspdf$Element=="hair"& Fspdf$Data.Origin=="assumed",],pch=21, col="green", add=TRUE)
plot(namap, add=TRUE)

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
##BANANA!!  Something weird is going on and there are two spots showing no data
##but there is data for everything?
colind = Fspdf$Iso.Value[Fspdf$Isotope == "d18O" & Fspdf$Element == "hair"]
colind = ceiling((colind - min(colind)) / diff(range(colind)) * 6) + 1
plot(Fspdf[Fspdf$Isotope=="d18O" & Fspdf$Element=="hair",], pch=21, col="purple",
     bg = prettypurple[colind])
plot(namap, add=TRUE)
plot(Fspdf[Fspdf$Isotope=="d18O" & Fspdf$Element=="hair",], pch=21, col="purple",
     bg = prettypurple[colind], add=TRUE)
#This needs some help
#Legend take2, this one is better, can put ranges in for legend, can move position
legendTypo(pos = "bottomright", title.txt = "d18O Hair Values",
           col = carto.pal("purple.pal", 6),
           categ = c("number", "type 2", "type 3", "type 4", "number"),
           nodata = FALSE)
#More maps- 
#Teeth distribution, Sr and O
plot(Fspdf[Fspdf$Isotope=="d18O" & Fspdf$Element=="teeth",],pch=21, col="purple")
plot(Fspdf[Fspdf$Isotope=="87Sr/86Sr" & Fspdf$Element=="teeth",],pch=21, col="green", add=TRUE)
plot(namap, add=TRUE)
## this needs help, it's not working 
legendTypo(pos = "bottomright", title.txt = "Teeth Distribution",
           col = 
           categ = c("Oxygen", "Strontium"),
           nodata = FALSE)

#Teeths 


#Teeth, Sr, range of values, Legend needs calculations
colind = Fspdf$Iso.Value[Fspdf$Isotope == "87Sr/86Sr" & Fspdf$Element == "teeth"]
colind = ceiling((colind - min(colind)) / diff(range(colind)) * 6) + 1
plot(Fspdf[Fspdf$Isotope=="87Sr/86Sr" & Fspdf$Element=="teeth",], pch=21, col="darkgreen",
     bg = greenbean[colind])
plot(namap, add=TRUE)
plot(Fspdf[Fspdf$Isotope=="87Sr/86Sr" & Fspdf$Element=="teeth",], pch=21, col="darkgreen",
     bg = greenbean[colind], add = TRUE)
legendTypo(pos = "bottomleft", title.txt = "87Sr/86Sr Teeth Values",
           col = carto.pal("green.pal", 6),
           categ = c("0.704243", "type 2", "type 3", "type 4", "0.71962"),
           nodata = FALSE)

#Teeth, Oxygen, range of values, Legend need calculations 
##BANANA! missing data for some points...
colind = Fspdf$Iso.Value[Fspdf$Isotope == "d18O" & Fspdf$Element == "teeth"]
colind = ceiling((colind - min(colind)) / diff(range(colind)) * 6) + 1
plot(Fspdf[Fspdf$Isotope=="d18O" & Fspdf$Element=="teeth",], pch=21, col="purple",
     bg = prettypurple[colind])
plot(namap, add=TRUE)
plot(Fspdf[Fspdf$Isotope=="d18O" & Fspdf$Element=="teeth",], pch=21, col="purple",
     bg = prettypurple[colind], add=TRUE)
legendTypo(pos = "bottomleft", title.txt = "d18O Teeth Values",
           col = carto.pal("purple.pal", 6),
           categ = c("number", "type 2", "type 3", "type 4", "number"),
           nodata = FALSE)

#Map, Bone, oxygen (only two Sr) distribution
plot(Fspdf[Fspdf$Isotope=="d18O" & Fspdf$Element=="bone",],pch=21, col="blue")
plot(namap, add=TRUE)

#MAP for bone, range, add values to legend!!
colind = Fspdf$Iso.Value[Fspdf$Isotope == "d18O" & Fspdf$Element == "bone"]
colind = ceiling((colind - min(colind)) / diff(range(colind)) * 6) + 1
plot(Fspdf[Fspdf$Isotope=="d18O" & Fspdf$Element=="bone",], pch=21, col="darkblue",
     bg = bluebaby[colind])
plot(namap, add=TRUE)
plot(Fspdf[Fspdf$Isotope=="d18O" & Fspdf$Element=="bone",], pch=21, col="darkblue",
     bg = bluebaby[colind], add = TRUE)
legendTypo(pos = "bottomleft", title.txt = "d18O Bone Values",
           col = carto.pal("blue.pal", 6),
           categ = c("2", "type 2", "type 3", "type 4", "18"),
           nodata = FALSE)



#Organizing data into batches by Isotope, Country, Element etc.
Oxygen=ForensicTIsoData[ForensicTIsoData$Isotope=="d18O",]
Stront=ForensicTIsoData[ForensicTIsoData$Isotope=="87Sr/86Sr",]
USAO=Oxygen[Oxygen$Country=="USA",]
USASr=Stront[Stront$Country=="USA",]
MexO=Oxygen[Oxygen$Country=="Mexico",]
MexSr=Stront[Stront$Country=="Mexico",]
CanO=Oxygen[Oxygen$Country=="Canada",]
CanSr=Stront[Stront$Country=="Canada",]

HairO=Oxygen[Oxygen$Element=="hair",]
HairSr=Stront[Stront$Element=="hair",]
ToothO=Oxygen[Oxygen$Element=="teeth",]
ToothSr=Stront[Stront$Element=="teeth",]
BoneO=Oxygen[Oxygen$Element=="bone",]
BoneSr=Stront[Stront$Element=="bone",]

USAOHair=USAO[USAO$Element=="hair",]
USASrHair=USASr[USASr$Element=="hair",]
MexOHair=MexO[MexO$Element=="hair",]
MexSrHair=MexSr[MexSr$Element=="hair",]

#New boxplots with Oxygen and Sr Hair data
boxplot(Iso.Value~Data.Origin, data=USAOHair, main= "USA Oxygen Values", xlab = "Analyte" 
        , ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Data.Origin, data=HairUSASr, main= "USA Stronium Values", xlab = "Analyte" 
        , ylab = "87Sr/86Sr", col= carto.pal("green.pal"))


#All Data Sr and Oxygen
boxplot(Iso.Value~Element, data = Stront, main= "Strontium", xlab = "Element", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))
boxplot(Iso.Value~Element, data = Oxygen, main= "Oxygen", xlab = "Element", ylab = "d18O", col= carto.pal("purple.pal"))

boxplot(Iso.Value~Cind, data = Stront, main= "Strontium", names = c("Canada", "USA", "Mexico"), 
        xlab = "Country", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))
boxplot(Iso.Value~Country, data = Oxygen, main= "Oxygen", xlab = "Country", ylab = "d18O", col= carto.pal("purple.pal"))

boxplot(Iso.Value~Analyte, data = Stront, main= "Strontium", xlab = "Analyte", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))
boxplot(Iso.Value~Analyte, data = Oxygen, main= "Oxygen", xlab = "Analyte", ylab = "d18O", col= carto.pal("purple.pal"))


#By Element and Analyte
boxplot(Iso.Value~Analyte, data = USAO, main= "USA Oxygen Values by Analyte", xlab = "Analyte", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Analyte, data = USASr, main= "USA Strontium Values by Analyte", xlab = "Analyte", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

#boxplots with names in N-S order
boxplot(Iso.Value~Cind, data = HairO, main= "Hair Oxygen Values by Country", names = c("Canada", "USA", "Mexico"), xlab = "Country", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Cind, data = HairSr, main= "Hair Strontium Values by Country", names = c("Canada", "USA", "Mexico"), xlab = "Country", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))     

boxplot(Iso.Value~Cind, data = ToothO, main= "Tooth Oxygen Values by Country", names = c("Canada", "USA", "Mexico"), xlab = "Country", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Cind, data = ToothSr, main= "Tooth Strontium Values by Country", names = c("Canada", "USA", "Mexico"), xlab = "Country", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal")) 

## Only bone data from USA
boxplot(Iso.Value~Cind, data = BoneO, main= "Tooth Oxygen Values by Country", names = c("Canada", "USA", "Mexico"), xlab = "Country", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Cind, data = BoneSr, main= "Tooth Strontium Values by Country", names = c("Canada", "USA", "Mexico"), xlab = "Country", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal")) 

boxplot(Iso.Value~Analyte, data = USAO, main= "USA Oxygen Values by Analyte", xlab = "Analyte", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Analyte, data = USASr, main= "USA Strontium Values by Analyte", xlab = "Analyte", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal")) 

boxplot(Iso.Value~Analyte, data = CanO, main= "Canada Oxygen Values by Analyte", xlab = "Analyte", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Analyte, data = CanSr, main= "Canada Strontium Values by Analyte", xlab = "Analyte", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal")) 

boxplot(Iso.Value~Analyte, data = MexO, main= "Mexico Oxygen Values by Analyte", xlab = "Analyte", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Analyte, data = MexSr, main= "Mexico Strontium Values by Analyte", xlab = "Analyte", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

boxplot(Iso.Value~Element, data = CanO, main= "Canada Oxygen Values by Element", xlab = "Element", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Element, data = CanSr, main= "Canada Strontium Values by Element", xlab = "Element", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

boxplot(Iso.Value~Element, data = MexO, main= "Mexico Oxygen Values by Element", xlab = "Element", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Element, data = MexSr, main= "Mexico Strontium Values by Element", xlab = "Element", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

boxplot(Iso.Value~Element, data = USAO, main= "USA Oxygen Values by Element", xlab = "Element", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Element, data = USASr, main= "USA Strontium Values by Element", xlab = "Element", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

boxplot(Iso.Value~Data.Origin, data = USAO, main= "USA Oxygen Values by Data Origin", xlab = "Data Origin", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Data.Origin, data = USASr1, main= "USA Strontium Values by Data Origin", xlab = "Data Origin", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

boxplot(Iso.Value~Data.Origin, data = MexO, main= "Mexico Oxygen Values by Data Origin", xlab = "Data Origin", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Data.Origin, data = MexSr, main= "Mexico Strontium Values by Data Origin", xlab = "Data Origin", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

boxplot(Iso.Value~Data.Origin, data = USAOHair, main= "USA Hair Oxygen Values by Data Origin", xlab = "Data Origin", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Data.Origin, data = USASrHair, main= "USA Hair Strontium Values by Data Origin", xlab = "Data Origin", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))
#All oxygen is known, all Sr is assumed
boxplot(Iso.Value~Data.Origin, data = MexOHair, main= "Mexico Hair Oxygen Values by Data Origin", xlab = "Data Origin", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Data.Origin, data = MexSrHair, main= "Mexico Hair Strontium Values by Data Origin", xlab = "Data Origin", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

boxplot(Iso.Value~Analyte, data = USAO, main= "USA Oxygen Values by Analyte", xlab = "Analyte", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Analyte, data = USASr, main= "USA Strontium Values by Analyte", xlab = "Analyte", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

#Biplots (some have codes to change colors from basic, see pretty colors below in#Make pretty section)
ggplot(data = USAO, aes(x=Iso.Value, y=Lat, color=Element))+geom_point(size=2)+scale_color_manual(values= c("#000080", "#800080", "#008000"))
ggplot(data = USAO, aes(x=Iso.Value, y=Elev, color=Element))+geom_point(size=2)+scale_color_manual(values=wes_palette(n=3, name="Darjeeling1"))

ggplot(data = USAO, aes(x=Iso.Value, y=Lat, color=Location.Quality, shape=Element))+geom_point()
ggplot(data = USAO, aes(x=Iso.Value, y=Lat, color=Biomolecule, shape=Element))+geom_point()

ggplot(data = MexO, aes(x=Iso.Value, y=Lat, color=Element))+geom_point(size=2)+scale_color_manual(values= c("#000080", "#800080", "#008000"))
ggplot(data = MexO, aes(x=Iso.Value, y=Elev, color=Element))+geom_point()

ggplot(data = CanO, aes(x=Iso.Value, y=Lat, color=Element))+geom_point()
ggplot(data = CanO, aes(x=Iso.Value, y=Elev, color=Element))+geom_point()

#Somewhat interesting
ggplot(data = USASr, aes(x=Iso.Value, y=Lat, color=Element))+geom_point()
ggplot(data = USASr, aes(x=Iso.Value, y=Elev, color=Element))+geom_point()

ggplot(data = MexSr, aes(x=Iso.Value, y=Lat, color=Element))+geom_point()
ggplot(data = MexSr, aes(x=Iso.Value, y=Elev, color=Element))+geom_point()

##more bi-plots
ggplot(data = MexSr, aes(x=Iso.Value, y=Lat, color=Element, shape=Data.Origin))+geom_point()

ggplot(data = HairO, aes(x=Iso.Value, y=Lat, color=Country))+geom_point()
ggplot(data = HairO, aes(x=Iso.Value, y=Lat, color=Location.Quality, shape=Country))+geom_point()

#Making biplots pretty
#wesanderson color palette
ggplot(data = USAO, aes(x=Iso.Value, y=Lat, color=Element))+geom_point(size=2)+scale_color_manual(values=wes_palette(n=3, name="Darjeeling1"))
#RColorBrewer
ggplot(data = USAO, aes(x=Iso.Value, y=Lat, color=Element))+geom_point(size=2)+scale_color_manual(values= brewer.pal(n=3, name="PRGn"))
#Manually enter colors
ggplot(data = USAO, aes(x=Iso.Value, y=Lat, color=Element))+geom_point(size=2)+scale_color_manual(values= c("#999999", "#E69F00", "#56B4E9"))
ggplot(data = USAO, aes(x=Iso.Value, y=Lat, color=Element))+geom_point(size=2)+scale_color_manual(values= c("#000080", "#800080", "#008000"))

                                                                                                            