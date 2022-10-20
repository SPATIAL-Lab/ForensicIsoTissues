library(sf)
library(sp)
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
IsoDataSr3 <-read_xlsx("DataComp_22_10.xlsx")
Data1 <-read_xlsx("DataComp_22_10.xlsx", sheet = "Individual")
Data2 <-read_xlsx("DataComp_22_10.xlsx", sheet = "Site")
Data3 <-read_xlsx("DataComp_22_10.xlsx", sheet = "Sample")
Data4 <-read_xlsx("DataComp_22_10.xlsx", sheet = "Data")
Comp1 <-merge(Data2,Data3,by= "Site.ID")
Comp2 <-merge(Comp1,Data1,by= "Ind.ID")
Comp3 <-merge(Comp2,Data4,by= "Sample.ID")
#convert lat and long to numeric
Comp3$Lat= as.numeric(Comp3$Lat)
Comp3$Lon= as.numeric(Comp3$Lon)
#clean up data
view(Comp3)
IsoData4 <-select(Comp3,1,2,3,5,6,7,8,9,10,11,15,16,17,23,24,25,26,29)
#get rid of NAs
IsoData4 =IsoData4[!is.na(IsoData4$Lat),]
IsoData4 =IsoData4[!is.na(IsoData4$Lon),]
#spatial points data frame and add CRS
spdf2 =SpatialPointsDataFrame(data.frame(IsoData4$Lon, IsoData4$Lat), IsoData4)
plot(spdf2)
proj4string(spdf2) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#trying out PCA. DOES NOT WORK, not all columns are numeric
IsoData04 <-na.omit(IsoData4)
pca.fit <- prcomp(x = IsoData4[, -c(1:7,10:12,14:21,23:26,28:30)], scale. = TRUE)

IsoData4$Cind = numeric(nrow(IsoData4))
IsoData4$Cind[IsoData4$Country == "Canada"] = 1
IsoData4$Cind[IsoData4$Country == "USA"] = 2
IsoData4$Cind[IsoData4$Country == "Mexico"] = 3


#some stats now? Descriptive Stats Take 1 Keratin/Oxygen
AnalytestatsK1=IsoData4[IsoData4$Analyte=="keratin",]
AnalytestatsKOxy1=AnalytestatsK1[AnalytestatsK1$Isotope=="d18O",]
OxyKeratin01=AnalytestatsKOxy1$Iso.Value
mean(OxyKeratin01)
sd(OxyKeratin01)
range(OxyKeratin01)
summary(OxyKeratin01)

#some stats now? Descriptive Stats Take 1 Keratin/Oxygen- known
AnalytestatsK2=IsoData4[IsoData4$Analyte=="keratin",]
KOxy2=AnalytestatsK2[AnalytestatsK2$Isotope=="d18O",]
KOxyK2=KOxy2[KOxy2$Data.Origin=="known",]
OxyKer1=KOxyK2$Iso.Value
mean(OxyKer1)
sd(OxyKer1)
range(OxyKer1)
summary(OxyKeratin)

#some stats now- Descriptive Stats Take 1 Keratin/Oxygen- Assumed data
KOxyA1=KOxy2[KOxy2$Data.Origin=="assumed",]
OxyKer2=KOxyA1$Iso.Value
mean(OxyKeratin2)
sd(OxyKeratin2)
range(OxyKeratin2)
summary(OxyKeratin)

#Descriptive Stats Keratin/87Sr/86Sr-overall and known
KSr=AnalytestatsK1[AnalytestatsK1$Isotope=="87Sr/86Sr",]
KSr1=KSr$Iso.Value
mean(KSr1)
sd(KSr1)
range(KSr1)

KSrK1=KSr[KSr$Data.Origin=="known",]
SrKer1=KSrK1$Iso.Value
mean(SrKer1)
sd(SrKer1)
range(SrKer1)

#Descriptive Stats Keratin/87Sr/86Sr-Assumed
KSrA=KSr[KSr$Data.Origin=="assumed",]
SrKer2=KSrA$Iso.Value
mean(SrKer2)
sd(SrKer2)
range(SrKer2)

#Descriptive Stats bioapatite/Sr (all known)
Bio=IsoData4[IsoData4$Analyte=="bioapatite",]
BSr=Bio[Bio$Isotope=="87Sr/86Sr",] 
SrBio=BSr$Iso.Value
mean(SrBio)
sd(SrBio)
range(SrBio)

#Descriptive Stats carbonate/oxy (all known)
Car=IsoData4[IsoData4$Analyte=="carbonate",]
COxy=Car[Car$Isotope=="d18O",]
OxyCar=COxy$Iso.Value
mean(OxyCar)
sd(OxyCar)
range(OxyCar)

MexK=Mexico[Mexico$Analyte=="keratin",]
USAK=USA[USA$Analyte=="keratin",]
USAKOxy=USAK[USAK$Isotope=="d18O",]
#Descriptive Stats by country 
USA=IsoData4[IsoData4$Country=="USA",]
Canada=IsoData4[IsoData4$Country=="Canada",]
Mexico=IsoData4[IsoData4$Country=="Mexico",]
USAO=USA[USA$Isotope=="d18O",]
USASr1=USA[USA$Isotope=="87Sr/86Sr",]
MexO=Mexico[Mexico$Isotope=="d18O",]
MexSr=Mexico[Mexico$Isotope=="87Sr/86Sr",]
CanOxy1=Canada[Canada$Isotope=="d18O",]
CanSr1=Canada[Canada$Isotope=="87Sr/86Sr",]
USAO01=USAO$Iso.Value
USASr01=USASr1$Iso.Value
MexO01=MexO$Iso.Value
MexSr01=MexSr$Iso.Value
CanO01=CanOxy1$Iso.Value
CanSr01=CanSr1$Iso.Value

mean(USAO01)
sd(USAO01)
range(USAO01)

mean(USASr01)
sd(USASr01)
range(USASr01)

mean(MexO01)
sd(MexO01)
range(MexO01)

mean(MexSr01)
sd(MexSr01)
range(MexSr01)

mean(CanO01)
sd(CanO01)
range(CanO01)

mean(CanSr01)
sd(CanSr01)
range(CanSr01)

##Descriptive Stats by country and analyte
USAOK01=USAO[USAO$Analyte=="keratin",]
USAOC01=USAO[USAO$Analyte=="carbonate",]
MexOK01=MexO[MexO$Analyte=="keratin",]
CanOK01=CanOxy1[CanOxy1$Analyte=="keratin",]
USAOK02=USAOK01$Iso.Value
USAOC02=USAOC01$Iso.Value
MexOK02=MexOK01$Iso.Value
CanOK02=CanOK01$Iso.Value
mean(USAOK02)
sd(USAOK02)
range(USAOK02)
mean(USAOC02)
sd(USAOC02)
range(USAOC02)
mean(MexOK02)
sd(MexOK02)
range(MexOK02)
mean(CanOK02)
sd(CanOK02)
range(CanOK02)
MexOC01=MexO[MexO$Analyte=="carbonate",]
CanOC01=CanOxy1[CanOxy1$Analyte=="carbonate",]
MexOC02=MexOC01$Iso.Value
CanOC02=CanOC01$Iso.Value
mean(MexOC02)
sd(MexOC02)
range(MexOC02)
mean(CanOC02)
sd(CanOC02)
range(CanOC02)

## Strontium values known and unknown for everything
USASrK01=USASr1[USASr1$Analyte=="keratin",]
USASrB01=USASr1[USASr1$Analyte=="bioapatite",]
MexSrK01=MexSr[MexSr$Analyte=="keratin",]
MexSrB01=MexSr[MexSr$Analyte=="bioapatite",]
CanSrK01=CanSr1[CanSr1$Analyte=="keratin",]
CanSrB01=CanSr1[CanSr1$Analyte=="bioapatite",]
USASrK02=USASrK01$Iso.Value
USAOSrB02=USASrB01$Iso.Value
MexSrB02=MexSrB01$Iso.Value
MexSrK02=MexSrK01$Iso.Value
CanSrK02=CanSrK01$Iso.Value
CanSrB02=CanSrB01$Iso.Value
mean(USASrK02)
sd(USASrK02)
range(USASrK02)
mean(USAOSrB02)
sd(USAOSrB02)
range(USAOSrB02)
mean(MexSrK02)
sd(MexSrK02)
range(MexSrK02)
mean(MexSrB02)
sd(MexSrB02)
range(MexSrB02)
mean(CanSrK02)
sd(CanSrK02)
range(CanSrK02)
mean(CanSrB02)
sd(CanSrB02)
range(CanSrB02)
#keratin USA known and assummed
USASrKK01=USASrK01[USASrK01$Data.Origin=="known",]
USASrKK02=USASrKK01$Iso.Value
mean(USASrKK1)
sd(USASrKK1)
range(USASrKK1)

USASrKA01=USASrK01[USASrK01$Data.Origin=="assumed",]##96
USASrKA02=USASrKA01$Iso.Value
mean(USASrKA1)
sd(USASrKA1)
range(USASrKA1)

##Descriptive stats-- Elements
Hair=IsoData4[IsoData4$Element=="hair",]
Nail=IsoData4[IsoData4$Element=="fingernail",]
Bone=IsoData4[IsoData4$Element=="bone",]
Teeth=IsoData4[IsoData4$Element=="teeth",]
OHair=Hair[Hair$Isotope=="d18O",]
SrHair=Hair[Hair$Isotope=="87Sr/86Sr",]
ONail=Nail[Nail$Isotope=="d18O",]## None
SrNail=Nail[Nail$Isotope=="87Sr/86Sr",]
OBone=Bone[Bone$Isotope=="d18O",]
SrBone=Bone[Bone$Isotope=="87Sr/86Sr",]
OTooth=Teeth[Teeth$Isotope=="d18O",]
SrTooth=Teeth[Teeth$Isotope=="87Sr/86Sr",]
HairO01=OHair$Iso.Value
HairSr01=SrHair$Iso.Value
NailSr01=SrNail$Iso.Value
BoneO01=OBone$Iso.Value
BoneSr01=SrBone$Iso.Value
ToothO01=OTooth$Iso.Value
ToothSr01=SrTooth$Iso.Value
mean(HairO01)
sd(HairO01)
range(HairO01)
mean(HairSr01)
sd(HairSr01)
range(HairSr01)
mean(NailSr01)
sd(NailSr01)
range(NailSr01)
mean(BoneO01)
sd(BoneO01)
range(BoneO01)
mean(BoneSr01)
sd(BoneSr01)
range(BoneSr01)
mean(ToothO01)
sd(ToothO01)
range(ToothO01)
mean(ToothSr01)
sd(ToothSr01)
range(ToothSr01)

Hair=IsoData4[IsoData4$Element=="hair",]
HairUSA=Hair[Hair$Country=="USA",]
KnownOHair=OHair[OHair$Data.Origin=="known",]
KOHair=KnownOHair$Iso.Value
mean(KOHair)
sd(KOHair)
range(KOHair)

HairOK01=OHair[OHair$Data.Origin=="known",]
HairOA01=OHair[OHair$Data.Origin=="assumed",]
HairOK02=HairOK01$Iso.Value
HairOA02=HairOA01$Iso.Value
mean(HairOK02)
sd(HairOK02)
range(HairOK02)
mean(HairOA02)
sd(HairOA02)
range(HairOA02)

HairSrK01=SrHair[SrHair$Data.Origin=="known",]
HairSrA01=SrHair[SrHair$Data.Origin=="assumed",]
HairSrK02=HairSrK01$Iso.Value
HairSrA02=HairSrA01$Iso.Value
mean(HairSrK02)
sd(HairSrK02)
range(HairSrK02)
mean(HairSrA02)
sd(HairSrA02)
range(HairSrA02)

Tooth=IsoData4[IsoData4$Element=="teeth",]
ToothUSA=Tooth[Tooth$Country=="USA",]

#Now element by countries -USA
USAHairO=OHair[OHair$Country=="USA",]
USAHairSr1=SrHair[SrHair$Country=="USA",]
USAToothO=OTooth[OTooth$Country=="USA",]
USAToothSr1=SrTooth[SrTooth$Country=="USA",]
USAHairO01=USAHairO$Iso.Value
USAHairSr01=USAHairSr1$Iso.Value
USAToothO01=USAToothO$Iso.Value
USAToothSr01=USAToothSr1$Iso.Value
mean(USAHairO01)
sd(USAHairO01)
range(USAHairO01)
mean(USAHairSr01)
sd(USAHairSr01)
range(USAHairSr01)
mean(USAToothO01)
sd(USAToothO01)
range(USAToothO01)
mean(USAToothSr01)
sd(USAToothSr01)
range(USAToothSr01)

USAHairOK01=USAHairO[USAHairO$Data.Origin=="known",]
USAHairSrK01=USAHairSr1[USAHairSr1$Data.Origin=="known",]
USAHairOxyK02=USAHairOK01$Iso.Value
USAHairSrK02=USAHairSrK01$Iso.Value
mean(USAHairOxyK02)
sd(USAHairOxyK02)
range(USAHairOxyK02)
mean(USAHairSrK02)
sd(USAHairSrK02)
range(USAHairSrK02)

USAHairOA01=USAHairO[USAHairO$Data.Origin=="assumed",]
USAHairSrA01=USAHairSr1[USAHairSr1$Data.Origin=="assumed",]
USAHairOA02=USAHairOA01$Iso.Value
USAHairSrA02=USAHairSrA01$Iso.Value
mean(USAHairOA02)
sd(USAHairOA02)
range(USAHairOA02)
mean(USAHairSrA02)
sd(USAHairSrA02)
range(USAHairSrA02)

#Elements Mexico (only hair and teeth)
MexHairO=OHair[OHair$Country=="Mexico",]
MexTooth=OTooth[OTooth$Country=="Mexico",]
MexHairSr1=SrHair[SrHair$Country=="Mexico",]
MexToothSr1=SrTooth[SrTooth$Country=="Mexico",]
MexHairO01=MexHairO$Iso.Value
MexHairSr01=MexHairSr1$Iso.Value
MexToothO01=MexToothO$Iso.Value
MexToothSr01=MexToothSr1$Iso.Value
mean(MexHairO1)
sd(MexHairO1)
range(MexHairO1)
mean(MexHairSr01)
sd(MexHairSr01)
range(MexHairSr01)
mean(MexToothO1)
sd(MexToothO1)
range(MexToothO1)
mean(MexToothSr1)
sd(MexToothSr1)
range(MexToothSr1)


#And elements in Canada (only teeth and hairs) 
CanHairOxy=HairOxy[HairOxy$Country=="Canada",]
CanToothOxy=ToothOxy[ToothOxy$Country=="Canada",]
CanHairSr=HairSr[HairSr$Country=="Canada",]
CanToothSr=ToothSr[ToothSr$Country=="Canada",]
CanHairO1=CanHairOxy$Iso.Value
CanHairSr1=CanHairSr$Iso.Value
CanToothO1=CanToothOxy$Iso.Value
CanToothSr1=CanToothSr$Iso.Value
mean(CanHairO1)
sd(CanHairO1)
range(CanHairO1)
mean(CanHairSr1)
sd(CanHairSr1)
range(CanHairSr1)
mean(CanToothO1)
sd(CanToothO1)
range(CanToothO1)
mean(CanToothSr1)
sd(CanToothSr1)
range(CanToothSr1)

#no boxplot(Iso.Value~Country, data = AnalytestatsKSr, main= "Keratin Strontium Values by Country", xlab = "Country", ylab = "d18O")
library(cartography)
library(sf)
install.packages("mapsf")
library(maps)
  
#Winner winner chicken dinner for boxplot coloring. 
#no boxplot(Iso.Value~Country, data = AnalytestatsKSr, main= "Keratin Strontium Values by Country", xlab = "Country", ylab = "d18O", col= carto.pal("harmo.pal"))
display.carto.all(n=8)

AllSrData=IsoData4[IsoData4$Isotope=="87Sr/86Sr",]
AllOData=IsoData4[IsoData4$Isotope=="d18O",]

library(ggplot2)
# Nope kable(AllSrData %>% group_by(Element) %>% summarize( mean = mean(Iso.Value, count = n())
      

#More boxplots. Yay.
boxplot(Iso.Value~Country, data = AnalytestatsKSrA, main= "Keratin Assumed Strontium Values", xlab = "Country", ylab = "87Sr/86Sr", col= carto.pal("harmo.pal"))
#All Data Sr and Oxygen

boxplot(Iso.Value~Element, data = AllSrData, main= "Strontium", xlab = "Element", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))
boxplot(Iso.Value~Element, data = AllOData, main= "Oxygen", xlab = "Element", ylab = "d18O", col= carto.pal("purple.pal"))

AllSrData$Cind = numeric(nrow(AllSrData))
AllSrData$Cind[AllSrData$Country == "Canada"] = 1
AllSrData$Cind[AllSrData$Country == "USA"] = 2
AllSrData$Cind[AllSrData$Country == "Mexico"] = 3
boxplot(Iso.Value~Cind, data = AllSrData, main= "Strontium", names = c("Canada", "USA", "Mexico"), 
        xlab = "Country", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))
boxplot(Iso.Value~Country, data = AllOData, main= "Oxygen", xlab = "Country", ylab = "d18O", col= carto.pal("purple.pal"))

boxplot(Iso.Value~Analyte, data = AllSrData, main= "Strontium", xlab = "Analyte", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))
boxplot(Iso.Value~Analyte, data = AllOData, main= "Oxygen", xlab = "Analyte", ylab = "d18O", col= carto.pal("purple.pal"))


#By Element and Analyte
boxplot(Iso.Value~Analyte, data = USAO, main= "USA Oxygen Values by Analyte", xlab = "Analyte", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Analyte, data = USASr, main= "USA Strontium Values by Analyte", xlab = "Analyte", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

#boxplots with names in N-S order
boxplot(Iso.Value~Cind, data = OHair, main= "Hair Oxygen Values by Country", names = c("Canada", "USA", "Mexico"), xlab = "Country", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Cind, data = SrHair, main= "Hair Strontium Values by Country", names = c("Canada", "USA", "Mexico"), xlab = "Country", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))     

boxplot(Iso.Value~Cind, data = OTooth, main= "Tooth Oxygen Values by Country", names = c("Canada", "USA", "Mexico"), xlab = "Country", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Cind, data = SrTooth, main= "Tooth Strontium Values by Country", names = c("Canada", "USA", "Mexico"), xlab = "Country", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal")) 

boxplot(Iso.Value~Cind, data = OBone, main= "Tooth Oxygen Values by Country", names = c("Canada", "USA", "Mexico"), xlab = "Country", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Country, data = SrBone, main= "Tooth Strontium Values by Country", names = c("Canada", "USA", "Mexico"), xlab = "Country", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal")) 


boxplot(Iso.Value~Analyte, data = USAO, main= "USA Oxygen Values by Analyte", xlab = "Analyte", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Analyte, data = USASr1, main= "USA Strontium Values by Analyte", xlab = "Analyte", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal")) 

boxplot(Iso.Value~Analyte, data = CanOxy1, main= "Canada Oxygen Values by Analyte", xlab = "Analyte", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Analyte, data = CanSr1, main= "Canada Strontium Values by Analyte", xlab = "Analyte", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal")) 

boxplot(Iso.Value~Analyte, data = MexO, main= "Mexico Oxygen Values by Analyte", xlab = "Analyte", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Analyte, data = MexSr, main= "Mexico Strontium Values by Analyte", xlab = "Analyte", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

boxplot(Iso.Value~Element, data = CanOxy1, main= "Canada Oxygen Values by Element", xlab = "Element", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Element, data = CanSr1, main= "Canada Strontium Values by Element", xlab = "Element", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

boxplot(Iso.Value~Element, data = MexO, main= "Mexico Oxygen Values by Element", xlab = "Element", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Element, data = MexSr, main= "Mexico Strontium Values by Element", xlab = "Element", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

boxplot(Iso.Value~Element, data = USAO, main= "USA Oxygen Values by Element", xlab = "Element", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Element, data = USASr1, main= "USA Strontium Values by Element", xlab = "Element", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

boxplot(Iso.Value~Data.Origin, data = USAO, main= "USA Oxygen Values by Data Origin", xlab = "Data Origin", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Data.Origin, data = USASr1, main= "USA Strontium Values by Data Origin", xlab = "Data Origin", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

boxplot(Iso.Value~Data.Origin, data = MexO, main= "Mexico Oxygen Values by Data Origin", xlab = "Data Origin", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Data.Origin, data = MexSr, main= "Mexico Strontium Values by Data Origin", xlab = "Data Origin", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

boxplot(Iso.Value~Data.Origin, data = USAHairO, main= "USA Hair Oxygen Values by Data Origin", xlab = "Data Origin", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Data.Origin, data = USAHairSr1, main= "USA Hair Strontium Values by Data Origin", xlab = "Data Origin", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

boxplot(Iso.Value~Data.Origin, data = MexHairOxy, main= "Mexico Hair Oxygen Values by Data Origin", xlab = "Data Origin", ylab = "d18O", col= carto.pal("purple.pal"))
boxplot(Iso.Value~Data.Origin, data = MexHairSr, main= "Mexico Hair Strontium Values by Data Origin", xlab = "Data Origin", ylab = "87Sr/86Sr", col= carto.pal("turquoise.pal"))

#EDA
summary(AllOData)
summary(AllSrData)
ggplot(data = AllSrData, aes(x=Iso.Value))+ geom_histogram(fill="steelblue", color="black")+ ggtitle("Test")
ggplot(data = AllOData, aes(x=Iso.Value))+ geom_histogram(fill="steelblue", color="black")+ ggtitle("Test1")

ggplot(data = AllSrData, aes(x=Iso.Value, y=Lat, color=Country))+geom_point()
ggplot(data = AllSrData, aes(x=Iso.Value, y=Lon, color=Country))+geom_point()
ggplot(data = AllOData, aes(x=Iso.Value, y=Lat, color=Country, shape=Biomolecule))+geom_point()
ggplot(data = AllOData, aes(x=Iso.Value, y=Lon, color=Country, shape=Biomolecule))+geom_point()
ggplot(data = AllOData, aes(x=Iso.Value, y=Lat, color=Country, shape=Element))+geom_point()

ggplot(data = OBone, aes(x=Iso.Value, y=Lat, color=Location.Quality, shape=Element))+geom_point()

ggplot(data = MexO, aes(x=Iso.Value, y=Lat, color=Element))+geom_point()
ggplot(data = MexSr, aes(x=Iso.Value, y=Lat, color=Element, shape=Data.Origin))+geom_point()

O <- ggplot(data = OHair, aes(x=Iso.Value, y=Lat, color=Country))+geom_point()
ggplot(data = OHair, aes(x=Iso.Value, y=Lat, color=Location.Quality, shape=Country))+geom_point()

##getting color crazy
ggplot(data = OHair, aes(x=Iso.Value, y=Lat, color=Country))+geom_point()+geom_bar(colour=carto.pal("purple.pal", n1=6))
O + scale_color_brewer(pal1 = "purple.pal", n=6)
prettypurple=carto.pal(pal1 = "purple.pal", n1=6)

#Add count to boxplots using ggplot
#no stat_box_data? ggplot(data=AllSrData, aes(x=Country,y=Iso.Value))+ geom_boxplot(fill="steelblue")+ stat_summary(AllSrData= stat_box_data, geom= "text", hjust=0.5, vjust=0.9)+ theme_fivethirtyeight()
#not working either, ggplot(data=AllSrData, aes(x=Country,y=Iso.Value))+ geom_boxplot(fill="steelblue")+ annotate("text", x=1:aggregate(Iso.Value~ Country, AllSrData, median)[,2], label = table(AllSrData$Country), col= "red", vjust = - 1)

library(corrplot)
library(assignR)
plot(spdf1)
plot(naMap)
class(naMap)
library(sp)
class(spdf1)
#add points to map

plot(spdf1, add=TRUE)
#do a selection of the data to access part of it

plot(spdf1[spdf1$Isotope=="87Sr/86Sr" & spdf1$Element=="teeth",],pch=21)
plot(naMap, add=TRUE)

plot(spdf1[spdf1$Isotope=="87Sr/86Sr" & spdf1$Element=="hair",],pch=19, col="darkgreen")
plot(naMap, add=TRUE)



MapCoords<- read_xlsx(Coords.xlsx)
