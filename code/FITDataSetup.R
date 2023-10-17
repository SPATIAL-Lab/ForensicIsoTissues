library(tidyverse);library(readxl);library(dplyr); library(assignR)

#Read in data from data file from Github and merge sheets together
FData1 <-read_excel("data/DataComp_23_10_3.xlsx", sheet = "Individual")
FData2 <-read_excel("data/DataComp_23_10_3.xlsx", sheet = "Site")
FData3 <-read_excel("data/DataComp_23_10_3.xlsx", sheet = "Sample")
FData4 <-read_excel("data/DataComp_23_10_3.xlsx", sheet = "Data")
Comp1 <-merge(FData2,FData3,by= "Site.ID")
Comp2 <-merge(Comp1,FData1,by= "Ind.ID")
Comp3 <-merge(Comp2,FData4,by= "Sample.ID")
#convert lat and long to numeric
Comp3$Lat= as.numeric(Comp3$Lat)
Comp3$Lon= as.numeric(Comp3$Lon)
#Select needed data columns
ForensicTisIsoData <-select(Comp3,1,2,3,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,,21,24,25,26, 27,28,32,33,35,36) 
rm( FData1, FData2, FData3, FData4, Comp1, Comp2, Comp3)
#Get rid of NA in Lat and Lon
ForensicTisIsoData =ForensicTisIsoData[!is.na(ForensicTisIsoData$Lat),]
ForensicTisIsoData =ForensicTisIsoData[!is.na(ForensicTisIsoData$Lon),]
FTID <-ForensicTisIsoData

#Summary statistics for entire dataset
summstats <- FTID %>%
  group_by(Isotope, Element, Country) %>%
  summarize(mean = mean(Iso.Value),
            sd = sd(Iso.Value),
            min = min(Iso.Value),
            max = max(Iso.Value),
            n = n()) %>% mutate(Range =max-min)

summstats2 <- FTID %>%
  group_by(Isotope, Element) %>% 
  summarize(mean = mean(Iso.Value),
            sd = sd(Iso.Value),
            min = min(Iso.Value),
            max = max(Iso.Value),
            n = n()) %>% mutate(Range =max-min)

summstats3 <- FTID %>%
  group_by(Isotope, Element, Reference.ID) %>%
  summarize(mean = mean(Iso.Value),
            sd = sd(Iso.Value),
            min = min(Iso.Value),
            max = max(Iso.Value),
            n = n()) %>% mutate(Range =max-min)

#Remove cities dropped from isoscapes
FTID<- subset(FTID, City!="Tofino" & City!="Washington D.C." & City!="Mexico City")
#subset and remove fingernail and bone data
FTID2 <- subset(FTID, Element=="hair"|Element=="teeth")

#Write data to csv to run FITMapping and FITIsoscape
write.csv(FTID2, file="data/ForensicTissue.csv")
