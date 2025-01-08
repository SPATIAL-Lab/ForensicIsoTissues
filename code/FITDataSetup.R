library(readxl);library(dplyr)

#Read in dataset xlsx from data file from GitHub and merge sheets together
FData1 <-read_excel("data/FIT_Dataset.xlsx", sheet = "Individual")
FData2 <-read_excel("data/FIT_Dataset.xlsx", sheet = "Site")
FData3 <-read_excel("data/FIT_Dataset.xlsx", sheet = "Sample")
FData4 <-read_excel("data/FIT_Dataset.xlsx", sheet = "Data", guess_max = 10000) # so Calibrate isn't assumed to be boolean
Comp1 <-merge(FData2,FData3,by= "Site.ID")
Comp2 <-merge(Comp1,FData1,by= "Ind.ID")
Comp3 <-merge(Comp2,FData4,by= "Sample.ID")
#convert lat and long to numeric
Comp3$Lat= as.numeric(Comp3$Lat)
Comp3$Lon= as.numeric(Comp3$Lon)
#Select needed data columns, this makes data easier to read and visualize
ForensicTisIsoData <-select(Comp3,1:3,5:15,17:21,24:29,30,32:34) 
rm( FData1, FData2, FData3, FData4, Comp1, Comp2, Comp3)
#Get rid of NAs in Lat and Lon
ForensicTisIsoData =ForensicTisIsoData[!is.na(ForensicTisIsoData$Lat),]
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

summstats4 <- FTID %>%
  group_by(Isotope, Element, Country, Data.Origin) %>%
  summarize(mean = mean(Iso.Value),
            sd = sd(Iso.Value),
            min = min(Iso.Value),
            max = max(Iso.Value),
            n = n()) %>% mutate(Range =max-min)

#Remove cities routinely dropped from isoscapes
FTID<- subset(FTID, City!="Tofino" & City!="Honolulu")
#Subset data and remove fingernail and bone data
FTID <- subset(FTID, Element=="hair"|Element=="teeth")
#Make sure there are no duplicates
FTID<- FTID[!duplicated(FTID$Data.ID), ]
#Write data to csv to run FITMapping and FITTapiso, or run it from the FTID dataframe
write.csv(FTID, file="data/ForensicTissue.csv")

