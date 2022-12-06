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

ForensicTIsoData$Cind = numeric(nrow(ForensicTIsoData))
ForensicTIsoData$Cind[ForensicTIsoData$Country == "Canada"] = 1
ForensicTIsoData$Cind[ForensicTIsoData$Country == "USA"] = 2
ForensicTIsoData$Cind[ForensicTIsoData$Country == "Mexico"] = 3

#then write to csv
write.csv(ForensicTIsoData, file="ForensicIsoDataNew.csv")