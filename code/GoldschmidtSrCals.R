#Hair####

#Get hair Sr data
sr_hair = subset(ForensicTIsoData, Element == 'hair' & Isotope == '87Sr/86Sr')
sr_hair$sd = rep(0.0003)

#Spatial
sr_hair.sp = vect(sr_hair, geom = c("Lon", "Lat"), crs = "WGS84")

#Get isoscape, project data
sriso = getIsoscapes("GlobalSr")
sr_hair.sp = project(sr_hair.sp, sriso)

#Check for geographic mismatches, there are a few so remove them
length(sr_hair.sp)
length(sr_hair.sp[!is.na(extract(sriso, sr_hair.sp)[2])])
sr_hair.sp = sr_hair.sp[!is.na(extract(sriso, sr_hair.sp)[2])]

#Condense dataframe for calRaster
values(sr_hair.sp) = data.frame(sr_hair.sp$Iso.Value, sr_hair.sp$sd)

#Calibrate
sr_hair.iso = calRaster(sr_hair.sp, sriso)

#Tooth####
#Get tooth Sr data
sr_teeth = subset(ForensicTIsoData, Element == 'teeth' & Isotope == '87Sr/86Sr')
sr_teeth$sd = rep(0.0003)

#Spatial and project
sr_teeth.sp = vect(sr_teeth, geom = c("Lon", "Lat"), crs = "WGS84")
sr_teeth.sp = project(sr_teeth.sp, sriso)

#Check for geographic mismatches
length(sr_teeth.sp)
length(sr_teeth.sp[!is.na(extract(sriso, sr_teeth.sp))[,2]])
sr_teeth.sp = sr_teeth.sp[!is.na(extract(sriso, sr_teeth.sp))[,2]]

#Condense dataframe for calRaster
values(sr_teeth.sp) = data.frame(sr_teeth.sp$Iso.Value, sr_teeth.sp$sd)

#Calibrate
sr_teeth.iso = calRaster(sr_teeth.sp, sriso)

