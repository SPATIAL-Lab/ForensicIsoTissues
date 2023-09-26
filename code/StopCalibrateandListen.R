library(terra);library(sf); library(readr); library(assignR); library(dplyr); library(assignR)
library(viridisLite)
#Hair####

#Read data
ForensicTIsoData <- read_csv("data/ForensicTissue4.csv", 
                             col_types = cols(...1 = col_skip()))

#Only hair d18O
testhair <- subset(ForensicTIsoData, Element == 'hair' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value,
        d18O_cal = Calibrate)

#add in SD
testhair$d18O.sd <-0.3

#reftrans can't handle data with no calibration scale
toTrans = data.frame(testhair[!is.na(testhair$d18O_cal),])

#Also let's remove sites that don't fall on the isoscape
#First make a spatial version of the data
ttsp = vect(toTrans, geom = c("Lon", "Lat"), crs = "WGS84", keepgeom = TRUE)

#now convert back to aspatial version for the refTrans
#this is where to remove values from tipple ds with TipFix.R
#I've kept that as a separate script for now because ultimately I think that
#should be dealt with at the database level
toTrans = values(ttsp, as.data.frame = TRUE)

#trying the refTrans, needs calibration scale field for the selected marker
e = refTrans(toTrans, marker = "d18O", ref_scale = "VSMOW_O")

#let's compare pre and post trans; sample order is different so we need
#to resort them using a common field
e$data = e$data[order(e$data$Sample.ID),]
toTrans = toTrans[order(toTrans$Sample.ID),]

#plot d18O
plot(toTrans$d18O, e$data$d18O, 
     col = match(toTrans$d18O_cal, unique(toTrans$d18O_cal)))

legend("topleft", col = seq_along(unique(toTrans$d18O_cal)), 
       pch = 1, unique(toTrans$d18O_cal))
abline(0, 1)

#Make a spatial version of the calibrated data
hsp.cal = vect(data.frame("lon" = e$data$Lon, "lat" = e$data$Lat, 
                        "d18O" = e$data$d18O, "d18O.sd" = e$data$d18O.sd), 
             crs = "WGS84")

#Oxygen isoscape
prpiso = getIsoscapes("GlobalPrecipMA")
prpiso = c(prpiso$d18o_MA, prpiso$d18o_se_MA)

#Calibrate
hairscape.cal = calRaster(hsp.cal, prpiso)

plot(hairscape.cal$lm.data$isoscape.iso, hairscape.cal$lm.data$tissue.iso, 
     pch = 21, bg = match(toTrans$d18O_cal, unique(toTrans$d18O_cal)),
     main = "Transformed",
     xlab = expression("Precipitation "*delta^{18}*"O"),
     ylab = expression("Hair "*delta^{18}*"O"))

legend("topleft", pt.bg = seq_along(unique(toTrans$d18O_cal)), 
       pch = 21, unique(toTrans$d18O_cal), cex = 0.75)

#now isoscape uncalibrated oxygen hairs
hsp.orig = vect(data.frame("lon" = toTrans$Lon, "lat" = toTrans$Lat, 
                        "d18O" = toTrans$d18O, "d18O.sd" = toTrans$d18O.sd), 
             crs = "WGS84")

hairscape.orig = calRaster(hsp.orig, prpiso)

plot(hairscape.orig$lm.data$isoscape.iso, hairscape.orig$lm.data$tissue.iso, 
     bg = match(toTrans$d18O_cal, unique(toTrans$d18O_cal)), pch = 21,
     main = "Untransformed",
     xlab = expression("Precipitation "*delta^{18}*"O"),
     ylab = expression("Hair "*delta^{18}*"O"))

legend("topleft", pt.bg = seq_along(unique(toTrans$d18O_cal)), 
       pch = 21, unique(toTrans$d18O_cal), cex = 0.75)

#residuals for the two different approaches, classed by original calibration
cals = unique(toTrans$d18O_cal)
plot(density(hairscape.cal$lm.model$residuals[toTrans$d18O_cal == cals[1]]),
     main = "Calibrated", ylim = c(0,0.5))
for(i in 2:length(cals)){
  lines(density(hairscape.cal$lm.model$residuals[toTrans$d18O_cal == cals[i]]),
       col = i)
}
legend("topleft", col = seq_along(unique(toTrans$d18O_cal)), 
       lty = 1, unique(toTrans$d18O_cal))

#residuals for the two different approaches, classed by original calibration
plot(density(hairscape.orig$lm.model$residuals[toTrans$d18O_cal == cals[1]]),
     main = "Uncalibrated", ylim = c(0,0.5))
for(i in 2:length(cals)){
  lines(density(hairscape.orig$lm.model$residuals[toTrans$d18O_cal == cals[i]]),
        col = i)
}
legend("topleft", col = seq_along(unique(toTrans$d18O_cal)), 
       lty = 1, unique(toTrans$d18O_cal))

#test for equal means of residuals from different orig calibrations
oneway.test(hairscape.cal$lm.model$residuals ~ toTrans$d18O_cal, var.equal = FALSE)
oneway.test(hairscape.orig$lm.model$residuals ~ toTrans$d18O_cal, var.equal = FALSE)
#slightly less sig. differnent for calibrated, but differences exist for both treatments 

#Teeth####

#teeth time, teeth oxygen isoscape
teeth <- subset(ForensicTIsoData, Element == 'teeth' & Isotope == 'd18O') %>% 
  rename(d18O  = Iso.Value,
         d18O_cal = Calibrate)

teeth$d18O.sd <- 0.3

#One outlier needs removing
teeth = teeth[!(teeth$d18O == min(teeth$d18O)),]

#Make spatial
teethsp = vect(data.frame("lon" = teeth$Lon, "lat" = teeth$Lat, 
                      "d18O" = teeth$d18O, "d18O.sd" = teeth$d18O.sd),
           crs = "WGS84")

#Model fit
teethscape = calRaster(teethsp, prpiso)

#Get prefix that hopefully is unique per study
stud = substr(teeth$Sample.ID, 1, 4)
studs = unique(stud)

par(mar = c(5,5,1,1))
plot(teethscape$lm.data$isoscape.iso, teethscape$lm.data$tissue.iso,
     bg = match(stud, studs), pch = 21, 
     xlab = expression("Precipitation "*delta^{18}*"O"),
     ylab = expression("Tooth "*delta^{18}*"O"))

#for convenience
residue = teethscape$lm.model$residuals

#residual corrected dataset
offs = numeric(length(studs))
for(i in seq_along(studs)){
  offs[i] = mean(residue[stud == studs[i]])
}
teethsp.stud = teethsp
teethsp.stud$d18O = teethsp.stud$d18O - offs[match(stud, studs)]

#Check new model fit
teethscape2 = calRaster(teethsp.stud, prpiso)

#Get site IDs
teethsp.uni = subset(teethsp, !(duplicated(geom(teethsp)[,3:4])))

si = match(geom(teethsp)[,3] * geom(teethsp)[,4], 
           geom(teethsp.uni)[,3] * geom(teethsp.uni)[,4])

teethsp$Site_ID = si
teethsp.stud$Site_ID = si

#Mask and QAs
msk = vect(matrix(c(-50, -170, -170, -50, 12, 12, 72, 72), ncol = 2), type = "polygons", 
           crs = "WGS84")

qa1 = QA(teethsp, prpiso, mask = msk, valiStation = 30, by = 5)
qa2 = QA(teethsp.stud, prpiso, mask = msk, valiStation = 35, by = 5)

plot(qa1, qa2)



#remove one study that's a singleton
studs = studs[studs != "KRAM"]
residue = residue[stud %in% studs]
stud = stud[stud %in% studs]

#colors
pal = viridis(length(studs))

#residuals for the two different approaches, classed by original calibration
plot(density(residue[stud == studs[1]]),
     main = "", ylim = c(0,0.8), col = pal[1], lwd = 3, 
     xlab = expression(delta^{18}*"O residual"))
for(i in 2:length(studs)){
  lines(density(residue[stud == studs[i]]),
        col = pal[i], lwd = 3)
}
legend("topleft", col = pal, lty = 1, lwd = 3, studs, cex = 0.95)
box()

#ANOVA, big effect
oneway.test(residue ~ stud, var.equal = FALSE)

#class by tooth mineralization age
tg = teeth$Tooth.group

#fix tooth group value for better sortability
tg[tg == ".5-6yrs"] = "0.5-6yrs"

#unique vals
tgs = unique(tg)
tgs = sort(tgs)

#for convenience
residue = teethscape$lm.model$residuals

#remove NAs
residue = residue[!is.na(tg)]
tg = tg[!is.na(tg)]
tgs = tgs[!is.na(tgs)]

#colors
pal = viridis(length(tgs))

#residuals for the two different approaches, classed by original calibration
plot(density(residue[tg == tgs[1]]),
     main = "", ylim = c(0,0.4), col = pal[1], lwd = 3, 
     xlab = expression(delta^{18}*"O residual"), xlim = c(-4.5, 5))
for(i in 2:length(tgs)){
  lines(density(residue[tg == tgs[i]]),
        col = pal[i], lwd = 3)
}
legend("topleft", col = pal, lty = 1, lwd = 3, tgs, cex = 0.95)
box()
