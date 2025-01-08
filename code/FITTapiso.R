library (readr); library(assignR); library(terra); library(ggplot2); library(viridis);
library(dplyr); library(tidyterra); library(geodata); library(car): library(ggpubr)

#Isoscapes, Statistical test and Quality Analysis
#This script is used after running the FITDataSetup script, 
#FTID can be run straight from the FITDataSetup without reading in the csv
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
#Read in North American tap (NAtap.tif) and pull in Global strontium isoscapes from assignR
NAtapiso <-rast("shapefiles/NAtap.tif")
NAtapiso <-c(NAtapiso$d18o.m, NAtapiso$d18o.se)

Sriso = getIsoscapes("GlobalSr")
Sriso= crop(Sriso, c(-16653815.4396, 0, 0, 8376837.3753))
Sriso = terra::project(Sriso, crs(namap1))
Sriso <-terra::mask (Sriso, namap1)
Sriso = crop(Sriso, namap1)

##Hair (Building the Isoscapes, followed by statistical analysis and figures)
#Oxygen Hair Isoscapes
#Calibrated hair, using RefTrans
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

#Statistical Tests
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

#Regression Plot
# Calculate means for the center of the data
mean_x <- mean(toTrans1$isoscape.iso)
mean_y <- mean(toTrans1$tissue.iso)
# Calculate the intercept for the line with slope 0.35 
intercept_0.35 <- mean_y - 0.35 * mean_x
# Fit the regression model
model3 <- lm(tissue.iso ~ isoscape.iso, data = toTrans1)
r_squared3 <- summary(model3)$r.squared
reg_eq3 <- paste0("y = ", round(coef(model3)[1], 2), " + ", round(coef(model3)[2], 2), "x")
# Create the plot
ggplot(data = toTrans1, aes(x = isoscape.iso, y = tissue.iso)) +
  geom_point(size = 4, shape = 21, color = "black", stroke = 0.5, fill = "#348FA6") +  # Plot points
  geom_abline(slope = 0.35, intercept = intercept_0.35, color = "black", linetype = "dashed", size = 1.5) +  # Line with slope = 0.35
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.5) +  # Add regression line (no confidence interval)
  labs(
    y = expression(delta^{18} * O[italic(hair)]),  # Y-axis label with subscript
    x = expression(delta^{18} * O[italic(isoscape)])  # X-axis label with subscript
  ) +
  theme(
    panel.background = element_rect(fill = 'white'),  # White panel background
    plot.background = element_rect(fill = 'transparent', color = NA),  # Transparent plot background
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),  # X-axis title size
    axis.title.y = element_text(size = 18,  margin = margin(r = 10)),  # Y-axis title size
    axis.text.x = element_text(size = 14),  # X-axis text size
    axis.text.y = element_text(size = 14),  # Y-axis text size
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) +
  annotate("text", x = Inf, y = -Inf, label = paste("R² =", round(r_squared3, 2)), 
           hjust = 1.1, vjust = -1.5, size = 5) +
  annotate("text", x = Inf, y = -Inf, label = reg_eq3,
           hjust = 1.05, vjust = -3.5, size = 5) +
  coord_cartesian(
    xlim = c(min(toTrans1$isoscape.iso), max(toTrans1$isoscape.iso)),  # Set x-axis limits
    ylim = c(min(toTrans1$tissue.iso), max(toTrans1$tissue.iso)),  # Set y-axis limits
    clip = "on"  
  ) +
  coord_equal()  
ggsave("figures/Figure5A.png")

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
hairSr$Sr.sd <- 0.0003
Srhair <- vect(data.frame("lon" = hairSr$Lon, "lat" = hairSr$Lat, 
                          "Sr" = hairSr$Sr, "Sr.sd" = hairSr$Sr.sd), 
               crs = "WGS84")
hairSrscape = calRaster(Srhair, Sriso)
hairSr$residuals = hairSrscape$lm.model$residuals
hairSr$isoscape.iso = hairSrscape$lm.data$isoscape.iso
hairSr$tissue.iso = hairSrscape$lm.data$tissue.iso
#Calculate standard residual error for isoscape
sd(hairSr$residuals)

#Regression Plot
# Fit the regression model
model_2 <- lm(tissue.iso ~ isoscape.iso, data = hairSr)
r_squared2 <- summary(model_2)$r.squared
reg_eq <- paste0("y = ", round(coef(model_2)[1], 2), " + ", round(coef(model_2)[2], 2), "x")
# Create the plot
ggplot(data = hairSr, aes(x = isoscape.iso, y = tissue.iso)) +
  geom_point(size = 4, shape = 21, color = "black", stroke = 0.5, fill = "#AADC32FF") +  
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1.5) +  # One-to-one line
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.5) +  # Add regression line
  labs(
    y = expression({}^{87} * Sr / {}^{86} * Sr[italic(hair)]),  
    x = expression({}^{87} * Sr / {}^{86} * Sr[italic(isoscape)])  
  ) +
  theme(
    panel.background = element_rect(fill = 'white'),
    plot.background = element_rect(fill = 'transparent', color = NA),
    axis.title.x = element_text(size = 18, margin = margin(t=10)),
    axis.title.y = element_text(size = 18, margin = margin(r=10)),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) +
  annotate("text", x = Inf, y = -Inf, label = paste("R² =", round(r_squared2, 2)),
           hjust = 1.1, vjust = -1.5, size = 5) +  # Add R-squared to the plot
  annotate("text", x = Inf, y = -Inf, label = reg_eq,
           hjust = 1.05, vjust = -3.5, size = 5) +  # Add regression equation
  coord_equal()  
ggsave("figures/Figure3A.png")

##Teeth
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
#Calculate the reduced major axis regression
sd(teethO$d18O)/sd(teethO$isoscape.iso)

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

#Regression plot
# Calculate means for the center of the data, then calculate the intercept for the line with slope 0.79
#This adds in the predicted body water line
mean_x <- mean(teethO$isoscape.iso)
mean_y <- mean(teethO$tissue.iso)
intercept_0.79 <- mean_y - 0.79 * mean_x
# Fit the regression model
model4 <- lm(tissue.iso ~ isoscape.iso, data = teethO)
r_squared4 <- summary(model4)$r.squared
reg_eq <- paste0("y = ", round(coef(model4)[1], 2), " + ", round(coef(model4)[2], 2), "x")
# Calculate axis limits to make the plot square
x_range <- range(teethO$isoscape.iso)
y_range <- range(teethO$tissue.iso)
axis_range <- range(c(x_range, y_range))  # Take the maximum range of both axes
# Create the plot
ggplot(data = teethO, aes(x = isoscape.iso, y = tissue.iso)) +  
  geom_point(size = 4, shape = 21, color = "black", stroke = 0.5, fill = "#348FA6") +  
  geom_abline(slope = 0.79, intercept = intercept_0.79, color = "black", linetype = "dashed", size = 1.5) +  # Line with slope = 0.79
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.5) +  # Add regression line (no confidence interval)
  labs(
    y = expression(delta^{18} * O[italic(enamel)]), 
    x = expression(delta^{18} * O[italic(isoscape)])  
  ) +
  theme(
    panel.background = element_rect(fill = 'white'),  
    plot.background = element_rect(fill = 'transparent', color = NA), 
    axis.title.x = element_text(size = 18, margin = margin (t=10)), 
    axis.title.y = element_text(size = 18, margin = margin (r=10)),  
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),  
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    legend.position = "none",  # Remove legend
    panel.border = element_rect(color = "black", fill = NA, size = 1) 
  ) +  
  annotate("text", x = Inf, y = -Inf, label = paste("R² =", round(r_squared4, 2)),  # Add R² annotation
           hjust = 1.1, vjust = -1.5, size = 5) +
  annotate("text", x = Inf, y = -Inf, label = reg_eq,  # Add regression equation annotation
           hjust = 1.05, vjust = -3.5, size = 5) +  
  coord_cartesian(
    xlim = c(min(teethO$isoscape.iso), max(teethO$isoscape.iso)),  # Set x-axis limits
    ylim = c(min(teethO$tissue.iso), max(teethO$tissue.iso)),  # Set y-axis limits
    clip = "on"  # Clip the plot to the limits
  ) +  
  coord_equal() +  # Ensure equal scaling of axes
  theme(
    aspect.ratio = 1  # Optional: Forces the plot to be square if using in RStudio or plotting window
  )
ggsave("figures/Figure5B.png")

#Density Plots
#Oxygen residuals by tooth group
teethO_filtered <- teethO %>%
  mutate(Tooth.group = as.character(Tooth.group)) %>% 
  filter(Tooth.group != "Unknown")
# Check counts per Tooth.group, Create new labels with counts
counts2 <- teethO_filtered %>% 
  group_by(Tooth.group) %>% 
  summarise(n = n())
new_labels2 <- paste0(counts2$Tooth.group, " (n=", counts2$n, ")")
names(new_labels2) <- counts2$Tooth.group
# Density plot
ggplot() + 
  geom_density(data = teethO_filtered, 
               aes(x = residuals, color = Tooth.group), linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
  scale_color_viridis(discrete = TRUE, option = 'D', labels = new_labels2) +
  labs(
    x = expression(delta^{18}*O[italic(enamel)] ~ "residuals"), 
    y = "Density", 
    color = "Tooth Group"
  ) +
  theme(
    panel.background = element_rect(fill = 'white'),
    plot.background = element_rect(fill = 'transparent', color = NA),
    legend.background = element_rect(fill = 'transparent'),
    legend.box.background = element_rect(fill = 'transparent'),  
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 18),
    axis.title.x = element_text(size = 18, margin = margin(t=10)),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )
ggsave("figures/Figure7.png")

#Oxygen residuals by Reference ID
RefIDsupdate <-read_excel("data/FIT_RefIDsRefNo.xlsx")
teethO <- teethO %>%
  inner_join(RefIDsupdate, by = "Reference.ID")
# Calculate counts per Reference.ID
counts4 <- teethO %>%
  filter(!is.na(Reference_ID_2)) %>%
  group_by(Reference_ID_2) %>%
  summarise(n = n())
# Create new labels with counts
new_labels4 <- paste0(counts4$Reference_ID_2, " (n=", counts4$n, ")")
names(new_labels4) <- counts4$Reference_ID_2
# Plot with updated legend labels including counts
ggplot() + 
  geom_density(data = subset(teethO, !is.na(Reference_ID_2)), 
               aes(x = residuals, color = Reference_ID_2), 
               linewidth = 1, alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1)+
  scale_fill_viridis(discrete = TRUE, option = 'D') + 
  scale_color_viridis(discrete = TRUE, option = 'D', labels = new_labels4) + 
  labs(
    x = expression(delta^{18}*O[italic(enamel)] ~ "residuals"), 
    y = "Density", 
    color = "Reference ID"  # Legend title for color
  ) +
  theme(
    panel.background = element_rect(fill = 'white'),
    plot.background = element_rect(fill = 'transparent', color = NA),
    legend.background = element_rect(fill = 'transparent'),
    legend.box.background = element_rect(fill = 'transparent'),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15),
    axis.title.x = element_text(size = 18, margin = margin(t=10)),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )
ggsave("figures/Figure6.png")

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

# QA 3, Bias correct using residuals based on Tooth group
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
plot(tOQA1, tOQA2, tOQA3)

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

#Statistical tests
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

#Regression plot
# Fit the regression model using the full dataset
model <- lm(tissue.iso ~ isoscape.iso, data = teethSr)
r_squared <- summary(model)$r.squared
# Manually specify the desired equation annotation
reg_eq <- "y = 0.33 + 0.53x"
# Calculate the range for both axes
axis_range <- range(teethSr$isoscape.iso)
# Create the plot
ggplot(data = teethSr, aes(x = isoscape.iso, y = tissue.iso)) +  
  geom_point(size = 4, shape = 21, color = "black", stroke = 0.5, fill = "#AADC32FF") + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1.5) +  # One-to-one line
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.5) +  # Add regression line
  labs(
    y = expression({}^{87} * Sr / {}^{86} * Sr[italic(enamel)]),  
    x = expression({}^{87} * Sr / {}^{86} * Sr[italic(isoscape)])  
  ) +  
  theme(
    panel.background = element_rect(fill = 'white'),  
    plot.background = element_rect(fill = 'transparent', color = NA), 
    axis.title.x = element_text(size = 18, margin = margin(t=10)), 
    axis.title.y = element_text(size = 18, margin = margin(t=10)),  
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14), 
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    legend.position = "none", 
    panel.border = element_rect(color = "black", fill = NA, size = 1) 
  ) +  
  annotate("text", x = max(axis_range), y = min(axis_range), label = paste("R² =", round(r_squared, 2)),  # Annotate R²
           hjust = .83, vjust = 0.2, size = 5) +  
  annotate("text", x = max(axis_range), y = min(axis_range), label = reg_eq,  # Annotate regression equation
           hjust = .91, vjust = -1.6, size = 5) +  
  coord_equal(  # Use only this to ensure equal scaling of axes
    xlim = axis_range,  
    ylim = axis_range  
  )
ggsave("figures/Figure3B.png")

#Density plot of Sr with Reference IDs
teethSr <- teethSr %>%
  inner_join(RefIDsupdate, by = "Reference.ID")
# Calculate counts 
counts_Sr <- teethSr %>%
  filter(!is.na(Reference_ID_2)) %>%
  group_by(Reference_ID_2) %>%
  summarise(n = n())
# Create new labels with counts
new_labels_Sr <- paste0(counts_Sr$Reference_ID_2, " (n=", counts_Sr$n, ")")
names(new_labels_Sr) <- counts_Sr$Reference_ID_2
# Plot with updated legend labels and all Reference.ID counts
ggplot() + 
  geom_density(data = subset(teethSr, !is.na(Reference_ID_2)), 
               aes(x = residuals, color = Reference_ID_2), 
               linewidth = 1, alpha = 0.7)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
  scale_fill_viridis(discrete = TRUE, option = 'D') + 
  scale_color_viridis(discrete = TRUE, option = 'D', labels = new_labels_Sr) + 
  labs(
    x = expression({}^{87} * Sr / {}^{86} * Sr[italic(enamel)] ~ "residuals"), 
    y = "Density", 
    color = "Reference ID"  # Legend title for color
  ) + 
  theme(
    panel.background = element_rect(fill = 'white'),
    plot.background = element_rect(fill = 'transparent', color = NA),
    legend.background = element_rect(fill = 'transparent'),
    legend.box.background = element_rect(fill = 'transparent'),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15),
    axis.title.x = element_text(size = 18, margin = margin(t=10)),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = c(0.05, 0.95),
    legend.justification = c("left", "top"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )
ggsave("figures/Figure4.png")


    