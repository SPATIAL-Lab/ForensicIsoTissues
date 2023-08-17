library(terra)

#this script starts with the testhair data object created in script
#StopCalibrateandListen.R

#data subsets for the problem studies
ehl = ttsp[substr(ttsp$Sample.ID, 1, 4) == "EHLE",]
tip = ttsp[substr(ttsp$Sample.ID, 1, 4) == "TIPP",]

#initial viz, clearly all Ehl sites are in Tip
plot(tip.sp)
plot(ehl.sp, col = "green", add = TRUE, cex = 0.5)

#initial try at spatial selection of Tip sites that match in Ehl
tip.sub = tip.sp[ehl.sp]

#oops, looks like not all sites were selected
length(ehl.sp)
length(tip.sub)

#reality check...should they have matched? 
#Yes, clearly there's a Tip site at each of the Ehl sites that were not matched
plot(tip.sp)
plot(tip.sub, col = "lightblue", add = TRUE)
plot(ehl.sp, col = "green", add = TRUE, cex = 0.5)

#let's try with a small buffer
ehl.buf = buffer(ehl.sp, 2e4)
tip.sub = tip.sp[ehl.buf]

#that gets 209 returns, but these cover all the sites in the 210 Ehl samples
length(tip.sub)
plot(tip.sp)
plot(tip.sub, col = "lightblue", add = TRUE)
plot(ehl.sp, col = "green", add = TRUE, cex = 0.5)

#so it looks like all the Ehl data except 1 appear in Tip, can we find it?
#shucks, they are even in the same order, looks like its one of the first ~30 samples:
plot(ehl.sp$d18O)
points(tip.sub$d18O, col = "red")

#looks like it's sample 19?
plot(ehl.sp$d18O[1:30])
points(tip.sub$d18O[1:30], col = "red")

#verified
ehl.sp$d18O[15:20]
tip.sub$d18O[15:20]

#look at it, it's from Artesia NM and there's no clear reason to reject it
View(values(ehl.sp))

#so it appears that the best option here is to remove the dupes from Tip
ttsp = ttsp[-match(tip.sub$Sample.ID, ttsp$Sample.ID)]
