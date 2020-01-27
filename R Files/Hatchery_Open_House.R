rm(list=ls())
library(ggplot2)

## Satisfaction scores

scores1 <- c(1,5,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5)
scores2 <- c(5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5)
scores3 <- c(5,5,5,5,1,5,4,1,5,5,5,5,5,5,1,5)
scores4 <- c(5,4,5,4,5,5,5,5,5,5,5,5)
scores5 <- c(5,5,5,5,5,5,5,5,5,5,5,5,5,5,4)
scores_tot <- c(scores1,scores2,scores3,scores4,scores5)

scores1 <- as.data.frame(scores1)
scores2 <- as.data.frame(scores2)
scores3 <- as.data.frame(scores3)
scores4 <- as.data.frame(scores4)
scores5 <- as.data.frame(scores5)
scores_tot <- as.data.frame(scores_tot)

pt <- ggplot(scores_tot, aes(scores_tot)) + geom_bar() + labs(x = "Score (1-5)",
  y = "Count", title = "All Locations") + lims(x = c(.5,5.5))

p1 <- ggplot(scores1, aes(scores1)) + geom_bar() + labs(x = "Score (1-5)",
  y = "Count", title = "Montebello") + lims(x = c(.5,5.5))

p2 <- ggplot(scores2, aes(scores2)) + geom_bar() + labs(x = "Score (1-5)",
  y = "Count", title = "Coursey Springs") + lims(x = c(.5,5.5))

p3 <- ggplot(scores3, aes(scores3)) + geom_bar() + labs(x = "Score (1-5)",
  y = "Count", title = "Paint Bank") + lims(x = c(.5,5.5))

p4 <- ggplot(scores4, aes(scores4)) + geom_bar() + labs(x = "Score (1-5)",
  y = "Count", title = "Wytheville") + lims(x = c(.5,5.5))

p5 <- ggplot(scores5, aes(scores5)) + geom_bar() + labs(x = "Score (1-5)",
  y = "Count", title = "Marion") + lims(x = c(.5,5.5))

p1
p2
p3
p4
p5
pt

########################################################################################################
rm(list=ls())

## Percent fishing license and plan to participate in free fishing day

perc <- matrix(c(12,10,21, 13,9,19, 11,12,16, 9,11,12, 9,8,15), nrow = 15, byrow = T)
perc <- as.data.frame(perc)
perc$Location <- c("Montebello","Montebello","Montebello","Coursey","Coursey","Coursey",
                          "Paint Bank","Paint Bank","Paint Bank","Wytheville","Wytheville",
                          "Wytheville","Marion","Marion","Marion")
perc$Location <- factor(perc$Location, levels=c("Montebello",
                    "Coursey", "Paint Bank", "Wytheville", "Marion"))
perc$ind <- c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3)
perc$ind <- as.factor(perc$ind)

p7 <- ggplot(perc, aes(x = Location, y = V1, group = ind, fill = ind)) +
  geom_col(position = position_dodge(width=-0.45)) + labs(y = "Number of People", title = "By Location")
p7 + scale_fill_discrete(name = "", labels = c("Have License", "Plan to Attend FFD","Attended")) +
  guides(fill = guide_legend(reverse = TRUE)) + theme(legend.position="bottom", text = element_text(size = 15))

#########################################################################################################

## Pie Chart for all locations, CHANGE COLORS IF NEEDED

pie <- data.frame(group = c("No Survey", "Have License & FFD", "Have License, No FFD", "No License & FFD", "No License, No FFD"),
                   value = c(131/214, 37/214, 17/214, 13/214, 16/214))
bp <- ggplot(pie, aes(x = "", y = value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(round(value, 4)*100, "%")), position = position_stack(vjust = 0.5))

piechart <- bp + coord_polar("y", start = 0) + labs(x = "", y = "",
  title = "All Locations") + scale_fill_discrete(name = "")
piechart + theme(legend.position="right", axis.ticks = element_blank(), axis.text = element_blank(), text = element_text(size = 15))


#######################################################################################################

## Distance traveled

library(USAboundaries)
library(USAboundariesData)
library(sf)
library(pracma)

rm(list=ls())

uszipcode <- us_zipcodes()

zips1 <- read.csv(file = "C:/Users/Chandler/Desktop/DGIF Projects/Hatchery Open House/zips_montebello.csv", header = F)
zips1 <- zips1[,-2]

zips2 <- read.csv(file = "C:/Users/Chandler/Desktop/DGIF Projects/Hatchery Open House/zips_coursey.csv", header = F)
zips2 <- zips2[,-2]

zips3 <- read.csv(file = "C:/Users/Chandler/Desktop/DGIF Projects/Hatchery Open House/zips_paint.csv", header = F)
zips3 <- zips3[,-2]

zips4 <- read.csv(file = "C:/Users/Chandler/Desktop/DGIF Projects/Hatchery Open House/zips_wytheville.csv", header = F)
zips4 <- zips4[,-2]

zips5 <- read.csv(file = "C:/Users/Chandler/Desktop/DGIF Projects/Hatchery Open House/zips_marion.csv", header = F)
zips5 <- zips5[,-2]



for(i in 1:21) {
  index[i] <- which(uszipcode$zipcode == zips1[i])
  #entries[i] <- uszipcode[zipcode[i],]
}

vamap <- us_states(states = c("Virginia", "Maryland", "Pennsylvania"))
plot(st_geometry(vamap), main = "Montebello")

#Montebello
uszipcode[19916,7]
uszipcode[3703,7]
uszipcode[24149,7]
uszipcode[5615,7] #2
uszipcode[24206,7]
uszipcode[12310,7]
uszipcode[8226,7]
uszipcode[31906,7]
uszipcode[5456,7]
uszipcode[10526,7]
uszipcode[31150,7] #2
uszipcode[23367,7]
uszipcode[27761,7]
uszipcode[9440,7] #2
uszipcode[23101,7]
uszipcode[21482,7] #2
uszipcode[24111,7]

points(-79.1302045, 37.8445982, col = "red", pch = 20, cex = 1.5)
points(-77.67761, 37.53843, pch = 16)
points(-79.15572, 37.35303, pch = 16)
points(-79.05923, 37.83381, pch = 16)
points(-78.46992, 38.3364, pch = 16, cex = 1.2)
points(-78.2794, 37.96122, pch = 16)
points(-76.8467, 39.68684, pch = 16)
points(-78.69426, 38.15171, pch = 16)
points(-78.56054, 37.3138, pch = 16)
points(-77.27943, 36.91725, pch = 16)
points(-77.04167, 40.80405, pch = 16)
points(-77.74281, 38.72255, pch = 16, cex = 1.2)
points(-77.24684, 37.61152, pch = 16)
points(-79.24958, 37.4533, pch = 16)
points(-78.28835, 37.84923, pch = 16, cex = 1.2)
points(-77.11395, 38.81377, pch = 16)
points(-79.1037, 37.87331, pch = 16, cex = 1.2)
points(-77.53341, 37.65986, pch = 16)

montebello <- c(37.8445982, -79.1302045)
p1 <- c(37.53843, -77.67761)
p2 <- c(37.35303, -79.15572)
p3 <- c(37.83381, -79.05923)
p4 <- c(38.3364, -78.46992)
p5 <- c(38.3364, -78.46992)
p6 <- c(37.96122, -78.2794)
p7 <- c(39.68684, -76.8467)
p8 <- c(38.15171, -78.69426)
p9 <- c(37.3138, -78.56054)
p10 <- c(36.91725, -77.27943)
p11 <- c(40.80405, -77.04167)
p12 <- c(38.72255, -77.74281)
p13 <- c(38.72255, -77.74281)
p14 <- c(37.61152, -77.24684)
p15 <- c(37.4533, -79.24958)
p16 <- c(37.84923, -78.28835)
p17 <- c(37.84923, -78.28835)
p18 <- c(38.81377, -77.11395)
p19 <- c(37.87331, -79.1037)
p20 <- c(37.87331, -79.1037)
p21 <- c(37.65986, -77.53341)

distMonte[1] <- haversine(p1, montebello, 3961)
distMonte[2] <- haversine(p2, montebello, 3961)
distMonte[3] <- haversine(p3, montebello, 3961)
distMonte[4] <- haversine(p4, montebello, 3961)
distMonte[5] <- haversine(p5, montebello, 3961)
distMonte[6] <- haversine(p6, montebello, 3961)
distMonte[7] <- haversine(p7, montebello, 3961)
distMonte[8] <- haversine(p8, montebello, 3961)
distMonte[9] <- haversine(p9, montebello, 3961)
distMonte[10] <- haversine(p10, montebello, 3961)
distMonte[11] <- haversine(p11, montebello, 3961)
distMonte[12] <- haversine(p12, montebello, 3961)
distMonte[13] <- haversine(p13, montebello, 3961)
distMonte[14] <- haversine(p14, montebello, 3961)
distMonte[15] <- haversine(p15, montebello, 3961)
distMonte[16] <- haversine(p16, montebello, 3961)
distMonte[17] <- haversine(p17, montebello, 3961)
distMonte[18] <- haversine(p18, montebello, 3961)
distMonte[19] <- haversine(p19, montebello, 3961)
distMonte[20] <- haversine(p20, montebello, 3961)
distMonte[21] <- haversine(p21, montebello, 3961)

summary(distMonte)


# Coursey Springs
vamap <- us_states(states = c("Virginia", "North Carolina"))
plot(st_geometry(vamap), main = "Coursey Springs")

uszipcode[9766,7]
uszipcode[2504,7] #3
uszipcode[5987,7] #2
uszipcode[1557,7] #2
uszipcode[6872,7]
uszipcode[17839,7]
uszipcode[1827,7] #2
uszipcode[26972,7]
uszipcode[24805,7] #2
uszipcode[23680,7] #2
uszipcode[2646,7]

points(-79.5833731, 38.1780251, col = "red", pch = 20, cex = 1.5)
points(-78.86195, 38.49951, pch = 16)
points(-79.08588, 38.13358, pch = 16, cex = 1.4)
points(-85.70846, 44.75813, pch = 16, cex = 1.2)
points(-77.57531, 38.28342, pch = 16, cex = 1.2)
points(-78.80944, 38.63039, pch = 16)
points(-77.19561, 38.86441, pch = 16)
points(-78.65975, 37.26654, pch = 16, cex = 1.2)
points(-77.61004, 38.96274, pch = 16)
points(-77.48596, 38.74708, pch = 16, cex = 1.2)
points(-78.71143, 35.92533, pch = 16, cex = 1.2)
points(-79.92247, 37.26651, pch = 16)

coursey <- c(38.1780251, -79.5833731)
p1 <- c(38.49951, -78.86195)
p2 <- c(38.13358, -79.08588)
p3 <- c(38.13358, -79.08588)
p4 <- c(38.13358, -79.08588)
p5 <- c(44.75813, -85.70846)
p6 <- c(44.75813, -85.70846)
p7 <- c(38.28342, -77.57531)
p8 <- c(38.28342, -77.57531)
p9 <- c(38.63039, -78.80944)
p10 <- c(38.86441, -77.19561)
p11 <- c(37.26654, -78.65975)
p12 <- c(37.26654, -78.65975)
p13 <- c(38.96274, -77.61004)
p14 <- c(38.74708, -77.48596)
p15 <- c(38.74708, -77.48596)
p16 <- c(35.92533, -78.71143)
p17 <- c(35.92533, -78.71143)
p18 <- c(37.26651, -79.92247)

distCoursey[1] <- haversine(p1, coursey, 3961)
distCoursey[2] <- haversine(p2, coursey, 3961)
distCoursey[3] <- haversine(p3, coursey, 3961)
distCoursey[4] <- haversine(p4, coursey, 3961)
distCoursey[5] <- haversine(p5, coursey, 3961)
distCoursey[6] <- haversine(p6, coursey, 3961)
distCoursey[7] <- haversine(p7, coursey, 3961)
distCoursey[8] <- haversine(p8, coursey, 3961)
distCoursey[9] <- haversine(p9, coursey, 3961)
distCoursey[10] <- haversine(p10, coursey, 3961)
distCoursey[11] <- haversine(p11, coursey, 3961)
distCoursey[12] <- haversine(p12, coursey, 3961)
distCoursey[13] <- haversine(p13, coursey, 3961)
distCoursey[14] <- haversine(p14, coursey, 3961)
distCoursey[15] <- haversine(p15, coursey, 3961)
distCoursey[16] <- haversine(p16, coursey, 3961)
distCoursey[17] <- haversine(p17, coursey, 3961)
distCoursey[18] <- haversine(p18, coursey, 3961)

summary(distCoursey)


# Paint Bank
vamap <- us_states(states = "Virginia")
plot(st_geometry(vamap), main = "Paint Bank")

uszipcode[22153,7]
uszipcode[7012,7] #4
uszipcode[6601,7]
uszipcode[27752,7]
uszipcode[16470,7]
uszipcode[4148,7]
uszipcode[2942,7]
uszipcode[22205,7]
uszipcode[31194,7]
uszipcode[2865,7]
uszipcode[6515,7]
uszipcode[2899,7]
uszipcode[31194,7]

points(-80.245927, 37.5532281, col = "red", pch = 20, cex = 1.5)
points(-80.04235, 37.21062, pch = 16)
points(-80.11064, 37.30016, pch = 16)
points(-81.46741, 37.17593, pch = 16, cex = 1.6)
points(-80.13579, 37.15456, pch = 16)
points(-80.25767, 37.13791, pch = 16)
points(-80.1909, 37.48623, pch = 16)
points(-80.42211, 37.12854, pch = 16)
points(-79.95412, 37.27294, pch = 16)
points(-79.80116, 37.28728, pch = 16)
points(-79.30434, 36.96667, pch = 16)
points(-79.88824, 37.52298, pch = 16)
points(-79.95225, 37.34625, pch = 16)
points(-79.80116, 37.28728, pch = 16)

paint <- c(37.5532281, -80.245927)
p1 <- c(37.21062, -80.04235)
p2 <- c(37.30016, -80.11064)
p3 <- c(37.17593, -81.46741)
p4 <- c(37.17593, -81.46741)
p5 <- c(37.17593, -81.46741)
p6 <- c(37.17593, -81.46741)
p7 <- c(37.15456, -80.13579)
p8 <- c(37.13791, -80.25767)
p9 <- c(37.48623, -80.1909)
p10 <- c(37.12854, -80.42211)
p11 <- c(37.27294, -79.95412)
p12 <- c(37.28728, -79.80116)
p13 <- c(36.96667, -79.30434)
p14 <- c(37.52298, -79.88824)
p15 <- c(37.34625, -79.95225)
p16 <- c(37.28728, -79.80116)

distPaint[1] <- haversine(p1, paint, 3961)
distPaint[2] <- haversine(p2, paint, 3961)
distPaint[3] <- haversine(p3, paint, 3961)
distPaint[4] <- haversine(p4, paint, 3961)
distPaint[5] <- haversine(p5, paint, 3961)
distPaint[6] <- haversine(p6, paint, 3961)
distPaint[7] <- haversine(p7, paint, 3961)
distPaint[8] <- haversine(p8, paint, 3961)
distPaint[9] <- haversine(p9, paint, 3961)
distPaint[10] <- haversine(p10, paint, 3961)
distPaint[11] <- haversine(p11, paint, 3961)
distPaint[12] <- haversine(p12, paint, 3961)
distPaint[13] <- haversine(p13, paint, 3961)
distPaint[14] <- haversine(p14, paint, 3961)
distPaint[15] <- haversine(p15, paint, 3961)
distPaint[16] <- haversine(p16, paint, 3961)

summary(distPaint)


# Wytheville
vamap <- us_states(states = c("Virginia", "Maryland", "Pennsylvania"))
plot(st_geometry(vamap), main = "Wytheville")

uszipcode[3280,7] #3
uszipcode[2442,7]
uszipcode[984,7]
uszipcode[2942,7] #2
uszipcode[12086,7]
uszipcode[6596,7]
uszipcode[2570,7]
uszipcode[14697,7]
uszipcode[30377,7]

points(-80.9012057,36.9323642, col = "red", pch = 20, cex = 1.5)
points(-80.89672, 36.93372, pch = 16, cex = 1.4)
points(-80.50027, 36.86012, pch = 16)
points(-80.70147, 36.76963, pch = 16)
points(-80.42211, 37.12854, pch = 16, cex = 1.2)
points(-81.09974, 36.9603, pch = 16)
points(-81.36639, 37.2448, pch = 16)
points(-80.42091, 37.25672, pch = 16)
points(-80.61552, 37.18419, pch = 16)
points(-76.51874, 39.98391, pch = 16)


wytheville <- c(36.9323642, -80.9012057)
p1 <- c(36.93372, -80.89672)
p2 <- c(36.93372, -80.89672)
p3 <- c(36.93372, -80.89672)
p4 <- c(36.86012, -80.50027)
p5 <- c(36.76963, -80.70147)
p6 <- c(37.12854, -80.42211)
p7 <- c(37.12854, -80.42211)
p8 <- c(36.9603, -81.09974)
p9 <- c(37.2448, -81.36639)
p10 <- c(37.25672, -80.42091)
p11 <- c(37.18419, -80.61552)
p12 <- c(39.98391, -76.51874)

distWytheville[1] <- haversine(p1, wytheville, 3961)
distWytheville[2] <- haversine(p2, wytheville, 3961)
distWytheville[3] <- haversine(p3, wytheville, 3961)
distWytheville[4] <- haversine(p4, wytheville, 3961)
distWytheville[5] <- haversine(p5, wytheville, 3961)
distWytheville[6] <- haversine(p6, wytheville, 3961)
distWytheville[7] <- haversine(p7, wytheville, 3961)
distWytheville[8] <- haversine(p8, wytheville, 3961)
distWytheville[9] <- haversine(p9, wytheville, 3961)
distWytheville[10] <- haversine(p10, wytheville, 3961)
distWytheville[11] <- haversine(p11, wytheville, 3961)
distWytheville[12] <- haversine(p12, wytheville, 3961)

summary(distWytheville)


# Marion
vamap <- us_states(states = c("Virginia", "North Carolina"))
plot(st_geometry(vamap), main = "Marion")

uszipcode[2437,7]
uszipcode[11901,7]
uszipcode[3261,7]
uszipcode[9674,7]
uszipcode[31764,7]
uszipcode[9713,7]
uszipcode[2340,7]
uszipcode[9674,7]
uszipcode[9713,7] #2
uszipcode[3267,7] #2
uszipcode[20851,7] #2

points(-81.5407995, 36.7591809, col = "red", pch = 20, cex = 1.5)
points(-81.29595, 36.88992, pch = 16)
points(-81.37519, 36.20973, pch = 16)
points(-81.47356, 36.3626, pch = 16)
points(-81.69253, 36.91371, pch = 16)
points(-81.84612, 36.76678, pch = 16)
points(-81.54324, 36.82716, pch = 16)
points(-82.11855, 36.86443, pch = 16)
points(-81.69253, 36.91371, pch = 16)
points(-81.54324, 36.82716, pch = 16, cex = 1.2)
points(-82.60644, 36.65889, pch = 16, cex = 1.2)
points(-81.40915, 36.88175, pch = 16, cex = 1.2)

marion <- c(36.7591809, -81.5407995)
p1 <- c(36.88992, -81.29595)
p2 <- c(36.20973, -81.37519)
p3 <- c(36.3626, -81.47356)
p4 <- c(36.91371, -81.69253)
p5 <- c(36.76678, -81.84612)
p6 <- c(36.82716, -81.54324)
p7 <- c(36.86443, -82.11855)
p8 <- c(36.91371, -81.69253)
p9 <- c(36.82716, -81.54324)
p10 <- c(36.82716, -81.54324)
p11 <- c(36.65889, -82.60644)
p12 <- c(36.65889, -82.60644)
p13 <- c(36.88175, -81.40915)
p14 <- c(36.88175, -81.40915)

distMarion[1] <- haversine(p1, marion, 3961)
distMarion[2] <- haversine(p2, marion, 3961)
distMarion[3] <- haversine(p3, marion, 3961)
distMarion[4] <- haversine(p4, marion, 3961)
distMarion[5] <- haversine(p5, marion, 3961)
distMarion[6] <- haversine(p6, marion, 3961)
distMarion[7] <- haversine(p7, marion, 3961)
distMarion[8] <- haversine(p8, marion, 3961)
distMarion[9] <- haversine(p9, marion, 3961)
distMarion[10] <- haversine(p10, marion, 3961)
distMarion[11] <- haversine(p11, marion, 3961)
distMarion[12] <- haversine(p12, marion, 3961)
distMarion[13] <- haversine(p13, marion, 3961)
distMarion[14] <- haversine(p14, marion, 3961)

summary(distMarion)

