rm(list=ls())

library(dplyr)
library(reshape2)

#### 2013-14 ####

### Killed
harv1314 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Small Game/CSV/HS2013-14_HARV.csv", header = T)

PQuail1314 <- sum(harv1314$PQuailKill, na.rm = T) # Put (Pen) Quail
WQuail1314 <- sum(harv1314$WQuailKill, na.rm = T) # Wild Quail
Grouse1314 <- sum(harv1314$GrouseKill, na.rm = T)
Wcock1314 <- sum(harv1314$WCockKill, na.rm = T)
GSquirrel1314 <- sum(harv1314$GSquirrelKill, na.rm = T)
FSquirrel1314 <- sum(harv1314$FSquirrelKill, na.rm = T)
Rabbit1314 <- sum(harv1314$RabbitKill, na.rm = T)

killed1314 <- c(PQuail1314, WQuail1314, Grouse1314, Wcock1314, GSquirrel1314, FSquirrel1314, Rabbit1314)/929

### Days
PQuailDays1314 <- sum(harv1314$PQuailDays, na.rm = T)
WQuailDays1314 <- sum(harv1314$WQuailDays, na.rm = T)
GrouseDays1314 <- sum(harv1314$GrouseDays, na.rm = T)
WcockDays1314 <- sum(harv1314$WCockDays, na.rm = T)
GSquirrelDays1314 <- sum(harv1314$GSquirrelDays, na.rm = T)
FSquirrelDays1314 <- sum(harv1314$FSquirrelDays, na.rm = T)
RabbitDays1314 <- sum(harv1314$RabbitDays, na.rm = T)

days1314 <- c(PQuailDays1314, WQuailDays1314, GrouseDays1314, WcockDays1314, GSquirrelDays1314, FSquirrelDays1314, RabbitDays1314)

############################################################################################################################################

#### 2011-12 ####

### Killed
harv1112 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Small Game/CSV/HS2011-12_HARV.csv", header = T)

PQuail1112 <- sum(harv1112$PQuailKill, na.rm = T)
WQuail1112 <- sum(harv1112$WQuailKill, na.rm = T)
Grouse1112 <- sum(harv1112$GrouseKill, na.rm = T)
Wcock1112 <- sum(harv1112$WCockKill, na.rm = T)   ## Not in File
GSquirrel1112 <- sum(harv1112$GsquirrelKill, na.rm = T)
FSquirrel1112 <- sum(harv1112$FsquirrelKill, na.rm = T)
Rabbit1112 <- sum(harv1112$RabbitKill, na.rm = T)

killed1112 <- c(PQuail1112, WQuail1112, Grouse1112, Wcock1112, GSquirrel1112, FSquirrel1112, Rabbit1112)/2006

### Days
PQuailDays1112 <- sum(harv1112$QuailDays, na.rm = T) ## P and W combined
WQuailDays1112 <- sum(harv1112$WQuailDays, na.rm = T) ##
GrouseDays1112 <- sum(harv1112$GrouseDays, na.rm = T)
WcockDays1112 <- sum(harv1112$WCockDays, na.rm = T)   ## Not in File
GSquirrelDays1112 <- sum(harv1112$GsquirrelDays, na.rm = T)
FSquirrelDays1112 <- sum(harv1112$FsquirrelDays, na.rm = T)
RabbitDays1112 <- sum(harv1112$RabbitDays, na.rm = T)

days1112 <- c(PQuailDays1112, WQuailDays1112, GrouseDays1112, WcockDays1112, GSquirrelDays1112, FSquirrelDays1112, RabbitDays1112)

############################################################################################################################################

#### 2009-10 ####

### Killed
harv0910 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Small Game/CSV/HS2009-10_HARV.csv", header = T)

Quail0910 <- sum(harv0910$QUAILHAR, na.rm = T)  ## Only one Quail
Grouse0910 <- sum(harv0910$GrouseKill, na.rm = T) ## Not in File
Wcock0910 <- sum(harv0910$WCockKill, na.rm = T)   ## Not in File
GSquirrel0910 <- sum(harv0910$GSHAR, na.rm = T)
FSquirrel0910 <- sum(harv0910$FSHAR, na.rm = T)
Rabbit0910 <- sum(harv0910$RABHAR, na.rm = T)

killed0910 <- c(Quail0910, NA, Grouse0910, Wcock0910, GSquirrel0910, FSquirrel0910, Rabbit0910)/1883

### Days
QuailDays0910 <- sum(harv0910$QUAILDAY, na.rm = T)  ## Only one Quail
GrouseDays0910 <- sum(harv0910$GrouseDays, na.rm = T) ## Not in File
WcockDays0910 <- sum(harv0910$WCockDays, na.rm = T)   ## Not in File
GSquirrelDays0910 <- sum(harv0910$GSDAY, na.rm = T)
FSquirrelDays0910 <- sum(harv0910$FSDAY, na.rm = T)
RabbitDays0910 <- sum(harv0910$RABDAY, na.rm = T)

days0910 <- c(QuailDays0910, NA, GrouseDays0910, WcockDays0910, GSquirrelDays0910, FSquirrelDays0910, RabbitDays0910)

############################################################################################################################################

#### 2008-09 ####

### Killed
harv0809 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Small Game/CSV/HS2008-09_HARV.csv", header = T)

Quail0809 <- sum(harv0809$Q3QUAIL, na.rm = T)  ## Only one Quail
Grouse0809 <- sum(harv0809$Q3RFGR, na.rm = T)
Wcock0809 <- sum(harv0809$Q3WDCK, na.rm = T)
GSquirrel0809 <- sum(harv0809$Q3GRSQ, na.rm = T)
FSquirrel0809 <- sum(harv0809$Q3FXSQ, na.rm = T)
Rabbit0809 <- sum(harv0809$Q3RBT, na.rm = T)

killed0809 <- c(Quail0809, NA, Grouse0809, Wcock0809, GSquirrel0809, FSquirrel0809, Rabbit0809)/1927

### Days
QuailDays0809 <- sum(harv0809$QUALDAY, na.rm = T)  ## Only one Quail
GrouseDays0809 <- sum(harv0809$RFGRDAY, na.rm = T)
WcockDays0809 <- sum(harv0809$WDCCKDAY, na.rm = T)
GSquirrelDays0809 <- sum(harv0809$GRSQDAY, na.rm = T)
FSquirrelDays0809 <- sum(harv0809$FXSQDAY, na.rm = T)
RabbitDays0809 <- sum(harv0809$RBTDAY, na.rm = T)

days0809 <- c(QuailDays0809, NA, GrouseDays0809, WcockDays0809, GSquirrelDays0809, FSquirrelDays0809, RabbitDays0809)

############################################################################################################################################

#### 2007-08 ####

### Killed
harv0708 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Small Game/CSV/HS2007-08_HARV.csv", header = T)

Quail0708 <- sum(harv0708$Quakill, na.rm = T)  ## Only one Quail
Grouse0708 <- sum(harv0708$Grskill, na.rm = T)
Wcock0708 <- sum(harv0708$Wdckkill, na.rm = T)
GSquirrel0708 <- sum(harv0708$GrSqkill, na.rm = T)
FSquirrel0708 <- sum(harv0708$FxSqkill, na.rm = T)
Rabbit0708 <- sum(harv0708$Rabkill, na.rm = T)

killed0708 <- c(Quail0708, NA, Grouse0708, Wcock0708, GSquirrel0708, FSquirrel0708, Rabbit0708)/2027

### Days
QuailDays0708 <- sum(harv0708$QuaDay, na.rm = T)  ## Only one Quail
GrouseDays0708 <- sum(harv0708$GrsDay, na.rm = T)
WcockDays0708 <- sum(harv0708$WdckDay, na.rm = T)
GSquirrelDays0708 <- sum(harv0708$GrSqDay, na.rm = T)
FSquirrelDays0708 <- sum(harv0708$FxSqDay, na.rm = T)
RabbitDays0708 <- sum(harv0708$RabDay, na.rm = T)

days0708 <- c(QuailDays0708, NA, GrouseDays0708, WcockDays0708, GSquirrelDays0708, FSquirrelDays0708, RabbitDays0708)

############################################################################################################################################

#### 2006-07 ####

### Killed
harv0607 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Small Game/CSV/HS2006-07_HARV.csv", header = T)

Quail0607 <- sum(harv0607$Quakill, na.rm = T)  ## Only one Quail
Grouse0607 <- sum(harv0607$Grskill, na.rm = T)
Wcock0607 <- sum(harv0607$Wdckkill, na.rm = T)
GSquirrel0607 <- sum(harv0607$GrSqkill, na.rm = T)
FSquirrel0607 <- sum(harv0607$FxSqkill, na.rm = T)
Rabbit0607 <- sum(harv0607$Rabkill, na.rm = T)

killed0607 <- c(Quail0607, NA, Grouse0607, Wcock0607, GSquirrel0607, FSquirrel0607, Rabbit0607)/1818

### Days
QuailDays0607 <- sum(harv0607$QuaDay, na.rm = T)  ## Only one Quail
GrouseDays0607 <- sum(harv0607$GrsDay, na.rm = T)
WcockDays0607 <- sum(harv0607$WdckDay, na.rm = T)
GSquirrelDays0607 <- sum(harv0607$GrSqDay, na.rm = T)
FSquirrelDays0607 <- sum(harv0607$FxSqDay, na.rm = T)
RabbitDays0607 <- sum(harv0607$RabDay, na.rm = T)

days0607 <- c(QuailDays0607, NA, GrouseDays0607, WcockDays0607, GSquirrelDays0607, FSquirrelDays0607, RabbitDays0607)

############################################################################################################################################

#### 2005-06 ####

### Killed
harv0506 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Small Game/CSV/HS2005-06_HARV.csv", header = T)

Quail0506 <- sum(harv0506$quakill, na.rm = T)  ## Only one Quail
Grouse0506 <- sum(harv0506$grskill, na.rm = T)
Wcock0506 <- sum(harv0506$wdckkill, na.rm = T)
GSquirrel0506 <- sum(harv0506$grsqkill, na.rm = T)
FSquirrel0506 <- sum(harv0506$fxsqkill, na.rm = T)
Rabbit0506 <- sum(harv0506$rabkill, na.rm = T)

killed0506 <- c(Quail0506, NA, Grouse0506, Wcock0506, GSquirrel0506, FSquirrel0506, Rabbit0506)/1360

### Days
QuailDays0506 <- sum(harv0506$quaday, na.rm = T)  ## Only one Quail
GrouseDays0506 <- sum(harv0506$grsday, na.rm = T)
WcockDays0506 <- sum(harv0506$wdckday, na.rm = T)
GSquirrelDays0506 <- sum(harv0506$grsqday, na.rm = T)
FSquirrelDays0506 <- sum(harv0506$fxsqday, na.rm = T)
RabbitDays0506 <- sum(harv0506$rabday, na.rm = T)

days0506 <- c(QuailDays0506, NA, GrouseDays0506, WcockDays0506, GSquirrelDays0506, FSquirrelDays0506, RabbitDays0506)

############################################################################################################################################

#### 2004-05 ####

### Killed
harv0405 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Small Game/CSV/HS2004-05_HARV.csv", header = T)

Quail0405 <- sum(harv0405$quakill, na.rm = T)  ## Only one Quail
Grouse0405 <- sum(harv0405$grskill, na.rm = T)
Wcock0405 <- sum(harv0405$wdckkill, na.rm = T)
GSquirrel0405 <- sum(harv0405$grsqkill, na.rm = T)
FSquirrel0405 <- sum(harv0405$fxsqkill, na.rm = T)
Rabbit0405 <- sum(harv0405$rabkill, na.rm = T)

killed0405 <- c(Quail0405, NA, Grouse0405, Wcock0405, GSquirrel0405, FSquirrel0405, Rabbit0405)/1414

### Days
QuailDays0405 <- sum(harv0405$quaday, na.rm = T)  ## Only one Quail
GrouseDays0405 <- sum(harv0405$grsday, na.rm = T)
WcockDays0405 <- sum(harv0405$wdckday, na.rm = T)
GSquirrelDays0405 <- sum(harv0405$grsqday, na.rm = T)
FSquirrelDays0405 <- sum(harv0405$fxsqday, na.rm = T)
RabbitDays0405 <- sum(harv0405$rabday, na.rm = T)

days0405 <- c(QuailDays0405, NA, GrouseDays0405, WcockDays0405, GSquirrelDays0405, FSquirrelDays0405, RabbitDays0405)

###################################################################################################################################

killed <- rbind(killed0405, killed0506, killed0607, killed0708, killed0809, killed0910, killed1112, killed1314)
killed[6,5] <- NA
killed[6,4] <- NA
killed[7,5] <- NA

killed <- matrix(data = c("Quail",3406,396,570,525,736,383,NA,893,NA,680,"Wild Quail",NA,NA,NA,NA,NA,NA,NA,198,NA,404,"Grouse",3134,183,169,155,107,NA,NA,71,NA,28,"Woodcock",14,18,64,16,38,NA,NA,NA,NA,31,"Gray Squirrel",17201,3728,6686,5710,5005,4243,NA,4996,NA,2274,"Fox Squirrel",11543,522,922,875,637,508,NA,711,NA,386,"Rabbit",11760,1675,2905,2398,2814,1703,NA,2232,NA,1043),nrow = 7,byrow = T)
killed <- as.data.frame(killed)
colnames(killed) <- c("Animal", "04-05", "05-06", "06-07", "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14")

## melt changes the data frame from wide to long
killed <- melt(killed, id="Animal")
killed$value <- as.numeric(killed$value)

ggplot(killed, aes(x = variable, y = value, group = Animal, col = Animal)) + geom_line(size = 1.5) + geom_point(size = 2.5) + scale_color_brewer(palette = "Set1") +
  geom_segment(aes(x = "08-09", y = 107, xend = "11-12", yend = 71), linetype = "dashed", col = "#4DAF4A", size = 1.25) +
  geom_segment(aes(x = "08-09", y = 38, xend = "13-14", yend = 31), linetype = "dashed", col = "#A65628", size = 1.25) +
  geom_segment(aes(x = "09-10", y = 4243, xend = "11-12", yend = 4996), linetype = "dashed", col = "#377EB8", size = 1.25) +
  geom_segment(aes(x = "09-10", y = 1703, xend = "11-12", yend = 2232), linetype = "dashed", col = "#FF7F00", size = 1.25) +
  geom_segment(aes(x = "09-10", y = 508, xend = "11-12", yend = 711), linetype = "dashed", col = "#E41A1C", size = 1.25) +
  geom_segment(aes(x = "09-10", y = 383, xend = "11-12", yend = 893), linetype = "dashed", col = "#984EA3", size = 1.25) +
  
  geom_segment(aes(x = "11-12", y = 4996, xend = "13-14", yend = 2274), linetype = "dashed", col = "#377EB8", size = 1.25) +
  geom_segment(aes(x = "11-12", y = 2232, xend = "13-14", yend = 1043), linetype = "dashed", col = "#FF7F00", size = 1.25) +
  geom_segment(aes(x = "11-12", y = 711, xend = "13-14", yend = 386), linetype = "dashed", col = "#E41A1C", size = 1.25) +
  geom_segment(aes(x = "11-12", y = 893, xend = "13-14", yend = 680), linetype = "dashed", col = "#984EA3", size = 1.25) +
  geom_segment(aes(x = "11-12", y = 71, xend = "13-14", yend = 31), linetype = "dashed", col = "#4DAF4A", size = 1.25) +
  geom_segment(aes(x = "11-12", y = 198, xend = "13-14", yend = 404), linetype = "dashed", col = "#FFFF33", size = 1.25)
  







