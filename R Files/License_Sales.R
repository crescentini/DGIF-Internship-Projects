rm(list=ls())


ggplot(license, aes(license$Date)) + geom_bar()

count <- table(license$County)

## All licenses sold in the zip codes in which people attended FFD from.
zipsx <- license[license$ZipCode %in% zips12,]

ggplot(zipsx, aes(zipsx$Date)) + geom_bar()

plot(x/30658)
plot(y/100670)

y <- table(license$Date)
x <- table(zipsx$Date)
z = y-x

xstan <- x/3883
ystan <- y/100670
zstan <- z/(100670-30658)

xstan <- x/56957
ystan <- y/189923


## Standardized comparison
barplot(xstan, ylim = c(0.0, 0.05), col = rgb(red = 0, blue = 1, green = 0, alpha = 0.5), main = "Standardized Daily License Sales Data - Burke Lake Area")
barplot(ystan, ylim = c(0.0, 0.05), add = T, col = rgb(red = 1, blue = 0, green = 0, alpha = 0.5))

barplot(zstan, ylim = c(0.0, 0.05), add = T, col = rgb(red = 0, blue = 0, green = 1, alpha = 0.5))

## Actual comparison
barplot(y, col = rgb(red = 1, blue = 0, green = 0, alpha = 0.5), main = "Actual Daily License Sales Data")
barplot(x, add = T, col = rgb(red = 0, blue = 1, green = 0, alpha = 0.5))

barplot(z, col = rgb(red = 1, blue = 0, green = 0, alpha = 0.5), main = "Actual Daily License Sales Data")

sum(xstan[1:37])
sum(ystan[1:37])

sum(xstan[41:61])
sum(ystan[41:61])

sum(x[1:37])
sum(y[1:37])

sum(x[41:61])
sum(y[41:61])

mean(x)
mean(y)

mean(x[1:37])
mean(x[41:61])

mean(y[1:37])
mean(y[41:61])







################################################################################################################

zips1 <- license$ZipCode

zips1 <- as.numeric(zips1)

uszipcode <- us_zipcodes()

vamap <- us_states(states = c("Virginia"))
plot(st_geometry(vamap))

cexVal = 0.2
index1 = 0

zips1 <- zips1[-1273]
zips1 <- zips1[-1519]

for(i in 1:length(zips1)) {
  if (length(which(uszipcode$zipcode == zips1[i])) != 0) {
    index1[i] <- which(uszipcode$zipcode == zips1[i])
    plot(st_geometry(uszipcode[index1[i],]), add = T, pch = 16, cex = cexVal)
  }
}

index <- as.data.frame(table(index1))
index$index1 <- as.numeric(levels(index$index1)[index$index1])

vamap <- us_states(states = c("Virginia"))
plot(st_geometry(vamap))

for(j in 1:length(index$index1)) {
  plot(st_geometry(uszipcode[index$index1[j],]), add = T, pch = 16, cex = 0.5,
       col = rgb(((255/835)*index$Freq)/255-0.000001, 0,0))#1.0-(((255/835)*index$Freq)/255-0.001), 0))
}



##############################################################################################################

reference <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/2019.03.15 refangler_datarequest.csv", header = T)

refx <- license[license$License_Sales_Detail_ID %in% reference$DGIFCustomerID,]






################################################################################################################
rm(list=ls())
## Licenses for FY 2019

head(license)

## Divides Licenses between sold and returned
return <- subset(license, license$Sold_Number == -1)
license <- subset(license, license$Sold_Number == 1)

## Return the specified age range
license <- subset(license, license$Age >= 18)
license <- subset(license, license$Age < 100)
license <- subset(license, license$FY == "2018")

summary(license$Age)

## Counts of all values in a table
table(license$Age)
table(return$License)
table(license$License)

## Divides licenses sold by type of license
lic101 <- subset(license, license$License == "101" & license$Sold_Number == 1)
lic103 <- subset(license, license$License == "103" & license$Sold_Number == 1)
lic111 <- subset(license, license$License == "111" & license$Sold_Number == 1)
lic113 <- subset(license, license$License == "113" & license$Sold_Number == 1)
lic137 <- subset(license, license$License == "137" & license$Sold_Number == 1)

lic335 <- subset(license, license$License == "335" & license$Sold_Number == 1)
lic353 <- subset(license, license$License == "353" & license$Sold_Number == 1)
lic357 <- subset(license, license$License == "357" & license$Sold_Number == 1)
lic359 <- subset(license, license$License == "359" & license$Sold_Number == 1)
lic701 <- subset(license, license$License == "701" & license$Sold_Number == 1)

licF44 <- subset(license, license$License == "FR44" & license$Sold_Number == 1)
licF50 <- subset(license, license$License == "FR50" & license$Sold_Number == 1)
licF55 <- subset(license, license$License == "FR55" & license$Sold_Number == 1)
licF60 <- subset(license, license$License == "FR60" & license$Sold_Number == 1)
licF64 <- subset(license, license$License == "FR64" & license$Sold_Number == 1)
licF65 <- subset(license, license$License == "FR65" & license$Sold_Number == 1)

licH44 <- subset(license, license$License == "HR44" & license$Sold_Number == 1)
licH50 <- subset(license, license$License == "HR50" & license$Sold_Number == 1)
licH55 <- subset(license, license$License == "HR55" & license$Sold_Number == 1)
licH60 <- subset(license, license$License == "HR60" & license$Sold_Number == 1)
licH64 <- subset(license, license$License == "HR64" & license$Sold_Number == 1)
licH65 <- subset(license, license$License == "HR65" & license$Sold_Number == 1)


## Returns the unique email addresses
email101 <- unique(lic101$EmailAddress)
email103 <- unique(lic103$EmailAddress)
email111 <- unique(lic111$EmailAddress)
email113 <- unique(lic113$EmailAddress)
email137 <- unique(lic137$EmailAddress)

email335 <- unique(lic335$EmailAddress)
email353 <- unique(lic353$EmailAddress)
email357 <- unique(lic357$EmailAddress)
email359 <- unique(lic359$EmailAddress)
email701 <- unique(lic701$EmailAddress)

emailF44 <- unique(licF44$EmailAddress)
emailF50 <- unique(licF50$EmailAddress)
emailF55 <- unique(licF55$EmailAddress)
emailF60 <- unique(licF60$EmailAddress)
emailF64 <- unique(licF64$EmailAddress)
emailF65 <- unique(licF65$EmailAddress)

emailH44 <- unique(licH44$EmailAddress)
emailH50 <- unique(licH50$EmailAddress)
emailH55 <- unique(licH55$EmailAddress)
emailH60 <- unique(licH60$EmailAddress)
emailH64 <- unique(licH64$EmailAddress)
emailH65 <- unique(licH65$EmailAddress)





## Used to remove outlier ages
table(lic113$Age)
which(lic113$Age == 94)
lic335 <- lic335[-9566,]
lic335 <- subset(lic335, lic335$Age > 64)

## Summarizes the Ages into quartiles
summary(lic101$Age)
summary(lic103$Age)
summary(lic111$Age)
summary(lic113$Age)
summary(lic137$Age)
summary(lic335$Age)
summary(lic353$Age)
summary(lic357$Age)
summary(lic359$Age)
summary(lic701$Age)

## remove outlier ages
table(licH64$Age)
which(licH50$Age == 53)
licH50 <- licH50[-90,]

## Summarizes the Ages
summary(licF44$Age)
summary(licF50$Age)
summary(licF55$Age)
summary(licF60$Age)
summary(licF64$Age)
summary(licF65$Age)
summary(licH44$Age)
summary(licH50$Age)
summary(licH55$Age)
summary(licH60$Age)
summary(licH64$Age)
summary(licH65$Age)

## Find emails with NA values
nullEmail <- is.na(email101)

## Count how many there are
count = 0
for(i in 1:length(nullEmail)) {
  if(nullEmail[i] == TRUE) {
    count <- count + 1
  }
}







sold <- data.frame(group = c("101","103","111","113","137","335","353","357","359","701","FR44","FR50","FR55","FR60","FR64","FR65","HR44","HR50","HR55","HR60","HR64","HR65"),
                    value = c(13827,2351,5465,129995,7154,14056,212472,5472,3476,46608,475,304,554,1044,1849,7595,238,160,182,363,704,2777))

ggplot(sold, aes(sold$group, sold$value)) + geom_bar(stat="identity") +
  geom_text(aes(label=value), vjust=-.25) +
  labs(x = "License Type", y = "Count", title = "License Sales: Fiscal Year 2019")




ggplot(license, aes(license$License)) + geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-.25) +
  labs(x = "License Type", y = "Count", title = "License Sales: Fiscal Year 2019")

ggplot(return, aes(return$License)) + geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-.25) +
  labs(x = "License Type", y = "Count", title = "License Sales: Fiscal Year 2019")



emails <- unique(license$EmailAddress)


nullEmail <- is.na(license$EmailAddress)

count = 0
for(i in 1:length(nullEmail)) {
  if(nullEmail[i] == TRUE) {
    count <- count + 1
  }
}

## Find emails which don't include the @ symbol
invalid <- grepl("@", emails)

count = 0

for(i in 1:length(invalid)) {
  if(invalid[i] == FALSE) {
    count <- count + 1
    emails <- emails[-i]
  }
}

## Write valid email to csv file
write.csv(emails, file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails_LSales_FY2019.csv")


licenseCheck <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails_LSales_FY2019.csv", header = T)
licenseCheck <- licenseCheck[,-1]
licenseCheck <- as.character(licenseCheck)

invalidCheck <- grepl("@", licenseCheck)

count = 0
for(i in 1:length(invalidCheck)) {
  if(invalidCheck[i] == FALSE) {
    count <- count + 1
    emails <- emails[-i]
    print(i)
  }
}


#################################################################################################################################

## Divide licenses by fiscal year
license16 <- subset(license, license$FY == '2016')
license17 <- subset(license, license$FY == '2017')
license18 <- subset(license, license$FY == '2018')
license19 <- subset(license, license$FY == '2019')


## How many licenses purchased four years in a row
mostRecent <- license17[license17$CustomerID %in% license16$CustomerID,]

mostRecent1 <- license18[license18$CustomerID %in% mostRecent$CustomerID,]

mostRecent2 <- license19[license19$CustomerID %in% mostRecent1$CustomerID,]

## How many licenses purchased three years in a row
mostRecent3 <- license18[license18$CustomerID %in% license17$CustomerID,]

mostRecent4 <- license19[license19$CustomerID %in% mostRecent3$CustomerID,]

## How many licenses purchased two years in a row
mostRecent5 <- license19[license19$CustomerID %in% license18$CustomerID,]


unique(mostRecent2$CustomerID)



y18n19 <- license18[!(license18$CustomerID %in% license19$CustomerID),]

y17n19 <- license17[!(license17$CustomerID %in% license19$CustomerID),]

y16n19 <- license16[!(license16$CustomerID %in% license19$CustomerID),]
y16y19 <- license16[(license16$CustomerID %in% license19$CustomerID),]


y18n19 <- subset(y18n19, !is.na(y18n19$EmailAddress))
table(y18n19$License)

y18n19_101 <- subset(y18n19, y18n19$License == "101")
y18n19_103 <- subset(y18n19, y18n19$License == "103")
y18n19_111 <- subset(y18n19, y18n19$License == "111")
y18n19_113 <- subset(y18n19, y18n19$License == "113")
y18n19_137 <- subset(y18n19, y18n19$License == "137")
y18n19_335 <- subset(y18n19, y18n19$License == "335")
y18n19_353 <- subset(y18n19, y18n19$License == "353")
y18n19_357 <- subset(y18n19, y18n19$License == "357")
y18n19_359 <- subset(y18n19, y18n19$License == "359")
y18n19_701 <- subset(y18n19, y18n19$License == "701")

y18n19_101_email <- unique(y18n19_101$EmailAddress)
y18n19_103_email <- unique(y18n19_103$EmailAddress)
y18n19_111_email <- unique(y18n19_111$EmailAddress)
y18n19_113_email <- unique(y18n19_113$EmailAddress)
y18n19_137_email <- unique(y18n19_137$EmailAddress)
y18n19_335_email <- unique(y18n19_335$EmailAddress)
y18n19_353_email <- unique(y18n19_353$EmailAddress)
y18n19_357_email <- unique(y18n19_357$EmailAddress)
y18n19_359_email <- unique(y18n19_359$EmailAddress)
y18n19_701_email <- unique(y18n19_701$EmailAddress)

c(length(y18n19_101_email),length(y18n19_103_email),length(y18n19_111_email),length(y18n19_113_email),
  length(y18n19_137_email),length(y18n19_335_email),length(y18n19_353_email),length(y18n19_357_email),
  length(y18n19_359_email),length(y18n19_701_email))

y18n19_hunting <- c(y18n19_101_email,y18n19_103_email,y18n19_111_email,y18n19_113_email,y18n19_137_email)
y18n19_fishing <- c(y18n19_101_email,y18n19_335_email,y18n19_353_email,y18n19_357_email,y18n19_359_email,y18n19_701_email)

y18n19_hunting <- unique(y18n19_hunting)
y18n19_fishing <- unique(y18n19_fishing)



y17n19 <- subset(y17n19, !is.na(y17n19$EmailAddress))
table(y17n19$License)

y17n19_101 <- subset(y17n19, y17n19$License == "101")
y17n19_103 <- subset(y17n19, y17n19$License == "103")
y17n19_111 <- subset(y17n19, y17n19$License == "111")
y17n19_113 <- subset(y17n19, y17n19$License == "113")
y17n19_137 <- subset(y17n19, y17n19$License == "137")
y17n19_335 <- subset(y17n19, y17n19$License == "335")
y17n19_353 <- subset(y17n19, y17n19$License == "353")
y17n19_357 <- subset(y17n19, y17n19$License == "357")
y17n19_359 <- subset(y17n19, y17n19$License == "359")
y17n19_701 <- subset(y17n19, y17n19$License == "701")

y17n19_101_email <- unique(y17n19_101$EmailAddress)
y17n19_103_email <- unique(y17n19_103$EmailAddress)
y17n19_111_email <- unique(y17n19_111$EmailAddress)
y17n19_113_email <- unique(y17n19_113$EmailAddress)
y17n19_137_email <- unique(y17n19_137$EmailAddress)
y17n19_335_email <- unique(y17n19_335$EmailAddress)
y17n19_357_email <- unique(y17n19_357$EmailAddress)
y17n19_353_email <- unique(y17n19_353$EmailAddress)
y17n19_359_email <- unique(y17n19_359$EmailAddress)
y17n19_701_email <- unique(y17n19_701$EmailAddress)

c(length(y17n19_101_email),length(y17n19_103_email),length(y17n19_111_email),length(y17n19_113_email),
  length(y17n19_137_email),length(y17n19_335_email),length(y17n19_353_email),length(y17n19_357_email),
  length(y17n19_359_email),length(y17n19_701_email))

y17n19_hunting <- c(y17n19_101_email,y17n19_103_email,y17n19_111_email,y17n19_113_email,y17n19_137_email)
y17n19_fishing <- c(y17n19_101_email,y17n19_335_email,y17n19_353_email,y17n19_357_email,y17n19_359_email,y17n19_701_email)

y17n19_hunting <- unique(y17n19_hunting)
y17n19_fishing <- unique(y17n19_fishing)



y16n19 <- subset(y16n19, !is.na(y16n19$EmailAddress))
table(y16n19$License)

y16n19_101 <- subset(y16n19, y16n19$License == "101")
y16n19_103 <- subset(y16n19, y16n19$License == "103")
y16n19_111 <- subset(y16n19, y16n19$License == "111")
y16n19_113 <- subset(y16n19, y16n19$License == "113")
y16n19_137 <- subset(y16n19, y16n19$License == "137")
y16n19_335 <- subset(y16n19, y16n19$License == "335")
y16n19_353 <- subset(y16n19, y16n19$License == "353")
y16n19_357 <- subset(y16n19, y16n19$License == "357")
y16n19_359 <- subset(y16n19, y16n19$License == "359")
y16n19_701 <- subset(y16n19, y16n19$License == "701")

y16n19_101_email <- unique(y16n19_101$EmailAddress)
y16n19_103_email <- unique(y16n19_103$EmailAddress)
y16n19_111_email <- unique(y16n19_111$EmailAddress)
y16n19_113_email <- unique(y16n19_113$EmailAddress)
y16n19_137_email <- unique(y16n19_137$EmailAddress)
y16n19_335_email <- unique(y16n19_335$EmailAddress)
y16n19_353_email <- unique(y16n19_353$EmailAddress)
y16n19_357_email <- unique(y16n19_357$EmailAddress)
y16n19_359_email <- unique(y16n19_359$EmailAddress)
y16n19_701_email <- unique(y16n19_701$EmailAddress)

c(length(y16n19_101_email),length(y16n19_103_email),length(y16n19_111_email),length(y16n19_113_email),
  length(y16n19_137_email),length(y16n19_335_email),length(y16n19_353_email),length(y16n19_357_email),
  length(y16n19_359_email),length(y16n19_701_email))

y16n19_hunting <- c(y16n19_101_email,y16n19_103_email,y16n19_111_email,y16n19_113_email,y16n19_137_email)
y16n19_fishing <- c(y16n19_101_email,y16n19_335_email,y16n19_353_email,y16n19_357_email,y16n19_359_email,y16n19_701_email)

y16n19_hunting <- unique(y16n19_hunting)
y16n19_fishing <- unique(y16n19_fishing)


write.csv(y16n19_hunting, file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/y16n19_hunting.csv")
write.csv(y16n19_fishing, file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/y16n19_fishing.csv")

write.csv(y17n19_hunting, file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/y17n19_hunting.csv")
write.csv(y17n19_fishing, file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/y17n19_fishing.csv")

write.csv(y18n19_hunting, file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/y18n19_hunting.csv")
write.csv(y18n19_fishing, file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/y18n19_fishing.csv")

#################################################################################################################################
rm(list=ls())

y16n19_fishing <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/y16n19_fishing.csv", header = F)
y16n19_hunting <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/y16n19_hunting.csv", header = F)
y16n19_sports <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/y16n19_sports.csv", header = F)

y17n19_fishing <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/y17n19_fishing.csv", header = F)
y17n19_hunting <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/y17n19_hunting.csv", header = F)
y17n19_sports <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/y17n19_sports.csv", header = F)

y18n19_fishing <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/y18n19_fishing.csv", header = F)
y18n19_hunting <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/y18n19_hunting.csv", header = F)
y18n19_sports <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/y18n19_sports.csv", header = F)

all_fishing <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/all_fishing.csv", header = F)
all_hunting <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/all_hunting.csv", header = F)
all_sports <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Emails/all_sports.csv", header = F)


all_both <- all_fishing[all_fishing$V1 %in% all_hunting$V1,]

all_both1 <- all_fishing[all_fishing$V1 %in% all_sports$V1,]

all_both2 <- all_hunting[all_hunting$V1 %in% all_sports$V1,]

#########################################################################################################################################



