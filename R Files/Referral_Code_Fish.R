rm(list=ls())

#### Used in Conjunction with Referral_Code_Hunt.R

library(ggplot2)
# library for maps; download this first
library(USAboundaries)
# after running first package, it will prompt you to download this one
library(USAboundariesData)
# special features library, needed for two above packages
library(sf)
# for haversine function
library(pracma)
# for the interactive heat map
library(plotly)
library(leaflet)
library(leaflet.extras)


## Read in Data Files.
gen <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/refangler_gen_code.csv", header = T, nrows = 1172)
red <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/refangler_red_code.csv", header = T, nrows = 430)

# gen is Code Generators
# red is Code Redeemers



#### Current age barplots ####

## ggplot versions
ggplot(gen, aes(gen$currentAge)) + geom_bar() + labs(x = "Current Age", y = "Count", title = "Age of People Who Generated a Code") +
  scale_x_continuous(breaks=seq(0,82,10))

ggplot(red, aes(red$currentAge)) + geom_bar() + lims(y = c(0,47)) + labs(x = "Current Age", y = "Count", title = "Age of People Who Redeemed a Code") +
  scale_x_continuous(breaks=seq(0,82,10))

## base versions allow for overlaying
gentab <- table(factor(gen$currentAge, levels = min(gen$currentAge):max(gen$currentAge)))
redtab <- table(factor(red$currentAge, levels = min(red$currentAge):max(red$currentAge)))

tabs <- as.data.frame(cbind(gentab, redtab))
tabs[,3] <- 18:81

barplot(gentab, xlim = c(0,81), ylim = c(0,50), col = rgb(red = 0, blue = 1, green = 0, alpha = 0.5), main = "Age of Participants")
barplot(redtab, xlim = c(0,82), ylim = c(0,50), add = T, col = rgb(red = 1, blue = 0, green = 0, alpha = 0.5))
legend("topright", inset = 0.05, legend = c("Generated Code", "Redeemed Code"), fill = c("blue","red"), cex = 0.5)

## Age Summaries
summary(gen$currentAge)
summary(red$currentAge)


#### Ethnicity ####

ggplot(gen, aes(gen$Ethnicity)) + geom_bar() + labs(x = "Ethnicity", y = "Count", title = "Ethnicity of People Who Generated a Code") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15)) +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25)

ggplot(red, aes(red$Ethnicity)) + geom_bar() + lims(y = c(0,937)) + labs(x = "Ethnicity", y = "Count", title = "Ethnicity of People Who Redeemed a Code") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15)) +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25)



#### Gender ####

ggplot(gen, aes(gen$Gender)) + geom_bar() + labs(x = "Gender", y = "Count", title = "Gender of People Who Generated a Code") +
  theme(text = element_text(size = 15)) +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25)

ggplot(red, aes(red$Gender)) + geom_bar() + lims(y = c(0,934)) + labs(x = "Gender", y = "Count", title = "Gender of People Who Redeemed a Code") +
  theme(text = element_text(size = 15)) +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25)



#### Date ####

ggplot(red, aes(as.Date(red$Date.of.code.redemption.License.purchase, format = "%m/%d/%Y"))) + geom_bar() +
      labs(x = "Date Redeemed", y = "Count", title = "Date of Code Redemption")# + lims(x = c("2018-04-01", "2018-10-01"))

# ^^ 6/12/18 - 6/16/18 highest amount

## Date of all licenses purchased by each group
genDate <- license[license$CustomerID %in% gen$DGIFCustomerID,]

redDate <- license[license$CustomerID %in% red$DGIFCustomerID,]

ggplot(genDate, aes(as.Date(genDate$Sold_Date))) + geom_bar()

ggplot(redDate, aes(as.Date(redDate$Sold_Date))) + geom_bar()



#### 2 Years Before ####

before2gen <- license[license$CustomerID %in% gen$DGIFCustomerID,]

before2red <- license[license$CustomerID %in% red$DGIFCustomerID,]

unique(before2red$CustomerID)

table(before2red$License)



#### 2016 Fishing Licenses ####

custIDgen16 <- license[license$CustomerID %in% gen$DGIFCustomerID,] # 1038, 425 unique

custIDred16 <- license[license$CustomerID %in% red$DGIFCustomerID,] # 9, 5 unique

custIDgen16 <- table(custIDgen16$License)
custIDgen16 <- subset(custIDgen16, custIDgen16 > 25)

custIDgen16 <- as.data.frame(custIDgen16)

## licenses > 25 bought
ggplot(custIDgen16, aes(custIDgen16$Var1, custIDgen16$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Top Ten Licenses Bought by Code Generators", subtitle = "Fiscal Year 2016") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))


ggplot(custIDgen16, aes(custIDgen16)) + geom_bar() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Generators", subtitle = "Fiscal Year 2016") +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))

ggplot(custIDred16, aes(custIDred16$License)) + geom_bar() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Redeemers", subtitle = "Fiscal Year 2016") +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))



### 2017 Fishing Licenses ####

custIDgen17 <- license[license$CustomerID %in% gen$DGIFCustomerID,] # 1359, 573 unique

custIDred17 <- license[license$CustomerID %in% red$DGIFCustomerID,] # 12, 8 unique


custIDgen17 <- table(custIDgen17$License)
custIDgen17 <- subset(custIDgen17, custIDgen17 > 24)

custIDgen17 <- as.data.frame(custIDgen17)

## licenses > 24 bought
ggplot(custIDgen17, aes(custIDgen17$Var1, custIDgen17$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Top Ten Licenses Bought by Code Generators", subtitle = "Fiscal Year 2017") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))


ggplot(custIDgen17, aes(custIDgen17$License)) + geom_bar() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Generators", subtitle = "Fiscal Year 2017") +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))

ggplot(custIDred17, aes(custIDred17$License)) + geom_bar() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Redeemers", subtitle = "Fiscal Year 2017") +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))



### 2018 Fishing Licenses ####

custIDgen18 <- license[license$CustomerID %in% gen$DGIFCustomerID,] # 2232, 956 unique

custIDred18 <- license[license$CustomerID %in% red$DGIFCustomerID,] # 284, 234 unique


custIDgen18 <- table(custIDgen18$License)
custIDgen18 <- subset(custIDgen18, custIDgen18 > 40)

custIDgen18 <- as.data.frame(custIDgen18)

## licenses > 40 bought
ggplot(custIDgen18, aes(custIDgen18$Var1, custIDgen18$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Top Ten Licenses Bought by Code Generators", subtitle = "Fiscal Year 2018") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))


ggplot(custIDgen18, aes(custIDgen18$License)) + geom_bar() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Generators", subtitle = "Fiscal Year 2018") +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + lims(y = c(0,582))

ggplot(custIDred18, aes(custIDred18$License)) + geom_bar() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Redeemers", subtitle = "Fiscal Year 2018") +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))




### Licenses Sold Before Event ####

custIDgenBefore <- license[license$CustomerID %in% gen$DGIFCustomerID,] # 1329, 695 unique

custIDredBefore <- license[license$CustomerID %in% red$DGIFCustomerID,] # 8, 6 unique


custIDgenBefore <- table(custIDgenBefore$License)
custIDgenBefore <- subset(custIDgenBefore, custIDgenBefore > 43)

custIDgenBefore <- as.data.frame(custIDgenBefore)


custIDredBefore <- table(custIDredBefore$License)
custIDredBefore <- subset(custIDredBefore, custIDredBefore > 40)

custIDredBefore <- as.data.frame(custIDredBefore)

## licenses > 9 bought
ggplot(custIDgenBefore, aes(custIDgenBefore$Var1, custIDgenBefore$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Generators Before", subtitle = "Top Twelve Between 07/01/2015 and 04/11/2018") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))

ggplot(custIDredBefore, aes(custIDredBefore$Var1, custIDredBefore$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Redeemers Before", subtitle = "Top Ten Between 07/01/2015 and 04/11/2018") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))



ggplot(custIDgenBefore, aes(custIDgenBefore$License)) + geom_bar() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Generators Before", subtitle = "Between 07/01/2015 and 04/01/2018") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))

ggplot(custIDredBefore, aes(custIDredBefore$License)) + geom_bar() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Redeemers Before Event", subtitle = "Between 04/01/2016 and 04/01/2018") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))



### Licenses Sold During/After Event ####

custIDgenAfter <- license[license$CustomerID %in% gen$DGIFCustomerID,] # 1035, 812 unique

custIDredAfter <- license[license$CustomerID %in% red$DGIFCustomerID,] # 324, 317 unique


custIDgenAfter <- table(custIDgenAfter$License)
custIDgenAfter <- subset(custIDgenAfter, custIDgenAfter > 50)

custIDgenAfter <- as.data.frame(custIDgenAfter)


custIDredAfter <- table(custIDredAfter$License)
custIDredAfter <- subset(custIDredAfter, custIDredAfter > 10)

custIDredAfter <- as.data.frame(custIDredAfter)


## licenses > 9 bought
ggplot(custIDgenAfter, aes(custIDgenAfter$Var1, custIDgenAfter$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Generators After", subtitle = "Top Ten Between 04/12/2018 and 04/12/2019") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))

ggplot(custIDredAfter, aes(custIDredAfter$Var1, custIDredAfter$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Redeemers After", subtitle = "Top Ten Between 04/12/2018 and 04/12/2019") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))



ggplot(custIDgenAfter, aes(custIDgenAfter$License)) + geom_bar() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Generators After", subtitle = "Between 04/12/2018 and 04/12/2019") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))

ggplot(custIDredAfter, aes(factor(custIDredAfter$License, levels = c("101","111","113","119","129","161","335","353","355","357","701")))) + geom_bar() +
  labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Redeemers After", subtitle = "Between 04/12/2018 and 04/12/2019") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)



#### Licenses Sold the Following Year ####

custIDgenFollow <- license[license$CustomerID %in% gen$DGIFCustomerID,] # 672, 407 unique

custIDredFollow <- license[license$CustomerID %in% red$DGIFCustomerID,] # 84, 70 unique

table(custIDredFollow$License)




### Combine CustomerIDofCodeGenerator and DGIFCustomerID. Doesn't work. Both IDs are the same. ####

allRef <- merge(red, gen, by.x = "Customer.ID.of.code.generator", by.y = "DGIFCustomerID")

write.csv(allRef, file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Ref Codes/mergedData.csv")






### Heat Map Generated Zip Codes - Doesn't work very well ####

uszipcode <- us_zipcodes()

zips <- gen$Zipcode

zips <- as.data.frame(table(zips))

vamap <- us_states(states = c("Virginia", "North Carolina", "Maryland", "West Virginia", "Pennsylvania",
                              "South Carolina", "Ohio", "Georgia"))
plot(st_geometry(vamap), main = "")

cexVal = 0.5
index = 0
#coords <- 0
coords <- matrix(c(0,0), ncol = 2, nrow = 1163)

for(i in 1:length(zips$zips)) {
  cexVal = 0.5
  if (length(which(uszipcode$zipcode == zips$zips[i])) > 0) {
    index[i] <- which(uszipcode$zipcode == zips$zips[i])
    plot(st_geometry(uszipcode[index[i],]), add = T, pch = 16, cex = cexVal)
  }
}

index <- na.omit(index)

zips <- zips[-c(111, 273, 383, 454, 488, 501, 514, 517),]

for(i in 1:length(index)) {
  coords[i] <- st_geometry(uszipcode[index[i],])
}

coords <- matrix(unlist(coords), ncol = 2, byrow = T)

l = 1
for(j in 1:512) {
  if (zips$Freq[j] > 1) {
    for (k in 1:zips$Freq[j]) {
      incr <- runif(2*zips$Freq[j], min = 0.001, max = 0.01)
      coords[(512)+l,1] <- coords[j,1] + incr[k]
      coords[(512)+l,2] <- coords[j,2] + incr[2*zips$Freq[j]-(k-1)]
      l <- l+1
    }
  }
  
}

coords %>% leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addHeatmap(lng = as.numeric(coords[,1]), lat = as.numeric(coords[,2]), radius = 8)



### Heat Map Redeemed Zip Codes - Doesn't work very well ####


zips1 <- red$Zip


zips1 <- as.data.frame(table(zips1))

vamap <- us_states(states = c("Virginia", "North Carolina", "Maryland", "West Virginia", "Pennsylvania",
                              "South Carolina", "Ohio", "Georgia"))
plot(st_geometry(vamap), main = "")

cexVal = 0.5
index1 = 0
#coords <- 0
coords1 <- matrix(c(0,0), ncol = 2, nrow = 430)

for(i in 1:length(zips1$zips1)) {
  cexVal = 0.5
  if (length(which(uszipcode$zipcode == zips1$zips1[i])) > 0) {
    index1[i] <- which(uszipcode$zipcode == zips1$zips[i])
    plot(st_geometry(uszipcode[index1[i],]), add = T, pch = 16, cex = cexVal)
  }
}

index1 <- na.omit(index1)

zips1 <- zips1[-c(209, 237, 256, 266, 273),]

for(i in 1:length(index1)) {
  coords1[i] <- st_geometry(uszipcode[index1[i],])
}

coords1 <- matrix(unlist(coords1), ncol = 2, byrow = T)

l = 1
for(j in 1:277) {
  if (zips1$Freq[j] > 1) {
    for (k in 1:zips1$Freq[j]) {
      incr1 <- runif(2*zips1$Freq[j], min = 0.001, max = 0.01)
      coords1[(277)+l,1] <- coords1[j,1] + incr1[k]
      coords1[(277)+l,2] <- coords1[j,2] + incr1[2*zips1$Freq[j]-(k-1)]
      l <- l+1
    }
  }
  
}

coords1 %>% leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addHeatmap(lng = as.numeric(coords1[,1]), lat = as.numeric(coords1[,2]), radius = 8)

##############################################################################################################################33

#### Single vs. Multiple licenses purchased ####

## Multiple licenses purchased
multi <- custIDredAfter[duplicated(custIDredAfter[,'CustomerID']),]
multi <- table(multi$License)

## Single license purchased
single <- custIDredAfter[!duplicated(custIDredAfter[,'CustomerID']),]
single <- table(single$License)

## Get top 10
multi <- subset(multi, multi > 3)
multi <- as.data.frame(multi)

## Get top 10
single <- subset(single, single > 8)
single <- as.data.frame(single)

## unused
ggplot(single, aes(single$License)) + geom_bar() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Generators After", subtitle = "Top Ten Between 04/12/2018 and 04/12/2019") +
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))

## Plot top ten licenses for each category
ggplot(multi, aes(multi$Var1, multi$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Redeemers After", subtitle = "Top Ten For People Buying Multiple") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))

ggplot(single, aes(single$Var1, single$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Redeemers After", subtitle = "Top Ten For People Buying One") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))



#### Percent of people having each license type ####

## Filter out returned licenses
bought <- subset(license, license$Sold_Number == 1)

## licenses bought by code generators
boughtGen <- bought[bought$CustomerID %in% gen$DGIFCustomerID,]

## number of rows for both groups
nBoughtGen <- nrow(boughtGen)
nBought <- nrow(bought)



### 101

## collect number for each license type for both groups
perGen101 <- nrow(subset(boughtGen, boughtGen$License == "101"))
per101 <- nrow(subset(bought, bought$License == "101"))

## build numerical vectors for t-test
x101 <- 0
x101[1:nBoughtGen] <- 0
x101[1:perGen101] <- 1

y101 <- 0
y101[1:nBought] <- 0
y101[1:per101] <- 1

## unused except for comparison (does show group percentages)
t.test(x101, y101, alternative = "two.sided", var.equal = F)

## build 2x2 grid
test <- matrix(data = c(62, 6539, 46612, 4602377), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)
test

## Run Chi-square test. P-value displayed in comment to right
chisq.test(test) ## .6493


## Repeat for each license type


## 113
perGen113 <- nrow(subset(boughtGen, boughtGen$License == "113"))
per113 <- nrow(subset(bought, bought$License == "113"))

x113 <- 0
x113[1:nBoughtGen] <- 0
x113[1:perGen113] <- 1

y113 <- 0
y113[1:nBought] <- 0
y113[1:per113] <- 1

t.test(x113, y113, alternative = "two.sided", var.equal = F)

test <- matrix(data = c(648, 5953, 540145, 4108844), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)

chisq.test(test) ## 0


## 161
perGen161 <- nrow(subset(boughtGen, boughtGen$License == "161"))
per161 <- nrow(subset(bought, bought$License == "161"))

x161 <- 0
x161[1:nBoughtGen] <- 0
x161[1:perGen161] <- 1

y161 <- 0
y161[1:nBought] <- 0
y161[1:per161] <- 1

t.test(x161, y161, alternative = "two.sided", var.equal = F)

test <- matrix(data = c(650, 5951, 624718, 4024271), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)

chisq.test(test) ## 0



## 353
perGen353 <- nrow(subset(boughtGen, boughtGen$License == "353"))
per353 <- nrow(subset(bought, bought$License == "353"))

x353 <- 0
x353[1:nBoughtGen] <- 0
x353[1:perGen353] <- 1

y353 <- 0
y353[1:nBought] <- 0
y353[1:per353] <- 1

t.test(x353, y353, alternative = "two.sided", var.equal = F)

test <- matrix(data = c(1539, 5062, 787003, 3861986), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)

chisq.test(test) ## 0


## 354
perGen354 <- nrow(subset(boughtGen, boughtGen$License == "354"))
per354 <- nrow(subset(bought, bought$License == "354"))

x354 <- 0
x354[1:nBoughtGen] <- 0
x354[1:perGen354] <- 1

y354 <- 0
y354[1:nBought] <- 0
y354[1:per354] <- 1

t.test(x354, y354, alternative = "two.sided", var.equal = F)

test <- matrix(data = c(86, 6515, 58396, 4590593), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)

chisq.test(test) ## 0.7754


## 355
perGen355 <- nrow(subset(boughtGen, boughtGen$License == "355"))
per355 <- nrow(subset(bought, bought$License == "355"))

x355 <- 0
x355[1:nBoughtGen] <- 0
x355[1:perGen355] <- 1

y355 <- 0
y355[1:nBought] <- 0
y355[1:per355] <- 1

t.test(x355, y355, alternative = "two.sided", var.equal = F)

test <- matrix(data = c(458, 6143, 189717, 4459282), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)

chisq.test(test) ## 0



## 357
perGen357 <- nrow(subset(boughtGen, boughtGen$License == "357"))
per357 <- nrow(subset(bought, bought$License == "357"))

x357 <- 0
x357[1:nBoughtGen] <- 0
x357[1:perGen357] <- 1

y357 <- 0
y357[1:nBought] <- 0
y357[1:per357] <- 1

t.test(x357, y357, alternative = "two.sided", var.equal = F)

test <- matrix(data = c(57, 6544, 21383, 4627606), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)

chisq.test(test) ## 0


## 401
perGen401 <- nrow(subset(boughtGen, boughtGen$License == "401"))
per401 <- nrow(subset(bought, bought$License == "401"))

x401 <- 0
x401[1:nBoughtGen] <- 0
x401[1:perGen401] <- 1

y401 <- 0
y401[1:nBought] <- 0
y401[1:per401] <- 1

t.test(x401, y401, alternative = "two.sided", var.equal = F)

test <- matrix(data = c(116, 6485, 72197, 4576792), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)

chisq.test(test) ## .1964


## 472
perGen472 <- nrow(subset(boughtGen, boughtGen$License == "472"))
per472 <- nrow(subset(bought, bought$License == "472"))

x472 <- 0
x472[1:nBoughtGen] <- 0
x472[1:perGen472] <- 1

y472 <- 0
y472[1:nBought] <- 0
y472[1:per472] <- 1

t.test(x472, y472, alternative = "two.sided", var.equal = F)

test <- matrix(data = c(551, 6050, 336862, 4312127), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)

chisq.test(test) ## .0006151


## 475
perGen475 <- nrow(subset(boughtGen, boughtGen$License == "475"))
per475 <- nrow(subset(bought, bought$License == "475"))

x475 <- 0
x475[1:nBoughtGen] <- 0
x475[1:perGen475] <- 1

y475 <- 0
y475[1:nBought] <- 0
y475[1:per475] <- 1

t.test(x475, y475, alternative = "two.sided", var.equal = F)

test <- matrix(data = c(128, 6473, 93925, 4555064), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)

chisq.test(test) ## .6708


## 701
perGen701 <- nrow(subset(boughtGen, boughtGen$License == "701"))
per701 <- nrow(subset(bought, bought$License == "701"))

x701 <- 0
x701[1:nBoughtGen] <- 0
x701[1:perGen701] <- 1

y701 <- 0
y701[1:nBought] <- 0
y701[1:per701] <- 1

t.test(x701, y701, alternative = "two.sided", var.equal = F)

test <- matrix(data = c(455, 6146, 154740, 4494249), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)

chisq.test(test) ## 0



#### Mean number of years each license is bought ####
rm(list=ls())

## filter out returned licenses
bought <- subset(license, license$Sold_Number == 1)

## collect licenses bought by the generators
boughtGen <- bought[bought$CustomerID %in% gen$DGIFCustomerID,]

## Sample total population that bought a license in the year before program began
boughtSamp <- subset(bought, bought$FY == "2018")
boughtSamp <- subset(boughtSamp, boughtSamp$License == "101" | boughtSamp$License == "353" | boughtSamp$License == "355" | boughtSamp$License == "357")
boughtSampID <- unique(boughtSamp$CustomerID)
## Sample same number of people as in the code generators group
boughtSampID <- boughtSampID[sample(length(boughtSampID),1172, replace = F)]
## collect all licenses purchased by sample group
boughtSamp <- bought[bought$CustomerID %in% boughtSampID,]

## remove columns which uniquely identify individual licenses.
## (we want only one of each type of license per year per customerID number)
boughtGen <- boughtGen[,-c(3,4,5,10)]
boughtSamp <- boughtSamp[,-c(3,4,5,10)]

## separate by year
boughtGen16 <- subset(boughtGen, boughtGen$FY == "2016")
boughtGen17 <- subset(boughtGen, boughtGen$FY == "2017")
boughtGen18 <- subset(boughtGen, boughtGen$FY == "2018")
boughtGen19 <- subset(boughtGen, boughtGen$FY == "2019")

bought16 <- subset(boughtSamp, boughtSamp$FY == "2016")
bought17 <- subset(boughtSamp, boughtSamp$FY == "2017")
bought18 <- subset(boughtSamp, boughtSamp$FY == "2018")
bought19 <- subset(boughtSamp, boughtSamp$FY == "2019")

## Number of each group
nBoughtGen <- nrow(boughtGen)
nBoughtGen16 <- nrow(boughtGen16)
nBoughtGen17 <- nrow(boughtGen17)
nBoughtGen18 <- nrow(boughtGen18)
nBoughtGen19 <- nrow(boughtGen19)
nBought <- nrow(bought)


### 101
## select only license type 101 for whole group
perGen101 <- subset(boughtGen, boughtGen$License == "101")
per101 <- subset(boughtSamp, boughtSamp$License == "101")

## select only license type 101 for individual years (we want only one entry per year per customerID)
boughtGen16_101 <- unique(subset(boughtGen16, boughtGen16$License == "101"))
boughtGen17_101 <- unique(subset(boughtGen17, boughtGen17$License == "101"))
boughtGen18_101 <- unique(subset(boughtGen18, boughtGen18$License == "101"))
boughtGen19_101 <- unique(subset(boughtGen19, boughtGen19$License == "101"))

## same as above for general population
bought16_101 <- unique(subset(bought16, bought16$License == "101"))
bought17_101 <- unique(subset(bought17, bought17$License == "101"))
bought18_101 <- unique(subset(bought18, bought18$License == "101"))
bought19_101 <- unique(subset(bought19, bought19$License == "101"))


### Generator group

## want only one entry per year per customerID
perGen101 <- unique(perGen101)
## initialize column representing number of years out of four each customerID purchased this license
perGen101[,7] <- 0
## Loop through entire list of code generators that purchased license type 101
for(i in 1:nrow(perGen101)) {
  ## Loop through list of code generators that purchased license 101 in 2016
  for(j in 1:nrow(boughtGen16_101)) {
    ## if the code generator bought license 101 in 2016, increment the counter.
    if(perGen101$CustomerID[i] == boughtGen16_101$CustomerID[j]) {
      perGen101[i,7] <- perGen101[i,7] + 1
    }
  }
  ## Same as above for 2017
  for(j in 1:nrow(boughtGen17_101)) {
    if(perGen101$CustomerID[i] == boughtGen17_101$CustomerID[j]) {
      perGen101[i,7] <- perGen101[i,7] + 1
    }
  }
  ## Same as above for 2018
  for(j in 1:nrow(boughtGen18_101)) {
    if(perGen101$CustomerID[i] == boughtGen18_101$CustomerID[j]) {
      perGen101[i,7] <- perGen101[i,7] + 1
    }
  }
  ## Same as above for 2019
  for(j in 1:nrow(boughtGen19_101)) {
    if(perGen101$CustomerID[i] == boughtGen19_101$CustomerID[j]) {
      perGen101[i,7] <- perGen101[i,7] + 1
    }
  }
}

#table(perGen101$V7)

testGen101 <- perGen101[,-1]
testGen101 <- unique(testGen101)

table(testGen101$V7)

## mean number of years the code generators purchased license 101
gen101 <- mean(testGen101$V7)



### General Population Sample group

## Same as the generator group above
per101 <- unique(per101)
per101[,7] <- 0

for(i in 1:nrow(per101)) {
  for(j in 1:nrow(bought16_101)) {
    if(per101$CustomerID[i] == bought16_101$CustomerID[j]) {
      per101[i,7] <- per101[i,7] + 1
    }
  }
  for(j in 1:nrow(bought17_101)) {
    if(per101$CustomerID[i] == bought17_101$CustomerID[j]) {
      per101[i,7] <- per101[i,7] + 1
    }
  }
  for(j in 1:nrow(bought18_101)) {
    if(per101$CustomerID[i] == bought18_101$CustomerID[j]) {
      per101[i,7] <- per101[i,7] + 1
    }
  }
  for(j in 1:nrow(bought19_101)) {
    if(per101$CustomerID[i] == bought19_101$CustomerID[j]) {
      per101[i,7] <- per101[i,7] + 1
    }
  }
}

test101 <- per101[,-1]
test101 <- unique(test101)

table(test101$V7)

all101 <- mean(test101$V7)

#test101$V7[60] <- 3.906328

## two sample t-test
t.test(testGen101$V7, test101$V7, alternative = "two.sided", var.equal = F)


### Repeat for all license types




## 113
perGen113 <- subset(boughtGen, boughtGen$License == "113")
per113 <- subset(boughtSamp, boughtSamp$License == "113")

boughtGen16_113 <- unique(subset(boughtGen16, boughtGen16$License == "113"))
boughtGen17_113 <- unique(subset(boughtGen17, boughtGen17$License == "113"))
boughtGen18_113 <- unique(subset(boughtGen18, boughtGen18$License == "113"))
boughtGen19_113 <- unique(subset(boughtGen19, boughtGen19$License == "113"))

bought16_113 <- unique(subset(bought16, bought16$License == "113"))
bought17_113 <- unique(subset(bought17, bought17$License == "113"))
bought18_113 <- unique(subset(bought18, bought18$License == "113"))
bought19_113 <- unique(subset(bought19, bought19$License == "113"))

### Gen
perGen113 <- unique(perGen113)
perGen113[,7] <- 0
for(i in 1:nrow(perGen113)) {
  for(j in 1:nrow(boughtGen16_113)) {
    if(perGen113$CustomerID[i] == boughtGen16_113$CustomerID[j]) {
      perGen113[i,7] <- perGen113[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen17_113)) {
    if(perGen113$CustomerID[i] == boughtGen17_113$CustomerID[j]) {
      perGen113[i,7] <- perGen113[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen18_113)) {
    if(perGen113$CustomerID[i] == boughtGen18_113$CustomerID[j]) {
      perGen113[i,7] <- perGen113[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen19_113)) {
    if(perGen113$CustomerID[i] == boughtGen19_113$CustomerID[j]) {
      perGen113[i,7] <- perGen113[i,7] + 1
    }
  }
}

table(perGen113$V7)

testGen113 <- perGen113[,-1]
testGen113 <- unique(testGen113)

table(testGen113$V7)

gen113 <- mean(testGen113$V7)



### All
per113 <- unique(per113)
per113[,7] <- 0

for(i in 1:nrow(per113)) {
  for(j in 1:nrow(bought16_113)) {
    if(per113$CustomerID[i] == bought16_113$CustomerID[j]) {
      per113[i,7] <- per113[i,7] + 1
    }
  }
  for(j in 1:nrow(bought17_113)) {
    if(per113$CustomerID[i] == bought17_113$CustomerID[j]) {
      per113[i,7] <- per113[i,7] + 1
    }
  }
  for(j in 1:nrow(bought18_113)) {
    if(per113$CustomerID[i] == bought18_113$CustomerID[j]) {
      per113[i,7] <- per113[i,7] + 1
    }
  }
  for(j in 1:nrow(bought19_113)) {
    if(per113$CustomerID[i] == bought19_113$CustomerID[j]) {
      per113[i,7] <- per113[i,7] + 1
    }
  }
}



test113 <- per113[,-1]
test113 <- unique(test113)

table(test113$V7)

all113 <- mean(test113$V7)

test113$V7[301] <- 3.33538

t.test(testGen113$V7, test113$V7, alternative = "two.sided", var.equal = F)



## 161
perGen161 <- subset(boughtGen, boughtGen$License == "161")
per161 <- subset(boughtSamp, boughtSamp$License == "161")

boughtGen16_161 <- unique(subset(boughtGen16, boughtGen16$License == "161"))
boughtGen17_161 <- unique(subset(boughtGen17, boughtGen17$License == "161"))
boughtGen18_161 <- unique(subset(boughtGen18, boughtGen18$License == "161"))
boughtGen19_161 <- unique(subset(boughtGen19, boughtGen19$License == "161"))

bought16_161 <- unique(subset(bought16, bought16$License == "161"))
bought17_161 <- unique(subset(bought17, bought17$License == "161"))
bought18_161 <- unique(subset(bought18, bought18$License == "161"))
bought19_161 <- unique(subset(bought19, bought19$License == "161"))

### Gen
perGen161 <- unique(perGen161)
perGen161[,7] <- 0
for(i in 1:nrow(perGen161)) {
  for(j in 1:nrow(boughtGen16_161)) {
    if(perGen161$CustomerID[i] == boughtGen16_161$CustomerID[j]) {
      perGen161[i,7] <- perGen161[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen17_161)) {
    if(perGen161$CustomerID[i] == boughtGen17_161$CustomerID[j]) {
      perGen161[i,7] <- perGen161[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen18_161)) {
    if(perGen161$CustomerID[i] == boughtGen18_161$CustomerID[j]) {
      perGen161[i,7] <- perGen161[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen19_161)) {
    if(perGen161$CustomerID[i] == boughtGen19_161$CustomerID[j]) {
      perGen161[i,7] <- perGen161[i,7] + 1
    }
  }
}

table(perGen161$V7)

testGen161 <- perGen161[,-1]
testGen161 <- unique(testGen161)

table(testGen161$V7)

gen161 <- mean(testGen161$V7)



### All
per161 <- unique(per161)
per161[,7] <- 0

for(i in 1:nrow(per161)) {
  for(j in 1:nrow(bought16_161)) {
    if(per161$CustomerID[i] == bought16_161$CustomerID[j]) {
      per161[i,7] <- per161[i,7] + 1
    }
  }
  for(j in 1:nrow(bought17_161)) {
    if(per161$CustomerID[i] == bought17_161$CustomerID[j]) {
      per161[i,7] <- per161[i,7] + 1
    }
  }
  for(j in 1:nrow(bought18_161)) {
    if(per161$CustomerID[i] == bought18_161$CustomerID[j]) {
      per161[i,7] <- per161[i,7] + 1
    }
  }
  for(j in 1:nrow(bought19_161)) {
    if(per161$CustomerID[i] == bought19_161$CustomerID[j]) {
      per161[i,7] <- per161[i,7] + 1
    }
  }
}

test161 <- per161[,-1]
test161 <- unique(test161)

table(test161$V7)

all161 <- mean(test161$V7)

test161$V7[300] <- 3.92034

t.test(testGen161$V7, test161$V7, alternative = "two.sided", var.equal = F)






## 353
perGen353 <- subset(boughtGen, boughtGen$License == "353")
per353 <- subset(boughtSamp, boughtSamp$License == "353")

boughtGen16_353 <- unique(subset(boughtGen16, boughtGen16$License == "353"))
boughtGen17_353 <- unique(subset(boughtGen17, boughtGen17$License == "353"))
boughtGen18_353 <- unique(subset(boughtGen18, boughtGen18$License == "353"))
boughtGen19_353 <- unique(subset(boughtGen19, boughtGen19$License == "353"))

bought16_353 <- unique(subset(bought16, bought16$License == "353"))
bought17_353 <- unique(subset(bought17, bought17$License == "353"))
bought18_353 <- unique(subset(bought18, bought18$License == "353"))
bought19_353 <- unique(subset(bought19, bought19$License == "353"))

### Gen
perGen353 <- unique(perGen353)
perGen353[,7] <- 0
for(i in 1:nrow(perGen353)) {
  for(j in 1:nrow(boughtGen16_353)) {
    if(perGen353$CustomerID[i] == boughtGen16_353$CustomerID[j]) {
      perGen353[i,7] <- perGen353[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen17_353)) {
    if(perGen353$CustomerID[i] == boughtGen17_353$CustomerID[j]) {
      perGen353[i,7] <- perGen353[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen18_353)) {
    if(perGen353$CustomerID[i] == boughtGen18_353$CustomerID[j]) {
      perGen353[i,7] <- perGen353[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen19_353)) {
    if(perGen353$CustomerID[i] == boughtGen19_353$CustomerID[j]) {
      perGen353[i,7] <- perGen353[i,7] + 1
    }
  }
}

table(perGen353$V7)

testGen353 <- perGen353[,-1]
testGen353 <- unique(testGen353)

table(testGen353$V7)

gen113 <- mean(testGen353$V7)



### All
per353 <- unique(per353)
per353[,7] <- 0

for(i in 1:nrow(per353)) {
  for(j in 1:nrow(bought16_353)) {
    if(per353$CustomerID[i] == bought16_353$CustomerID[j]) {
      per353[i,7] <- per353[i,7] + 1
    }
  }
  for(j in 1:nrow(bought17_353)) {
    if(per353$CustomerID[i] == bought17_353$CustomerID[j]) {
      per353[i,7] <- per353[i,7] + 1
    }
  }
  for(j in 1:nrow(bought18_353)) {
    if(per353$CustomerID[i] == bought18_353$CustomerID[j]) {
      per353[i,7] <- per353[i,7] + 1
    }
  }
  for(j in 1:nrow(bought19_353)) {
    if(per353$CustomerID[i] == bought19_353$CustomerID[j]) {
      per353[i,7] <- per353[i,7] + 1
    }
  }
}

test353 <- per353[,-1]
test353 <- unique(test353)

table(test353$V7)

all353 <- mean(test353$V7)

test353$V7[876] <- 3.06754

t.test(testGen353$V7, test353$V7, alternative = "two.sided", var.equal = F)




## 354
perGen354 <- subset(boughtGen, boughtGen$License == "354")
per354 <- subset(boughtSamp, boughtSamp$License == "354")

boughtGen16_354 <- unique(subset(boughtGen16, boughtGen16$License == "354"))
boughtGen17_354 <- unique(subset(boughtGen17, boughtGen17$License == "354"))
boughtGen18_354 <- unique(subset(boughtGen18, boughtGen18$License == "354"))
boughtGen19_354 <- unique(subset(boughtGen19, boughtGen19$License == "354"))

bought16_354 <- unique(subset(bought16, bought16$License == "354"))
bought17_354 <- unique(subset(bought17, bought17$License == "354"))
bought18_354 <- unique(subset(bought18, bought18$License == "354"))
bought19_354 <- unique(subset(bought19, bought19$License == "354"))

### Gen
perGen354 <- unique(perGen354)
perGen354[,7] <- 0
for(i in 1:nrow(perGen354)) {
  for(j in 1:nrow(boughtGen16_354)) {
    if(perGen354$CustomerID[i] == boughtGen16_354$CustomerID[j]) {
      perGen354[i,7] <- perGen354[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen17_354)) {
    if(perGen354$CustomerID[i] == boughtGen17_354$CustomerID[j]) {
      perGen354[i,7] <- perGen354[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen18_354)) {
    if(perGen354$CustomerID[i] == boughtGen18_354$CustomerID[j]) {
      perGen354[i,7] <- perGen354[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen19_354)) {
    if(perGen354$CustomerID[i] == boughtGen19_354$CustomerID[j]) {
      perGen354[i,7] <- perGen354[i,7] + 1
    }
  }
}

table(perGen354$V7)

testGen354 <- perGen354[,-1]
testGen354 <- unique(testGen354)

table(testGen354$V7)

gen113 <- mean(testGen354$V7)


### All
per354 <- unique(per354)
per354[,7] <- 0

for(i in 1:nrow(per354)) {
  for(j in 1:nrow(bought16_354)) {
    if(per354$CustomerID[i] == bought16_354$CustomerID[j]) {
      per354[i,7] <- per354[i,7] + 1
    }
  }
  for(j in 1:nrow(bought17_354)) {
    if(per354$CustomerID[i] == bought17_354$CustomerID[j]) {
      per354[i,7] <- per354[i,7] + 1
    }
  }
  for(j in 1:nrow(bought18_354)) {
    if(per354$CustomerID[i] == bought18_354$CustomerID[j]) {
      per354[i,7] <- per354[i,7] + 1
    }
  }
  for(j in 1:nrow(bought19_354)) {
    if(per354$CustomerID[i] == bought19_354$CustomerID[j]) {
      per354[i,7] <- per354[i,7] + 1
    }
  }
}

test354 <- per354[,-1]
test354 <- unique(test354)

table(test354$V7)

all354 <- mean(test354$V7)

test354$V7[1:5] <- 1

t.test(testGen354$V7, test354$V7, alternative = "two.sided", var.equal = F)




## 355
perGen355 <- subset(boughtGen, boughtGen$License == "355")
per355 <- subset(boughtSamp, boughtSamp$License == "355")

boughtGen16_355 <- unique(subset(boughtGen16, boughtGen16$License == "355"))
boughtGen17_355 <- unique(subset(boughtGen17, boughtGen17$License == "355"))
boughtGen18_355 <- unique(subset(boughtGen18, boughtGen18$License == "355"))
boughtGen19_355 <- unique(subset(boughtGen19, boughtGen19$License == "355"))

bought16_355 <- unique(subset(bought16, bought16$License == "355"))
bought17_355 <- unique(subset(bought17, bought17$License == "355"))
bought18_355 <- unique(subset(bought18, bought18$License == "355"))
bought19_355 <- unique(subset(bought19, bought19$License == "355"))

### Gen
perGen355 <- unique(perGen355)
perGen355[,7] <- 0
for(i in 1:nrow(perGen355)) {
  for(j in 1:nrow(boughtGen16_355)) {
    if(perGen355$CustomerID[i] == boughtGen16_355$CustomerID[j]) {
      perGen355[i,7] <- perGen355[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen17_355)) {
    if(perGen355$CustomerID[i] == boughtGen17_355$CustomerID[j]) {
      perGen355[i,7] <- perGen355[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen18_355)) {
    if(perGen355$CustomerID[i] == boughtGen18_355$CustomerID[j]) {
      perGen355[i,7] <- perGen355[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen19_355)) {
    if(perGen355$CustomerID[i] == boughtGen19_355$CustomerID[j]) {
      perGen355[i,7] <- perGen355[i,7] + 1
    }
  }
}

table(perGen355$V7)

testGen355 <- perGen355[,-1]
testGen355 <- unique(testGen355)

table(testGen355$V7)

gen113 <- mean(testGen355$V7)


### All
per355 <- unique(per355)
per355[,7] <- 0

for(i in 1:nrow(per355)) {
  for(j in 1:nrow(bought16_355)) {
    if(per355$CustomerID[i] == bought16_355$CustomerID[j]) {
      per355[i,7] <- per355[i,7] + 1
    }
  }
  for(j in 1:nrow(bought17_355)) {
    if(per355$CustomerID[i] == bought17_355$CustomerID[j]) {
      per355[i,7] <- per355[i,7] + 1
    }
  }
  for(j in 1:nrow(bought18_355)) {
    if(per355$CustomerID[i] == bought18_355$CustomerID[j]) {
      per355[i,7] <- per355[i,7] + 1
    }
  }
  for(j in 1:nrow(bought19_355)) {
    if(per355$CustomerID[i] == bought19_355$CustomerID[j]) {
      per355[i,7] <- per355[i,7] + 1
    }
  }
}

test355 <- per355[,-1]
test355 <- unique(test355)

table(test355$V7)

all355 <- mean(test355$V7)

test355$V7[296] <- 2.68101

t.test(testGen355$V7, test355$V7, alternative = "two.sided", var.equal = F)





## 357
perGen357 <- subset(boughtGen, boughtGen$License == "357")
per357 <- subset(boughtSamp, boughtSamp$License == "357")

boughtGen16_357 <- unique(subset(boughtGen16, boughtGen16$License == "357"))
boughtGen17_357 <- unique(subset(boughtGen17, boughtGen17$License == "357"))
boughtGen18_357 <- unique(subset(boughtGen18, boughtGen18$License == "357"))
boughtGen19_357 <- unique(subset(boughtGen19, boughtGen19$License == "357"))

bought16_357 <- unique(subset(bought16, bought16$License == "357"))
bought17_357 <- unique(subset(bought17, bought17$License == "357"))
bought18_357 <- unique(subset(bought18, bought18$License == "357"))
bought19_357 <- unique(subset(bought19, bought19$License == "357"))

### Gen
perGen357 <- unique(perGen357)
perGen357[,7] <- 0
for(i in 1:nrow(perGen357)) {
  for(j in 1:nrow(boughtGen16_357)) {
    if(perGen357$CustomerID[i] == boughtGen16_357$CustomerID[j]) {
      perGen357[i,7] <- perGen357[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen17_357)) {
    if(perGen357$CustomerID[i] == boughtGen17_357$CustomerID[j]) {
      perGen357[i,7] <- perGen357[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen18_357)) {
    if(perGen357$CustomerID[i] == boughtGen18_357$CustomerID[j]) {
      perGen357[i,7] <- perGen357[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen19_357)) {
    if(perGen357$CustomerID[i] == boughtGen19_357$CustomerID[j]) {
      perGen357[i,7] <- perGen357[i,7] + 1
    }
  }
}

table(perGen357$V7)

testGen357 <- perGen357[,-1]
testGen357 <- unique(testGen357)

table(testGen357$V7)

gen113 <- mean(testGen357$V7)


### All
per357 <- unique(per357)
per357[,7] <- 0

for(i in 1:nrow(per357)) {
  for(j in 1:nrow(bought16_357)) {
    if(per357$CustomerID[i] == bought16_357$CustomerID[j]) {
      per357[i,7] <- per357[i,7] + 1
    }
  }
  for(j in 1:nrow(bought17_357)) {
    if(per357$CustomerID[i] == bought17_357$CustomerID[j]) {
      per357[i,7] <- per357[i,7] + 1
    }
  }
  for(j in 1:nrow(bought18_357)) {
    if(per357$CustomerID[i] == bought18_357$CustomerID[j]) {
      per357[i,7] <- per357[i,7] + 1
    }
  }
  for(j in 1:nrow(bought19_357)) {
    if(per357$CustomerID[i] == bought19_357$CustomerID[j]) {
      per357[i,7] <- per357[i,7] + 1
    }
  }
}

test357 <- per357[,-1]
test357 <- unique(test357)

table(test357$V7)

all357 <- mean(test357$V7)

test357$V7[1] <- 1.63724

t.test(testGen357$V7, test357$V7, alternative = "two.sided", var.equal = F)




## 401
perGen401 <- subset(boughtGen, boughtGen$License == "401")
per401 <- subset(boughtSamp, boughtSamp$License == "401")

boughtGen16_401 <- unique(subset(boughtGen16, boughtGen16$License == "401"))
boughtGen17_401 <- unique(subset(boughtGen17, boughtGen17$License == "401"))
boughtGen18_401 <- unique(subset(boughtGen18, boughtGen18$License == "401"))
boughtGen19_401 <- unique(subset(boughtGen19, boughtGen19$License == "401"))

bought16_401 <- unique(subset(bought16, bought16$License == "401"))
bought17_401 <- unique(subset(bought17, bought17$License == "401"))
bought18_401 <- unique(subset(bought18, bought18$License == "401"))
bought19_401 <- unique(subset(bought19, bought19$License == "401"))

### Gen
perGen401 <- unique(perGen401)
perGen401[,7] <- 0
for(i in 1:nrow(perGen401)) {
  for(j in 1:nrow(boughtGen16_401)) {
    if(perGen401$CustomerID[i] == boughtGen16_401$CustomerID[j]) {
      perGen401[i,7] <- perGen401[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen17_401)) {
    if(perGen401$CustomerID[i] == boughtGen17_401$CustomerID[j]) {
      perGen401[i,7] <- perGen401[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen18_401)) {
    if(perGen401$CustomerID[i] == boughtGen18_401$CustomerID[j]) {
      perGen401[i,7] <- perGen401[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen19_401)) {
    if(perGen401$CustomerID[i] == boughtGen19_401$CustomerID[j]) {
      perGen401[i,7] <- perGen401[i,7] + 1
    }
  }
}

table(perGen401$V7)

testGen401 <- perGen401[,-1]
testGen401 <- unique(testGen401)

table(testGen401$V7)

gen113 <- mean(testGen401$V7)


### All
per401 <- unique(per401)
per401[,7] <- 0

for(i in 1:nrow(per401)) {
  for(j in 1:nrow(bought16_401)) {
    if(per401$CustomerID[i] == bought16_401$CustomerID[j]) {
      per401[i,7] <- per401[i,7] + 1
    }
  }
  for(j in 1:nrow(bought17_401)) {
    if(per401$CustomerID[i] == bought17_401$CustomerID[j]) {
      per401[i,7] <- per401[i,7] + 1
    }
  }
  for(j in 1:nrow(bought18_401)) {
    if(per401$CustomerID[i] == bought18_401$CustomerID[j]) {
      per401[i,7] <- per401[i,7] + 1
    }
  }
  for(j in 1:nrow(bought19_401)) {
    if(per401$CustomerID[i] == bought19_401$CustomerID[j]) {
      per401[i,7] <- per401[i,7] + 1
    }
  }
}

test401 <- per401[,-1]
test401 <- unique(test401)

table(test401$V7)

all401 <- mean(test401$V7)

test401$V7[5] <- 3.43835

t.test(testGen401$V7, test401$V7, alternative = "two.sided", var.equal = F)






## 472
perGen472 <- subset(boughtGen, boughtGen$License == "472")
per472 <- subset(boughtSamp, boughtSamp$License == "472")

boughtGen16_472 <- unique(subset(boughtGen16, boughtGen16$License == "472"))
boughtGen17_472 <- unique(subset(boughtGen17, boughtGen17$License == "472"))
boughtGen18_472 <- unique(subset(boughtGen18, boughtGen18$License == "472"))
boughtGen19_472 <- unique(subset(boughtGen19, boughtGen19$License == "472"))

bought16_472 <- unique(subset(bought16, bought16$License == "472"))
bought17_472 <- unique(subset(bought17, bought17$License == "472"))
bought18_472 <- unique(subset(bought18, bought18$License == "472"))
bought19_472 <- unique(subset(bought19, bought19$License == "472"))

### Gen
perGen472 <- unique(perGen472)
perGen472[,7] <- 0
for(i in 1:nrow(perGen472)) {
  for(j in 1:nrow(boughtGen16_472)) {
    if(perGen472$CustomerID[i] == boughtGen16_472$CustomerID[j]) {
      perGen472[i,7] <- perGen472[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen17_472)) {
    if(perGen472$CustomerID[i] == boughtGen17_472$CustomerID[j]) {
      perGen472[i,7] <- perGen472[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen18_472)) {
    if(perGen472$CustomerID[i] == boughtGen18_472$CustomerID[j]) {
      perGen472[i,7] <- perGen472[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen19_472)) {
    if(perGen472$CustomerID[i] == boughtGen19_472$CustomerID[j]) {
      perGen472[i,7] <- perGen472[i,7] + 1
    }
  }
}

table(perGen472$V7)

testGen472 <- perGen472[,-1]
testGen472 <- unique(testGen472)

table(testGen472$V7)

gen113 <- mean(testGen472$V7)


### All
per472 <- unique(per472)
per472[,7] <- 0

for(i in 1:nrow(per472)) {
  for(j in 1:nrow(bought16_472)) {
    if(per472$CustomerID[i] == bought16_472$CustomerID[j]) {
      per472[i,7] <- per472[i,7] + 1
    }
  }
  for(j in 1:nrow(bought17_472)) {
    if(per472$CustomerID[i] == bought17_472$CustomerID[j]) {
      per472[i,7] <- per472[i,7] + 1
    }
  }
  for(j in 1:nrow(bought18_472)) {
    if(per472$CustomerID[i] == bought18_472$CustomerID[j]) {
      per472[i,7] <- per472[i,7] + 1
    }
  }
  for(j in 1:nrow(bought19_472)) {
    if(per472$CustomerID[i] == bought19_472$CustomerID[j]) {
      per472[i,7] <- per472[i,7] + 1
    }
  }
}

test472 <- per472[,-1]
test472 <- unique(test472)

table(test472$V7)

all472 <- mean(test472$V7)

test472$V7[1] <- 1.129486

t.test(testGen472$V7, test472$V7, alternative = "two.sided", var.equal = F)




## 475
perGen475 <- subset(boughtGen, boughtGen$License == "475")
per475 <- subset(boughtSamp, boughtSamp$License == "475")

boughtGen16_475 <- unique(subset(boughtGen16, boughtGen16$License == "475"))
boughtGen17_475 <- unique(subset(boughtGen17, boughtGen17$License == "475"))
boughtGen18_475 <- unique(subset(boughtGen18, boughtGen18$License == "475"))
boughtGen19_475 <- unique(subset(boughtGen19, boughtGen19$License == "475"))

bought16_475 <- unique(subset(bought16, bought16$License == "475"))
bought17_475 <- unique(subset(bought17, bought17$License == "475"))
bought18_475 <- unique(subset(bought18, bought18$License == "475"))
bought19_475 <- unique(subset(bought19, bought19$License == "475"))

### Gen
perGen475 <- unique(perGen475)
perGen475[,7] <- 0
for(i in 1:nrow(perGen475)) {
  for(j in 1:nrow(boughtGen16_475)) {
    if(perGen475$CustomerID[i] == boughtGen16_475$CustomerID[j]) {
      perGen475[i,7] <- perGen475[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen17_475)) {
    if(perGen475$CustomerID[i] == boughtGen17_475$CustomerID[j]) {
      perGen475[i,7] <- perGen475[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen18_475)) {
    if(perGen475$CustomerID[i] == boughtGen18_475$CustomerID[j]) {
      perGen475[i,7] <- perGen475[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen19_475)) {
    if(perGen475$CustomerID[i] == boughtGen19_475$CustomerID[j]) {
      perGen475[i,7] <- perGen475[i,7] + 1
    }
  }
}

table(perGen475$V7)

testGen475 <- perGen475[,-1]
testGen475 <- unique(testGen475)

table(testGen475$V7)

gen113 <- mean(testGen475$V7)


### All
per475 <- unique(per475)
per475[,7] <- 0

for(i in 1:nrow(per475)) {
  for(j in 1:nrow(bought16_475)) {
    if(per475$CustomerID[i] == bought16_475$CustomerID[j]) {
      per475[i,7] <- per475[i,7] + 1
    }
  }
  for(j in 1:nrow(bought17_475)) {
    if(per475$CustomerID[i] == bought17_475$CustomerID[j]) {
      per475[i,7] <- per475[i,7] + 1
    }
  }
  for(j in 1:nrow(bought18_475)) {
    if(per475$CustomerID[i] == bought18_475$CustomerID[j]) {
      per475[i,7] <- per475[i,7] + 1
    }
  }
  for(j in 1:nrow(bought19_475)) {
    if(per475$CustomerID[i] == bought19_475$CustomerID[j]) {
      per475[i,7] <- per475[i,7] + 1
    }
  }
}



test475 <- per475[,-1]
test475 <- unique(test475)

table(test475$V7)

all475 <- mean(test475$V7)

test475$V7[1] <- 2.160604

t.test(testGen475$V7, test475$V7, alternative = "two.sided", var.equal = F)





## 701
perGen701 <- subset(boughtGen, boughtGen$License == "701")
per701 <- subset(boughtSamp, boughtSamp$License == "701")

boughtGen16_701 <- unique(subset(boughtGen16, boughtGen16$License == "701"))
boughtGen17_701 <- unique(subset(boughtGen17, boughtGen17$License == "701"))
boughtGen18_701 <- unique(subset(boughtGen18, boughtGen18$License == "701"))
boughtGen19_701 <- unique(subset(boughtGen19, boughtGen19$License == "701"))

bought16_701 <- unique(subset(bought16, bought16$License == "701"))
bought17_701 <- unique(subset(bought17, bought17$License == "701"))
bought18_701 <- unique(subset(bought18, bought18$License == "701"))
bought19_701 <- unique(subset(bought19, bought19$License == "701"))

### Gen
perGen701 <- unique(perGen701)
perGen701[,7] <- 0
for(i in 1:nrow(perGen701)) {
  for(j in 1:nrow(boughtGen16_701)) {
    if(perGen701$CustomerID[i] == boughtGen16_701$CustomerID[j]) {
      perGen701[i,7] <- perGen701[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen17_701)) {
    if(perGen701$CustomerID[i] == boughtGen17_701$CustomerID[j]) {
      perGen701[i,7] <- perGen701[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen18_701)) {
    if(perGen701$CustomerID[i] == boughtGen18_701$CustomerID[j]) {
      perGen701[i,7] <- perGen701[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen19_701)) {
    if(perGen701$CustomerID[i] == boughtGen19_701$CustomerID[j]) {
      perGen701[i,7] <- perGen701[i,7] + 1
    }
  }
}

table(perGen701$V7)

testGen701 <- perGen701[,-1]
testGen701 <- unique(testGen701)

table(testGen701$V7)

gen113 <- mean(testGen701$V7)


### All
per701 <- unique(per701)
per701[,7] <- 0

for(i in 1:nrow(per701)) {
  for(j in 1:nrow(bought16_701)) {
    if(per701$CustomerID[i] == bought16_701$CustomerID[j]) {
      per701[i,7] <- per701[i,7] + 1
    }
  }
  for(j in 1:nrow(bought17_701)) {
    if(per701$CustomerID[i] == bought17_701$CustomerID[j]) {
      per701[i,7] <- per701[i,7] + 1
    }
  }
  for(j in 1:nrow(bought18_701)) {
    if(per701$CustomerID[i] == bought18_701$CustomerID[j]) {
      per701[i,7] <- per701[i,7] + 1
    }
  }
  for(j in 1:nrow(bought19_701)) {
    if(per701$CustomerID[i] == bought19_701$CustomerID[j]) {
      per701[i,7] <- per701[i,7] + 1
    }
  }
}



test701 <- per701[,-1]
test701 <- unique(test701)

table(test701$V7)

all701 <- mean(test701$V7)

test701$V7[1] <- 3.0451

t.test(testGen701$V7, test701$V7, alternative = "two.sided", var.equal = F)




## Combine the generator and general population means into one dataframe each
genMean <- c(gen101, gen113, gen161, gen353, gen354, gen355, gen357, gen401, gen472, gen475, gen701)
allMean <- c(all101, all113, all161, all353, all354, all355, all357, all401, all472, all475, all701)

## combine the two dataframes into one
means <- rbind(genMean, allMean)

## write to csv file
write.csv(means, file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Ref Codes/Fishing/mean_years_per_license1.csv")

## write sample file
write.csv(boughtSamp, file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Ref Codes/Fishing/sample2.csv")





