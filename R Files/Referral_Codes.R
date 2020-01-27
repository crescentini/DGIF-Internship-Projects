rm(list=ls())

library(ggplot2)
# library for maps; download this first
library(USAboundaries)
# after running first package, it will prompt you to download this one
library(USAboundariesData)
# special features library, needed for two above packages
library(sf)
# for haversine function
library(pracma)
library(plotly)
library(leaflet)
library(leaflet.extras)


gen <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/refangler_gen_code.csv", header = T, nrows = 1172)

red <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/refangler_red_code.csv", header = T, nrows = 430)




### Current age barplots ####

ggplot(gen, aes(gen$currentAge)) + geom_bar() + labs(x = "Current Age", y = "Count", title = "Age of People Who Generated a Code") +
  scale_x_continuous(breaks=seq(0,82,10))

ggplot(red, aes(red$currentAge)) + geom_bar() + lims(y = c(0,47)) + labs(x = "Current Age", y = "Count", title = "Age of People Who Redeemed a Code") +
  scale_x_continuous(breaks=seq(0,82,10))

gentab <- table(factor(gen$currentAge, levels = min(gen$currentAge):max(gen$currentAge)))
redtab <- table(factor(red$currentAge, levels = min(red$currentAge):max(red$currentAge)))

tabs <- as.data.frame(cbind(gentab, redtab))
tabs[,3] <- 18:81

barplot(gentab, xlim = c(0,81), ylim = c(0,50), col = rgb(red = 0, blue = 1, green = 0, alpha = 0.5), main = "Age of Participants")
barplot(redtab, xlim = c(0,82), ylim = c(0,50), add = T, col = rgb(red = 1, blue = 0, green = 0, alpha = 0.5))
legend("topright", inset = 0.05, legend = c("Generated Code", "Redeemed Code"), fill = c("blue","red"), cex = 0.5)


summary(gen$currentAge)
summary(red$currentAge)


### Ethnicity ####

ggplot(gen, aes(gen$Ethnicity)) + geom_bar() + labs(x = "Ethnicity", y = "Count", title = "Ethnicity of People Who Generated a Code") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15)) +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25)

ggplot(red, aes(red$Ethnicity)) + geom_bar() + lims(y = c(0,937)) + labs(x = "Ethnicity", y = "Count", title = "Ethnicity of People Who Redeemed a Code") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15)) +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25)



### Gender ####

ggplot(gen, aes(gen$Gender)) + geom_bar() + labs(x = "Gender", y = "Count", title = "Gender of People Who Generated a Code") +
  theme(text = element_text(size = 15)) +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25)

ggplot(red, aes(red$Gender)) + geom_bar() + lims(y = c(0,934)) + labs(x = "Gender", y = "Count", title = "Gender of People Who Redeemed a Code") +
  theme(text = element_text(size = 15)) +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25)



### Date ####

ggplot(red, aes(as.Date(red$Date.of.code.redemption.License.purchase, format = "%m/%d/%Y"))) + geom_bar() +
      labs(x = "Date Redeemed", y = "Count", title = "Date of Code Redemption")# + lims(x = c("2018-04-01", "2018-10-01"))

# ^^ 6/12/18 - 6/16/18 highest amount

genDate <- license[license$CustomerID %in% gen$DGIFCustomerID,]

redDate <- license[license$CustomerID %in% red$DGIFCustomerID,]

ggplot(genDate, aes(as.Date(genDate$Sold_Date))) + geom_bar()

ggplot(redDate, aes(as.Date(redDate$Sold_Date))) + geom_bar()

unique(redDate$CustomerID)

table(redDate$License)

### 2 Years Before ####

before2gen <- license[license$CustomerID %in% gen$DGIFCustomerID,]

before2red <- license[license$CustomerID %in% red$DGIFCustomerID,]

unique(before2red$CustomerID)

table(before2red$License)

x <- before2red %>% group_by(CustomerID)

### 2016 Fishing Licenses ####

custIDgen16 <- license[license$CustomerID %in% gen$DGIFCustomerID,] # 1038, 425 unique

custIDred16 <- license[license$CustomerID %in% red$DGIFCustomerID,] # 9, 5 unique

custIDgen16 <- table(custIDgen16$License)
custIDgen16 <- subset(custIDgen16, custIDgen16 > 25)

custIDgen16 <- as.data.frame(custIDgen16)

## licenses > 9 bought
ggplot(custIDgen16, aes(custIDgen16$Var1, custIDgen16$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Top Ten Licenses Bought by Code Generators", subtitle = "Fiscal Year 2016") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))


ggplot(custIDgen16, aes(custIDgen16)) + geom_bar() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Generators", subtitle = "Fiscal Year 2016") +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))

ggplot(custIDred16, aes(custIDred16$License)) + geom_bar() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Redeemers", subtitle = "Fiscal Year 2016") +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))


ggplot(custIDgen16, aes(custIDgen16$Sold_Date)) + geom_bar()

autobuy16 <- subset(custIDgen16, Sold_Date == min(Sold_Date))

ggplot(autobuy1)

### 2017 Fishing Licenses ####

custIDgen17 <- license[license$CustomerID %in% gen$DGIFCustomerID,] # 1359, 573 unique

custIDred17 <- license[license$CustomerID %in% red$DGIFCustomerID,] # 12, 8 unique


custIDgen17 <- table(custIDgen17$License)
custIDgen17 <- subset(custIDgen17, custIDgen17 > 24)

custIDgen17 <- as.data.frame(custIDgen17)

## licenses > 9 bought
ggplot(custIDgen17, aes(custIDgen17$Var1, custIDgen17$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Top Ten Licenses Bought by Code Generators", subtitle = "Fiscal Year 2017") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))


ggplot(custIDgen17, aes(custIDgen17$License)) + geom_bar() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Generators", subtitle = "Fiscal Year 2017") +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))

ggplot(custIDred17, aes(custIDred17$License)) + geom_bar() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Redeemers", subtitle = "Fiscal Year 2017") +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))

autobuy17 <- subset(custIDgen17, Sold_Date == min(Sold_Date))

ggplot(autobuy)

### 2018 Fishing Licenses ####

custIDgen18 <- license[license$CustomerID %in% gen$DGIFCustomerID,] # 2232, 956 unique

custIDred18 <- license[license$CustomerID %in% red$DGIFCustomerID,] # 284, 234 unique


custIDgen18 <- table(custIDgen18$License)
custIDgen18 <- subset(custIDgen18, custIDgen18 > 40)

custIDgen18 <- as.data.frame(custIDgen18)

## licenses > 9 bought
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












### Combine CustomerIDofCodeGenerator and DGIFCustomerID. Doesn't work. Somethings wrong with the dataset ####

allRef <- merge(red, gen, by.x = "Customer.ID.of.code.generator", by.y = "DGIFCustomerID")

write.csv(allRef, file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Ref Codes/mergedData.csv")






### Heat Map Generated Zip Codes ####

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



### Heat Map Redeemed Zip Codes ####


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

multi <- custIDredAfter[duplicated(custIDredAfter[,'CustomerID']),]

multi <- table(multi$License)

single <- custIDredAfter[!duplicated(custIDredAfter[,'CustomerID']),]

single <- table(single$License)

multi <- subset(multi, multi > 3)

multi <- as.data.frame(multi)

single <- subset(single, single > 8)

single <- as.data.frame(single)

ggplot(single, aes(single$License)) + geom_bar() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Generators After", subtitle = "Top Ten Between 04/12/2018 and 04/12/2019") +
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))

ggplot(multi, aes(multi$Var1, multi$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Redeemers After", subtitle = "Top Ten For People Buying Multiple") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))

ggplot(single, aes(single$Var1, single$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Redeemers After", subtitle = "Top Ten For People Buying One") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))










