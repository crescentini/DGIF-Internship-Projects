rm(list=ls())

#### Used in Conjunction with Referral_Code_Fish.R

## Read in data files
gen <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/refhunter_gen_code.csv", header = T)
red <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/refhunter_red_code.csv", header = T)

# gen is Code Generators
# red is Code Redeemers


#### Current Age Barplots ####

## base versions allow for overlaying groups
gentab <- table(factor(gen$currentAge, levels = min(gen$currentAge):max(gen$currentAge)))
redtab <- table(factor(red$currentAge, levels = min(red$currentAge):max(gen$currentAge)))

tabs <- as.data.frame(cbind(gentab, redtab))
tabs[,3] <- 18:79

barplot(gentab, xlim = c(0,81), ylim = c(0,30), col = rgb(red = 0, blue = 1, green = 0, alpha = 0.5), main = "Age of Participants", xlab = "Age", ylab = "Count")
barplot(redtab, xlim = c(0,82), ylim = c(0,30), add = T, col = rgb(red = 1, blue = 0, green = 0, alpha = 0.5))
legend("topright", inset = 0.05, legend = c("Generated Code", "Redeemed Code"), fill = c("blue","red"), cex = 0.5)

summary(gen$currentAge)
summary(red$currentAge)


#### Ethnicity ####

ggplot(gen, aes(gen$Ethnicity)) + geom_bar() + labs(x = "Ethnicity", y = "Count", title = "Ethnicity of People Who Generated a Code") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15)) +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25)

ggplot(red, aes(red$Ethnicity)) + geom_bar() + lims(y = c(0,514)) + labs(x = "Ethnicity", y = "Count", title = "Ethnicity of People Who Redeemed a Code") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15)) +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25)



#### Gender ####

ggplot(gen, aes(gen$Gender)) + geom_bar() + labs(x = "Gender", y = "Count", title = "Gender of People Who Generated a Code") +
  theme(text = element_text(size = 15)) +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25)

ggplot(red, aes(red$Gender)) + geom_bar() + lims(y = c(0,551)) + labs(x = "Gender", y = "Count", title = "Gender of People Who Redeemed a Code") +
  theme(text = element_text(size = 15)) +
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25)



#### Date of Redemption ####

ggplot(red, aes(as.Date(red$Date.of.code.redemption.License.purchase, format = "%m/%d/%Y"))) + geom_bar() +
  labs(x = "Date Redeemed", y = "Count", title = "Date of Code Redemption")# + lims(x = c("2018-04-01", "2018-10-01"))



### 2 Years Before ####

before2gen <- license[license$CustomerID %in% gen$DGIFCustomerID,] ## 3224, 493 Unique

before2red <- license[license$CustomerID %in% red$DGIFCustomerID,] ## 0

unique(before2gen$CustomerID)

table(before2gen$License)



#### 2016 Fishing Licenses ####

custIDgen16 <- license[license$CustomerID %in% gen$DGIFCustomerID,] # 1320, 358 unique

custIDred16 <- license[license$CustomerID %in% red$DGIFCustomerID,] # 0

custIDgen16 <- table(custIDgen16$License)
custIDgen16 <- subset(custIDgen16, custIDgen16 > 25)

custIDgen16 <- as.data.frame(custIDgen16)

## licenses > 25 bought
ggplot(custIDgen16, aes(custIDgen16$Var1, custIDgen16$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Top Ten Licenses Bought by Code Generators", subtitle = "Fiscal Year 2016") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + lims(y = c(0,316))



#### 2017 Fishing Licenses ####

custIDgen17 <- license[license$CustomerID %in% gen$DGIFCustomerID,] # 1476, 573 unique

custIDred17 <- license[license$CustomerID %in% red$DGIFCustomerID,] # 0


custIDgen17 <- table(custIDgen17$License)
custIDgen17 <- subset(custIDgen17, custIDgen17 > 34)

custIDgen17 <- as.data.frame(custIDgen17)

## licenses > 34 bought
ggplot(custIDgen17, aes(custIDgen17$Var1, custIDgen17$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Top Ten Licenses Bought by Code Generators", subtitle = "Fiscal Year 2017") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + lims(y = c(0,316))



### 2018 Fishing Licenses ####

custIDgen18 <- license[license$CustomerID %in% gen$DGIFCustomerID,] # 1686, 450 unique

custIDred18 <- license[license$CustomerID %in% red$DGIFCustomerID,] # 0


custIDgen18 <- table(custIDgen18$License)
custIDgen18 <- subset(custIDgen18, custIDgen18 > 39)

custIDgen18 <- as.data.frame(custIDgen18)

## licenses > 39 bought
ggplot(custIDgen18, aes(custIDgen18$Var1, custIDgen18$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Top Ten Licenses Bought by Code Generators", subtitle = "Fiscal Year 2018") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + lims(y = c(0,316))



### 2019 Fishing Licenses ####

custIDgen19 <- license[license$CustomerID %in% gen$DGIFCustomerID,] # 1960, 577 unique

custIDred19 <- license[license$CustomerID %in% red$DGIFCustomerID,] # 303, 170 unique


custIDgen19 <- table(custIDgen19$License)
custIDgen19 <- subset(custIDgen19, custIDgen19 > 52)

custIDgen19 <- as.data.frame(custIDgen19)

## licenses > 52 bought
ggplot(custIDgen19, aes(custIDgen19$Var1, custIDgen19$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Top Ten Licenses Bought by Code Generators", subtitle = "Fiscal Year 2019") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))



#### Licenses Sold During/After Event ####

custIDgenAfter <- license[license$CustomerID %in% gen$DGIFCustomerID,] # 1731, 539 unique

custIDredAfter <- license[license$CustomerID %in% red$DGIFCustomerID,] # 309, 170 unique


custIDgenAfter <- table(custIDgenAfter$License)
custIDgenAfter <- subset(custIDgenAfter, custIDgenAfter > 43)

custIDgenAfter <- as.data.frame(custIDgenAfter)


custIDredAfter <- table(custIDredAfter$License)
custIDredAfter <- subset(custIDredAfter, custIDredAfter > 6)

custIDredAfter <- as.data.frame(custIDredAfter)


## licenses > 9 bought
ggplot(custIDgenAfter, aes(custIDgenAfter$Var1, custIDgenAfter$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Generators After", subtitle = "Top Ten Between 09/06/2018 and 09/06/2019") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))

ggplot(custIDredAfter, aes(custIDredAfter$Var1, custIDredAfter$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Redeemers After", subtitle = "Top Ten Between 09/06/2018 and 09/06/2019") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))



#### Licenses Sold the Following Year ####

custIDgenFollow <- license[license$CustomerID %in% gen$DGIFCustomerID,] # 264, 155 unique

custIDredFollow <- license[license$CustomerID %in% red$DGIFCustomerID,] # 11, 10 unique

table(custIDredFollow$License)



#### Single and Multiple ####

## Multiple Licenses purchased
multi <- custIDredAfter[duplicated(custIDredAfter[,'CustomerID']),]
multi <- table(multi$License)

## Single license purchased
single <- custIDredAfter[!duplicated(custIDredAfter[,'CustomerID']),]
single <- table(single$License)

## Top 10 licenses
multi <- subset(multi, multi > 4)
multi <- as.data.frame(multi)

## Top 10 licenses
single <- subset(single, single > 3)
single <- as.data.frame(single)

## Plot top 10 Licenses
ggplot(multi, aes(multi$Var1, multi$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Redeemers After", subtitle = "Top Ten For People Buying Multiple") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))

ggplot(single, aes(single$Var1, single$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Code Redeemers After", subtitle = "Top Ten For People Buying One") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))



#### Combine Survey and Code Generator Files ####

## read in files
survey <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Ref Codes/Hunting/ref_curhunters_18_r.csv", header = T)
genCode <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/License Sales/refhunter_gen_code_match.csv", header = T)

## Combine based on emailAddress
test <- merge(genCode, survey, by = "emailAddress")

## write to a csv
write.csv(test, file = "C:/Users/wxc36638/Desktop/Projects/License Sales/combined_hunters.csv")



#### What survey respondents bought in two years before ####

survey2before <- license[license$CustomerID %in% test$DGIFCustomerID,] # 950, 126 unique

survey2before1 <- table(survey2before$License)
survey2before1 <- subset(survey2before1, survey2before1 > 20)
survey2before1 <- as.data.frame(survey2before1)

## top ten licenses purchased
ggplot(survey2before1, aes(survey2before1$Var1, survey2before1$Freq)) + geom_col() + labs(x = "License Type", y = "Count", title = "Licenses Bought by Survey Respondents", subtitle = "Top Ten Between 09/06/2016 and 09/06/2018") +
  geom_text(stat = 'identity', aes(label = Freq), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))

x <- unique(survey2before$CustomerID) ## 126/146 customerIDs found

survey2beforeNot <- test[!(test$DGIFCustomerID %in% license$CustomerID),] ## 20 not found. can find 1 extra based on emails

## Age of survey respondents
ggplot(test, aes(test$currentAge)) + geom_bar(width = 1) + labs(x = "Age", y = "Count", title = "Age of Survey Respondents") +
  theme(text = element_text(size = 15)) + lims(x = c(18,80))

summary(test$currentAge)



#### Percent of People having each license type ####

## filter out returned licenses
bought <- subset(license, license$Sold_Number == 1)

## licenses bought by code generators
boughtGen <- bought[bought$CustomerID %in% gen$DGIFCustomerID,]

## number in each group
nBoughtGen <- nrow(boughtGen)
nBought <- nrow(bought)



### 101 

## Collect number of each license type for both groups
perGen101 <- nrow(subset(boughtGen, boughtGen$License == "101"))
per101 <- nrow(subset(bought, bought$License == "101"))

## build numerical vectors for t-test
x101 <- 0
x101[1:nBoughtGen] <- 0
x101[1:perGen101] <- 1

y101 <- 0
y101[1:nBought] <- 0
y101[1:per101] <- 1

## unused except for comparison
t.test(x101, y101, alternative = "two.sided", var.equal = F)

## build 2x2 grid for Chi-square test
test <- matrix(data = c(131, 6264, 46543, 4602652), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)
test

## run Chi-square test. P-value displayed to the right
chisq.test(test) ## 0


## Repeat for all other license types

### 103
perGen103 <- nrow(subset(boughtGen, boughtGen$License == "103"))
per103 <- nrow(subset(bought, bought$License == "103"))

x103 <- 0
x103[1:nBoughtGen] <- 0
x103[1:perGen103] <- 1

y103 <- 0
y103[1:nBought] <- 0
y103[1:per103] <- 1

t.test(x103, y103, alternative = "two.sided", var.equal = F)

test <- matrix(data = c(18, 6377, 10764, 4638431), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)
test

chisq.test(test) ## .4838


## 111
perGen111 <- nrow(subset(boughtGen, boughtGen$License == "111"))
per111 <- nrow(subset(bought, bought$License == "111"))

x111 <- 0
x111[1:nBoughtGen] <- 0
x111[1:perGen111] <- 1

y111 <- 0
y111[1:nBought] <- 0
y111[1:per111] <- 1

t.test(x111, y111, alternative = "two.sided", var.equal = F)

test <- matrix(data = c(22, 6373, 23537, 4625658), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)
test

chisq.test(test) ## .08203


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

test <- matrix(data = c(1016, 5379, 539777, 4109418), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)
test

chisq.test(test) ## 0


## 114
perGen114 <- nrow(subset(boughtGen, boughtGen$License == "114"))
per114 <- nrow(subset(bought, bought$License == "114"))

x114 <- 0
x114[1:nBoughtGen] <- 0
x114[1:perGen114] <- 1

y114 <- 0
y114[1:nBought] <- 0
y114[1:per114] <- 1

t.test(x114, y114, alternative = "two.sided", var.equal = F)

test <- matrix(data = c(76, 6319, 41026, 4608169), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)
test

chisq.test(test) ## 0



## 119
perGen119 <- nrow(subset(boughtGen, boughtGen$License == "119"))
per119 <- nrow(subset(bought, bought$License == "119"))

x119 <- 0
x119[1:nBoughtGen] <- 0
x119[1:perGen119] <- 1

y119 <- 0
y119[1:nBought] <- 0
y119[1:per119] <- 1

t.test(x119, y119, alternative = "two.sided", var.equal = F)

test <- matrix(data = c(531, 5864, 226095, 4423100), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)
test

chisq.test(test) ## 0



## 129
perGen129 <- nrow(subset(boughtGen, boughtGen$License == "129"))
per129 <- nrow(subset(bought, bought$License == "129"))

x129 <- 0
x129[1:nBoughtGen] <- 0
x129[1:perGen129] <- 1

y129 <- 0
y129[1:nBought] <- 0
y129[1:per129] <- 1

t.test(x129, y129, alternative = "two.sided", var.equal = F)

test <- matrix(data = c(570, 5825, 305521, 4343674), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)
test

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

test <- matrix(data = c(1081, 5314, 624287, 4024908), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)
test

chisq.test(test) ## 0



## 167
perGen167 <- nrow(subset(boughtGen, boughtGen$License == "167"))
per167 <- nrow(subset(bought, bought$License == "167"))

x167 <- 0
x167[1:nBoughtGen] <- 0
x167[1:perGen167] <- 1

y167 <- 0
y167[1:nBought] <- 0
y167[1:per167] <- 1

t.test(x167, y167, alternative = "two.sided", var.equal = F)

test <- matrix(data = c(228, 6167, 121879, 4527316), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)
test

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

test <- matrix(data = c(535, 5860, 788007, 3861188), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)
test

chisq.test(test) ## 0



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

test <- matrix(data = c(162, 6233, 190013, 4459182), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)
test

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

test <- matrix(data = c(197, 6198, 72116, 4577079), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)
test

chisq.test(test) ## .001406


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

test <- matrix(data = c(485, 5910, 336928, 4312267), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)
test

chisq.test(test) ## .3103


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

test <- matrix(data = c(245, 6150, 93808, 4555387), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)
test

chisq.test(test) ## 0


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

test <- matrix(data = c(132, 6263, 155063, 4494132), byrow = T, nrow = 2)
rownames(test) <- c("Gen", "Full")
colnames(test) <- c("Bought", "Not")
test <- as.data.frame(test)
test

chisq.test(test) ## 0



#### Mean number of years each license is bought ####
rm(list=ls())

## filter out returned licenses
bought <- subset(license, license$Sold_Number == 1)

## collect licenses bought by code generators
boughtGen <- bought[bought$CustomerID %in% gen$DGIFCustomerID,]

## sample total population that bought a license in the year before program began
boughtSamp <- subset(bought, bought$FY == "2018")
boughtSamp <- subset(boughtSamp, boughtSamp$License == "101" | boughtSamp$License == "103" | boughtSamp$License == "111" | boughtSamp$License == "113" | boughtSamp$License == "114")
boughtSampID <- unique(boughtSamp$CustomerID)
## sample same number of people as in the code generator group
boughtSampID <- boughtSampID[sample(length(boughtSampID),615, replace = F)]
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

## Number in each group
nBoughtGen <- nrow(boughtGen)
nBoughtGen16 <- nrow(boughtGen16)
nBoughtGen17 <- nrow(boughtGen17)
nBoughtGen18 <- nrow(boughtGen18)
nBoughtGen19 <- nrow(boughtGen19)
nBought <- nrow(bought)


### 101
## Select only license type 101 for both groups
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

## we want only one entry per year per customerID
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

#test101$V7[4] <- 2.759625

## two sample t-test
t.test(testGen101$V7, test101$V7, alternative = "two.sided", var.equal = F)



### Repeat for all license types

### 103
perGen103 <- subset(boughtGen, boughtGen$License == "103")
per103 <- subset(boughtSamp, boughtSamp$License == "103")

boughtGen16_103 <- unique(subset(boughtGen16, boughtGen16$License == "103"))
boughtGen17_103 <- unique(subset(boughtGen17, boughtGen17$License == "103"))
boughtGen18_103 <- unique(subset(boughtGen18, boughtGen18$License == "103"))
boughtGen19_103 <- unique(subset(boughtGen19, boughtGen19$License == "103"))

bought16_103 <- unique(subset(bought16, bought16$License == "103"))
bought17_103 <- unique(subset(bought17, bought17$License == "103"))
bought18_103 <- unique(subset(bought18, bought18$License == "103"))
bought19_103 <- unique(subset(bought19, bought19$License == "103"))

### Gen
perGen103 <- unique(perGen103)
perGen103[,7] <- 0
for(i in 1:nrow(perGen103)) {
  for(j in 1:nrow(boughtGen16_103)) {
    if(perGen103$CustomerID[i] == boughtGen16_103$CustomerID[j]) {
      perGen103[i,7] <- perGen103[i,7] + 1
    }
  }
  ## Commented out because there are no 103 licenses bought in 2017
  #for(j in 1:nrow(boughtGen17_103)) {
  #  if(perGen103$CustomerID[i] == boughtGen17_103$CustomerID[j]) {
  #    perGen103[i,7] <- perGen103[i,7] + 1
  #  }
  #}
  for(j in 1:nrow(boughtGen18_103)) {
    if(perGen103$CustomerID[i] == boughtGen18_103$CustomerID[j]) {
      perGen103[i,7] <- perGen103[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen19_103)) {
    if(perGen103$CustomerID[i] == boughtGen19_103$CustomerID[j]) {
      perGen103[i,7] <- perGen103[i,7] + 1
    }
  }
}

testGen103 <- perGen103[,-1]
testGen103 <- unique(testGen103)

table(testGen103$V7)

gen103 <- mean(testGen103$V7)



### All
per103 <- unique(per103)
per103[,7] <- 0

for(i in 1:nrow(per103)) {
  for(j in 1:nrow(bought16_103)) {
    if(per103$CustomerID[i] == bought16_103$CustomerID[j]) {
      per103[i,7] <- per103[i,7] + 1
    }
  }
  for(j in 1:nrow(bought17_103)) {
    if(per103$CustomerID[i] == bought17_103$CustomerID[j]) {
      per103[i,7] <- per103[i,7] + 1
    }
  }
  for(j in 1:nrow(bought18_103)) {
    if(per103$CustomerID[i] == bought18_103$CustomerID[j]) {
      per103[i,7] <- per103[i,7] + 1
    }
  }
  for(j in 1:nrow(bought19_103)) {
    if(per103$CustomerID[i] == bought19_103$CustomerID[j]) {
      per103[i,7] <- per103[i,7] + 1
    }
  }
}

test103 <- per103[,-1]
test103 <- unique(test103)

table(test103$V7)

all103 <- mean(test103$V7)

test103$V7[1] <- 1.67368

t.test(testGen103$V7, test103$V7, alternative = "two.sided", var.equal = F)




### 111
perGen111 <- subset(boughtGen, boughtGen$License == "111")
per111 <- subset(boughtSamp, boughtSamp$License == "111")

boughtGen16_111 <- unique(subset(boughtGen16, boughtGen16$License == "111"))
boughtGen17_111 <- unique(subset(boughtGen17, boughtGen17$License == "111"))
boughtGen18_111 <- unique(subset(boughtGen18, boughtGen18$License == "111"))
boughtGen19_111 <- unique(subset(boughtGen19, boughtGen19$License == "111"))

bought16_111 <- unique(subset(bought16, bought16$License == "111"))
bought17_111 <- unique(subset(bought17, bought17$License == "111"))
bought18_111 <- unique(subset(bought18, bought18$License == "111"))
bought19_111 <- unique(subset(bought19, bought19$License == "111"))

### Gen
perGen111 <- unique(perGen111)
perGen111[,7] <- 0
for(i in 1:nrow(perGen111)) {
  for(j in 1:nrow(boughtGen16_111)) {
    if(perGen111$CustomerID[i] == boughtGen16_111$CustomerID[j]) {
      perGen111[i,7] <- perGen111[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen17_111)) {
    if(perGen111$CustomerID[i] == boughtGen17_111$CustomerID[j]) {
      perGen111[i,7] <- perGen111[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen18_111)) {
    if(perGen111$CustomerID[i] == boughtGen18_111$CustomerID[j]) {
      perGen111[i,7] <- perGen111[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen19_111)) {
    if(perGen111$CustomerID[i] == boughtGen19_111$CustomerID[j]) {
      perGen111[i,7] <- perGen111[i,7] + 1
    }
  }
}

testGen111 <- perGen111[,-1]
testGen111 <- unique(testGen111)

table(testGen111$V7)

gen111 <- mean(testGen111$V7)


### All
per111 <- unique(per111)
per111[,7] <- 0

for(i in 1:nrow(per111)) {
  for(j in 1:nrow(bought16_111)) {
    if(per111$CustomerID[i] == bought16_111$CustomerID[j]) {
      per111[i,7] <- per111[i,7] + 1
    }
  }
  for(j in 1:nrow(bought17_111)) {
    if(per111$CustomerID[i] == bought17_111$CustomerID[j]) {
      per111[i,7] <- per111[i,7] + 1
    }
  }
  for(j in 1:nrow(bought18_111)) {
    if(per111$CustomerID[i] == bought18_111$CustomerID[j]) {
      per111[i,7] <- per111[i,7] + 1
    }
  }
  for(j in 1:nrow(bought19_111)) {
    if(per111$CustomerID[i] == bought19_111$CustomerID[j]) {
      per111[i,7] <- per111[i,7] + 1
    }
  }
}

test111 <- per111[,-1]
test111 <- unique(test111)

table(test111$V7)

all111 <- mean(test111$V7)

test111$V7[3] <- 2.14486

t.test(testGen111$V7, test111$V7, alternative = "two.sided", var.equal = F)



### 113
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

test113$V7[1] <- 1.56023

t.test(testGen113$V7, test113$V7, alternative = "two.sided", var.equal = F)




### 114
perGen114 <- subset(boughtGen, boughtGen$License == "114")
per114 <- subset(boughtSamp, boughtSamp$License == "114")

boughtGen16_114 <- unique(subset(boughtGen16, boughtGen16$License == "114"))
boughtGen17_114 <- unique(subset(boughtGen17, boughtGen17$License == "114"))
boughtGen18_114 <- unique(subset(boughtGen18, boughtGen18$License == "114"))
boughtGen19_114 <- unique(subset(boughtGen19, boughtGen19$License == "114"))

bought16_114 <- unique(subset(bought16, bought16$License == "114"))
bought17_114 <- unique(subset(bought17, bought17$License == "114"))
bought18_114 <- unique(subset(bought18, bought18$License == "114"))
bought19_114 <- unique(subset(bought19, bought19$License == "114"))

### Gen
perGen114 <- unique(perGen114)
perGen114[,7] <- 0
for(i in 1:nrow(perGen114)) {
  for(j in 1:nrow(boughtGen16_114)) {
    if(perGen114$CustomerID[i] == boughtGen16_114$CustomerID[j]) {
      perGen114[i,7] <- perGen114[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen17_114)) {
    if(perGen114$CustomerID[i] == boughtGen17_114$CustomerID[j]) {
      perGen114[i,7] <- perGen114[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen18_114)) {
    if(perGen114$CustomerID[i] == boughtGen18_114$CustomerID[j]) {
      perGen114[i,7] <- perGen114[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen19_114)) {
    if(perGen114$CustomerID[i] == boughtGen19_114$CustomerID[j]) {
      perGen114[i,7] <- perGen114[i,7] + 1
    }
  }
}

testGen114 <- perGen114[,-1]
testGen114 <- unique(testGen114)

table(testGen114$V7)

gen114 <- mean(testGen114$V7)


### All
per114 <- unique(per114)
per114[,7] <- 0

for(i in 1:nrow(per114)) {
  for(j in 1:nrow(bought16_114)) {
    if(per114$CustomerID[i] == bought16_114$CustomerID[j]) {
      per114[i,7] <- per114[i,7] + 1
    }
  }
  for(j in 1:nrow(bought17_114)) {
    if(per114$CustomerID[i] == bought17_114$CustomerID[j]) {
      per114[i,7] <- per114[i,7] + 1
    }
  }
  for(j in 1:nrow(bought18_114)) {
    if(per114$CustomerID[i] == bought18_114$CustomerID[j]) {
      per114[i,7] <- per114[i,7] + 1
    }
  }
  for(j in 1:nrow(bought19_114)) {
    if(per114$CustomerID[i] == bought19_114$CustomerID[j]) {
      per114[i,7] <- per114[i,7] + 1
    }
  }
}

test114 <- per114[,-1]
test114 <- unique(test114)

table(test114$V7)

all114 <- mean(test114$V7)

test114$V7[42] <- 2.81405

t.test(testGen114$V7, test114$V7, alternative = "two.sided", var.equal = F)




### 119
perGen119 <- subset(boughtGen, boughtGen$License == "119")
per119 <- subset(boughtSamp, boughtSamp$License == "119")

boughtGen16_119 <- unique(subset(boughtGen16, boughtGen16$License == "119"))
boughtGen17_119 <- unique(subset(boughtGen17, boughtGen17$License == "119"))
boughtGen18_119 <- unique(subset(boughtGen18, boughtGen18$License == "119"))
boughtGen19_119 <- unique(subset(boughtGen19, boughtGen19$License == "119"))

bought16_119 <- unique(subset(bought16, bought16$License == "119"))
bought17_119 <- unique(subset(bought17, bought17$License == "119"))
bought18_119 <- unique(subset(bought18, bought18$License == "119"))
bought19_119 <- unique(subset(bought19, bought19$License == "119"))

### Gen
perGen119 <- unique(perGen119)
perGen119[,7] <- 0
for(i in 1:nrow(perGen119)) {
  for(j in 1:nrow(boughtGen16_119)) {
    if(perGen119$CustomerID[i] == boughtGen16_119$CustomerID[j]) {
      perGen119[i,7] <- perGen119[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen17_119)) {
    if(perGen119$CustomerID[i] == boughtGen17_119$CustomerID[j]) {
      perGen119[i,7] <- perGen119[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen18_119)) {
    if(perGen119$CustomerID[i] == boughtGen18_119$CustomerID[j]) {
      perGen119[i,7] <- perGen119[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen19_119)) {
    if(perGen119$CustomerID[i] == boughtGen19_119$CustomerID[j]) {
      perGen119[i,7] <- perGen119[i,7] + 1
    }
  }
}

testGen119 <- perGen119[,-1]
testGen119 <- unique(testGen119)

table(testGen119$V7)

gen119 <- mean(testGen119$V7)


### All
per119 <- unique(per119)
per119[,7] <- 0

for(i in 1:nrow(per119)) {
  for(j in 1:nrow(bought16_119)) {
    if(per119$CustomerID[i] == bought16_119$CustomerID[j]) {
      per119[i,7] <- per119[i,7] + 1
    }
  }
  for(j in 1:nrow(bought17_119)) {
    if(per119$CustomerID[i] == bought17_119$CustomerID[j]) {
      per119[i,7] <- per119[i,7] + 1
    }
  }
  for(j in 1:nrow(bought18_119)) {
    if(per119$CustomerID[i] == bought18_119$CustomerID[j]) {
      per119[i,7] <- per119[i,7] + 1
    }
  }
  for(j in 1:nrow(bought19_119)) {
    if(per119$CustomerID[i] == bought19_119$CustomerID[j]) {
      per119[i,7] <- per119[i,7] + 1
    }
  }
}

test119 <- per119[,-1]
test119 <- unique(test119)

table(test119$V7)

all119 <- mean(test119$V7)

test119$V7[2] <- 2.79301

t.test(testGen119$V7, test119$V7, alternative = "two.sided", var.equal = F)




### 129
perGen129 <- subset(boughtGen, boughtGen$License == "129")
per129 <- subset(boughtSamp, boughtSamp$License == "129")

boughtGen16_129 <- unique(subset(boughtGen16, boughtGen16$License == "129"))
boughtGen17_129 <- unique(subset(boughtGen17, boughtGen17$License == "129"))
boughtGen18_129 <- unique(subset(boughtGen18, boughtGen18$License == "129"))
boughtGen19_129 <- unique(subset(boughtGen19, boughtGen19$License == "129"))

bought16_129 <- unique(subset(bought16, bought16$License == "129"))
bought17_129 <- unique(subset(bought17, bought17$License == "129"))
bought18_129 <- unique(subset(bought18, bought18$License == "129"))
bought19_129 <- unique(subset(bought19, bought19$License == "129"))

### Gen
perGen129 <- unique(perGen129)
perGen129[,7] <- 0
for(i in 1:nrow(perGen129)) {
  for(j in 1:nrow(boughtGen16_129)) {
    if(perGen129$CustomerID[i] == boughtGen16_129$CustomerID[j]) {
      perGen129[i,7] <- perGen129[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen17_129)) {
    if(perGen129$CustomerID[i] == boughtGen17_129$CustomerID[j]) {
      perGen129[i,7] <- perGen129[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen18_129)) {
    if(perGen129$CustomerID[i] == boughtGen18_129$CustomerID[j]) {
      perGen129[i,7] <- perGen129[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen19_129)) {
    if(perGen129$CustomerID[i] == boughtGen19_129$CustomerID[j]) {
      perGen129[i,7] <- perGen129[i,7] + 1
    }
  }
}

testGen129 <- perGen129[,-1]
testGen129 <- unique(testGen129)

table(testGen129$V7)

gen129 <- mean(testGen129$V7)


### All
per129 <- unique(per129)
per129[,7] <- 0

for(i in 1:nrow(per129)) {
  for(j in 1:nrow(bought16_129)) {
    if(per129$CustomerID[i] == bought16_129$CustomerID[j]) {
      per129[i,7] <- per129[i,7] + 1
    }
  }
  for(j in 1:nrow(bought17_129)) {
    if(per129$CustomerID[i] == bought17_129$CustomerID[j]) {
      per129[i,7] <- per129[i,7] + 1
    }
  }
  for(j in 1:nrow(bought18_129)) {
    if(per129$CustomerID[i] == bought18_129$CustomerID[j]) {
      per129[i,7] <- per129[i,7] + 1
    }
  }
  for(j in 1:nrow(bought19_129)) {
    if(per129$CustomerID[i] == bought19_129$CustomerID[j]) {
      per129[i,7] <- per129[i,7] + 1
    }
  }
}

test129 <- per129[,-1]
test129 <- unique(test129)

table(test129$V7)

all129 <- mean(test129$V7)

test129$V7[1] <- 2.0068

t.test(testGen129$V7, test129$V7, alternative = "two.sided", var.equal = F)




### 161
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

test161$V7[1] <- 3.667402

t.test(testGen161$V7, test161$V7, alternative = "two.sided", var.equal = F)




### 167
perGen167 <- subset(boughtGen, boughtGen$License == "167")
per167 <- subset(boughtSamp, boughtSamp$License == "167")

boughtGen16_167 <- unique(subset(boughtGen16, boughtGen16$License == "167"))
boughtGen17_167 <- unique(subset(boughtGen17, boughtGen17$License == "167"))
boughtGen18_167 <- unique(subset(boughtGen18, boughtGen18$License == "167"))
boughtGen19_167 <- unique(subset(boughtGen19, boughtGen19$License == "167"))

bought16_167 <- unique(subset(bought16, bought16$License == "167"))
bought17_167 <- unique(subset(bought17, bought17$License == "167"))
bought18_167 <- unique(subset(bought18, bought18$License == "167"))
bought19_167 <- unique(subset(bought19, bought19$License == "167"))

### Gen
perGen167 <- unique(perGen167)
perGen167[,7] <- 0
for(i in 1:nrow(perGen167)) {
  for(j in 1:nrow(boughtGen16_167)) {
    if(perGen167$CustomerID[i] == boughtGen16_167$CustomerID[j]) {
      perGen167[i,7] <- perGen167[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen17_167)) {
    if(perGen167$CustomerID[i] == boughtGen17_167$CustomerID[j]) {
      perGen167[i,7] <- perGen167[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen18_167)) {
    if(perGen167$CustomerID[i] == boughtGen18_167$CustomerID[j]) {
      perGen167[i,7] <- perGen167[i,7] + 1
    }
  }
  for(j in 1:nrow(boughtGen19_167)) {
    if(perGen167$CustomerID[i] == boughtGen19_167$CustomerID[j]) {
      perGen167[i,7] <- perGen167[i,7] + 1
    }
  }
}

testGen167 <- perGen167[,-1]
testGen167 <- unique(testGen167)

table(testGen167$V7)

gen167 <- mean(testGen167$V7)


### All
per167 <- unique(per167)
per167[,7] <- 0

for(i in 1:nrow(per167)) {
  for(j in 1:nrow(bought16_167)) {
    if(per167$CustomerID[i] == bought16_167$CustomerID[j]) {
      per167[i,7] <- per167[i,7] + 1
    }
  }
  for(j in 1:nrow(bought17_167)) {
    if(per167$CustomerID[i] == bought17_167$CustomerID[j]) {
      per167[i,7] <- per167[i,7] + 1
    }
  }
  for(j in 1:nrow(bought18_167)) {
    if(per167$CustomerID[i] == bought18_167$CustomerID[j]) {
      per167[i,7] <- per167[i,7] + 1
    }
  }
  for(j in 1:nrow(bought19_167)) {
    if(per167$CustomerID[i] == bought19_167$CustomerID[j]) {
      per167[i,7] <- per167[i,7] + 1
    }
  }
}

test167 <- per167[,-1]
test167 <- unique(test167)

table(test167$V7)

all167 <- mean(test167$V7)

test167$V7[1] <- 1.933339

t.test(testGen167$V7, test167$V7, alternative = "two.sided", var.equal = F)





### 353
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

testGen353 <- perGen353[,-1]
testGen353 <- unique(testGen353)

table(testGen353$V7)

gen353 <- mean(testGen353$V7)



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

test353$V7[1] <- 1.864466

t.test(testGen353$V7, test353$V7, alternative = "two.sided", var.equal = F)




### 355
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

testGen355 <- perGen355[,-1]
testGen355 <- unique(testGen355)

table(testGen355$V7)

gen355 <- mean(testGen355$V7)


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

test355$V7[60] <- 3.280256

t.test(testGen355$V7, test355$V7, alternative = "two.sided", var.equal = F)



### 401
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

testGen401 <- perGen401[,-1]
testGen401 <- unique(testGen401)

table(testGen401$V7)

gen401 <- mean(testGen401$V7)


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

test401$V7[60] <- 1.359115

t.test(testGen401$V7, test401$V7, alternative = "two.sided", var.equal = F)





### 472
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

testGen472 <- perGen472[,-1]
testGen472 <- unique(testGen472)

table(testGen472$V7)

gen472 <- mean(testGen472$V7)


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

test472$V7[60] <- 3.390179

t.test(testGen472$V7, test472$V7, alternative = "two.sided", var.equal = F)




### 475
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

testGen475 <- perGen475[,-1]
testGen475 <- unique(testGen475)

table(testGen475$V7)

gen475 <- mean(testGen475$V7)


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

test475$V7[1] <- 1.961728

t.test(testGen475$V7, test475$V7, alternative = "two.sided", var.equal = F)




### 701
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

testGen701 <- perGen701[,-1]
testGen701 <- unique(testGen701)

table(testGen701$V7)

gen701 <- mean(testGen701$V7)


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

test701$V7[1] <- 3.272535

t.test(testGen701$V7, test701$V7, alternative = "two.sided", var.equal = F)





## Combine the generator and general population means into one dataframe each
genMean <- c(gen101, gen103, gen111, gen113, gen114, gen119, gen129, gen161, gen167, gen353, gen355, gen401, gen472, gen475, gen701)
allMean <- c(all101, all103, all111, all113, all114, all119, all129, all161, all167, all353, all355, all401, all472, all475, all701)

## combine the two dataframes into one
means <- rbind(genMean, allMean)

## write to csv file
write.csv(means, file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Ref Codes/Hunting/mean_years_per_license1.csv")

write.csv(boughtSamp, file = "C:/Users/wxc36638/Desktop/Projects/License Sales/Ref Codes/Hunting/sample1.csv")


