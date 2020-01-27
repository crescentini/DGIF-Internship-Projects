rm(list=ls())


## The survey question and year corresponding to each plot is in the ggplot function call.


##### 2013-2014 #####

survey1314 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Small Game/HS2013-14_OTH.csv")

#### Age ####
table(survey1314$Q75)

ggplot(survey1314, aes(survey1314$Q75)) + geom_bar() + labs(x = "Age", y = "Count", title = "Age of Respondents") +
  theme(text = element_text(size = 15))

summary(survey1314$Q75)

#### Gender ####
table(survey1314$Q76)

ggplot(survey1314, aes(factor(survey1314$Q76, levels = c("1", "0"), labels = c("Male", "Female")))) + geom_bar() + labs(x = "Gender", y = "Count", title = "Gender of Respondents") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))


#### Region ####
table(survey1314$Region)

ggplot(survey1314, aes(factor(survey1314$Region, levels = c("1", "2", "3", "4", "5")))) + geom_bar() + labs(x = "Region", y = "Count", title = "Region of Respondents") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))


#### Level of Importance for Hunting ####

### Rabbit
ggplot(survey1314, aes(factor(survey1314$Q22E, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Rabbit") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,338))

### Squirrel
ggplot(survey1314, aes(factor(survey1314$Q22F, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Squirrel") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,338))

### Quail
ggplot(survey1314, aes(factor(survey1314$Q22G, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Quail") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,338))

mean(survey1314$Q22G, na.rm = T)


#### Describe Interest in Quail Hunting ####
table(survey1314$Q65)

ggplot(survey1314, aes(factor(survey1314$Q65, levels = c("1", "2", "3", "4")))) + geom_bar() +
  labs(x = "Interest Level", y = "Count", title = "Describe Interest in Quail Hunting") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### Close Season for Quail West of Blue Ridge ####
table(survey1314$Q66)

ggplot(survey1314, aes(factor(survey1314$Q66, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Support Rating", y = "Count", title = "Support for Closing Quail Season", subtitle = "West of Blue Ridge") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### Places Where Quail were Hunted ####
table(survey1314$Q67A)

ggplot(survey1314, aes(factor(survey1314$Q67A, levels = c("1")))) + geom_bar() + 
  labs(x = "Support Rating", y = "Count", title = "Description of Where Quail was Hunted", subtitle = "Areas with Wild Quail Only") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

ggplot(survey1314, aes(factor(survey1314$Q67B, levels = c("1")))) + geom_bar() + 
  labs(x = "Support Rating", y = "Count", title = "Description of Where Quail was Hunted", subtitle = "Areas with Some Form of Preseason Release") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

ggplot(survey1314, aes(factor(survey1314$Q67C, levels = c("1")))) + geom_bar() + 
  labs(x = "Support Rating", y = "Count", title = "Description of Where Quail was Hunted", subtitle = "Areas Where Birds Released Same Day or Shortly Before") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


qal1314 <- data.frame(group = c("Wild Quail Only", "Preseason Release", "Released Day Of", "NA"), value = c(166,18,31,714))
qal1314$group <- factor(qal1314$group, levels = c("Wild Quail Only", "Preseason Release", "Released Day Of", "NA"))

ggplot(qal1314, aes(qal1314$group, qal1314$value)) + geom_bar(stat = "identity") +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Description of Where Quail was Hunted")



#### Money Spend on Upland Game Birds ####
table(survey1314$Q68)


#### VA Quail Recovery Initiative ####
table(survey1314$Q69)

ggplot(survey1314, aes(factor(survey1314$Q69, levels = c("0", "1")))) + geom_bar() + 
  labs(x = "Response", y = "Count", title = "Awareness of Quail Recovery Initiative in VA") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)



#### Number of Days Squirrel Hunting####

### September
table(survey1314$Q70A)

ggplot(survey1314, aes(factor(survey1314$Q70A, levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25")))) + geom_bar() + 
  labs(x = "Number of Days", y = "Count", title = "Number of Days Spent Hunting Squirrel", subtitle = "September") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


### October
table(survey1314$Q70B)

ggplot(survey1314, aes(factor(survey1314$Q70B, levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25")))) + geom_bar() + 
  labs(x = "Number of Days", y = "Count", title = "Number of Days Spent Hunting Squirrel", subtitle = "October") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


### November
table(survey1314$Q70C)

ggplot(survey1314, aes(factor(survey1314$Q70C, levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25")))) + geom_bar() + 
  labs(x = "Number of Days", y = "Count", title = "Number of Days Spent Hunting Squirrel", subtitle = "November") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


### December
table(survey1314$Q70D)

ggplot(survey1314, aes(factor(survey1314$Q70D, levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25")))) + geom_bar() + 
  labs(x = "Number of Days", y = "Count", title = "Number of Days Spent Hunting Squirrel", subtitle = "December") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


### January
table(survey1314$Q70E)

ggplot(survey1314, aes(factor(survey1314$Q70E, levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25")))) + geom_bar() + 
  labs(x = "Number of Days", y = "Count", title = "Number of Days Spent Hunting Squirrel", subtitle = "January") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


### February
table(survey1314$Q70F)

ggplot(survey1314, aes(factor(survey1314$Q70F, levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25")))) + geom_bar() + 
  labs(x = "Number of Days", y = "Count", title = "Number of Days Spent Hunting Squirrel", subtitle = "February") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### Rate Experience of Spring Squirrel Season ####
table(survey1314$Q71)

ggplot(survey1314, aes(factor(survey1314$Q71, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Rating", y = "Count", title = "Quality of Spring Squirrel Season") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### Support for Spring Squirrel Season ####
table(survey1314$Q72)

ggplot(survey1314, aes(factor(survey1314$Q72, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Support Level", y = "Count", title = "Support for Spring Squirrel Season") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)



#### Number of Days Spent Hunting Rabbit ####

### November
table(survey1314$Q73A)

ggplot(survey1314, aes(factor(survey1314$Q73A, levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20")))) + geom_bar() + 
  labs(x = "Number of Days", y = "Count", title = "Number of Days Spent Hunting Rabbit", subtitle = "November") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### December
table(survey1314$Q73B)

ggplot(survey1314, aes(factor(survey1314$Q73B, levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30")))) + geom_bar() + 
  labs(x = "Number of Days", y = "Count", title = "Number of Days Spent Hunting Rabbit", subtitle = "December") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### January
table(survey1314$Q73C)

ggplot(survey1314, aes(factor(survey1314$Q73C, levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25")))) + geom_bar() + 
  labs(x = "Number of Days", y = "Count", title = "Number of Days Spent Hunting Rabbit", subtitle = "January") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### February
table(survey1314$Q73D)

ggplot(survey1314, aes(factor(survey1314$Q73D, levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25")))) + geom_bar() + 
  labs(x = "Number of Days", y = "Count", title = "Number of Days Spent Hunting Rabbit", subtitle = "February") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


######################################################################################################################################################################################


##### 2011-2012 #####

survey1112 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Small Game/HS2011-12_OTH.csv")


#### Age ####
table(survey1112$Q63)

ggplot(survey1112, aes(survey1112$Q63)) + geom_bar() + labs(x = "Age", y = "Count", title = "Age of Respondents") +
  theme(text = element_text(size = 15))

age <- subset(survey1112, survey1112$Q63 > 15)
summary(age$Q63)

#### Gender ####
table(survey1112$Q64)

ggplot(survey1112, aes(factor(survey1112$Q64, levels = c("1", "2"), labels = c("Male", "Female")))) + geom_bar() + labs(x = "Gender", y = "Count", title = "Gender of Respondents") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))



#### Level of Importance for Hunting ####

### Rabbit
ggplot(survey1112, aes(factor(survey1112$Q19e, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Rabbit") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,685))

### Squirrel
ggplot(survey1112, aes(factor(survey1112$Q19f, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Squirrel") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,685))

### Quail
ggplot(survey1112, aes(factor(survey1112$Q19g, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Quail") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

mean(survey1112$Q19g, na.rm = T)

#### Describe Interest in Quail Hunting ####
table(survey1112$Q54)

ggplot(survey1112, aes(factor(survey1112$Q54, levels = c("1", "2", "3", "4")))) + geom_bar() +
  labs(x = "Interest Level", y = "Count", title = "Describe Interest in Quail Hunting") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### Close Season for Quail West of Blue Ridge ####
table(survey1112$Q56)

ggplot(survey1112, aes(factor(survey1112$Q56, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Support Rating", y = "Count", title = "Support for Closing Quail Season", subtitle = "West of Blue Ridge") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### Places Where Quail was Hunted ####
table(survey1112$Q58a)

ggplot(survey1112, aes(factor(survey1112$Q58a, levels = c("1")))) + geom_bar() + 
  labs(x = "Support Rating", y = "Count", title = "Description of Where Quail was Hunted", subtitle = "Areas with Wild Quail Only") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

ggplot(survey1112, aes(factor(survey1112$Q58b, levels = c("1")))) + geom_bar() + 
  labs(x = "Support Rating", y = "Count", title = "Description of Where Quail was Hunted", subtitle = "Areas with Some Form of Preseason Release") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

ggplot(survey1112, aes(factor(survey1112$Q58c, levels = c("1")))) + geom_bar() + 
  labs(x = "Support Rating", y = "Count", title = "Description of Where Quail was Hunted", subtitle = "Areas Where Birds Released Same Day or Shortly Before") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


qal1112 <- data.frame(group = c("Wild Quail Only", "Preseason Release", "Released Day Of", "NA"), value = c(96,24,37,1849))
qal1112$group <- factor(qal1112$group, levels = c("Wild Quail Only", "Preseason Release", "Released Day Of", "NA"))

ggplot(qal1112, aes(qal1112$group, qal1112$value)) + geom_bar(stat = "identity") +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Description of Where Quail was Hunted")


#### Money Spend on Upland Game Birds ####
table(survey1112$Q59)


#### VA Quail Recovery Initiative ####
table(survey1112$Q57)

ggplot(survey1112, aes(factor(survey1112$Q57, levels = c("1", "2")))) + geom_bar() + 
  labs(x = "Response", y = "Count", title = "Awareness of Quail Recovery Initiative in VA") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### Rate Experience of Spring Squirrel Season ####
table(survey1112$Q60)

ggplot(survey1112, aes(factor(survey1112$Q60, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Rating", y = "Count", title = "Experience of Spring Squirrel Season") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### Support for Spring Squirrel Season ####
table(survey1112$Q61)

ggplot(survey1112, aes(factor(survey1112$Q61, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Support Level", y = "Count", title = "Support for Spring Squirrel Season") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)



################################################################################################################################################################


##### 2009-2010 #####

survey0910 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Small Game/HS2009-10_OTH.csv", header = T)


#### Age ####
table(survey0910$Q35)

ggplot(survey0910, aes(survey0910$Q35)) + geom_bar() + labs(x = "Age", y = "Count", title = "Age of Respondents") +
  theme(text = element_text(size = 15))

summary(survey0910$Q35)

#### Gender ####
table(survey0910$Q36)

ggplot(survey0910, aes(factor(survey0910$Q36, levels = c("1", "2"), labels = c("Male", "Female")))) + geom_bar() + labs(x = "Gender", y = "Count", title = "Gender of Respondents") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))


#### Level of Importance for Hunting ####

### Rabbit
ggplot(survey0910, aes(factor(survey0910$Q18e, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Rabbit") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,615))

### Squirrel
ggplot(survey0910, aes(factor(survey0910$Q18f, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Squirrel") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,615))

### Quail
ggplot(survey0910, aes(factor(survey0910$Q18g, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Quail") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

mean(survey0910$Q18g, na.rm = T)

################################################################################################################################################################


##### 2008-2009 #####

survey0809 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Small Game/HS2008-09_OTH.csv", header = T)


#### Age ####
table(survey0809$Q59)

ggplot(survey0809, aes(survey0809$Q59)) + geom_bar() + labs(x = "Age", y = "Count", title = "Age of Respondents") +
  theme(text = element_text(size = 15))

summary(survey0809$Q59)

#### Gender ####
table(survey0809$Q60)

ggplot(survey0809, aes(factor(survey0809$Q60, levels = c("1", "2"), labels = c("Male", "Female")))) + geom_bar() + labs(x = "Gender", y = "Count", title = "Gender of Respondents") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))


#### Level of Importance for Hunting ####

### Rabbit
ggplot(survey0809, aes(factor(survey0809$Q20e, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Rabbit") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,776))

### Gray Squirrel
ggplot(survey0809, aes(factor(survey0809$Q20f, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Gray Squirrel") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,776))

### Fox Squirrel
ggplot(survey0809, aes(factor(survey0809$Q20g, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Fox Squirrel") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,776))

### Grouse
ggplot(survey0809, aes(factor(survey0809$Q20h, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Grouse") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,776))

### Quail
ggplot(survey0809, aes(factor(survey0809$Q20i, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Quail") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,776))

### Woodcock
ggplot(survey0809, aes(factor(survey0809$Q20j, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Woodcock") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

mean(survey0809$Q20j, na.rm = T)

#### 2008 June Squirrel Season ####
table(survey0809$Q23)

ggplot(survey0809, aes(factor(survey0809$Q23, levels = c("1", "2")))) + geom_bar() + 
  labs(x = "Response", y = "Count", title = "Hunt During June Squirrel Season") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### Harvest a Squirrel? # might be wrong field
table(survey0809$Field48)

ggplot(survey0809, aes(factor(survey0809$Field48, levels = c("1", "2")))) + geom_bar() + 
  labs(x = "Response", y = "Count", title = "Hunt During June Squirrel Season") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### Population Ratings on National Forest Lands ####

### Grouse
table(survey0809$Q32e)

ggplot(survey0809, aes(factor(survey0809$Q32e, levels = c("1", "2", "3", "4", "5", "6", "7", "8")))) + geom_bar() + 
  labs(x = "Rating", y = "Count", title = "Wildlife Pop. Rating on Nat. Forest Lands", subtitle = "Grouse") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### Squirrel
table(survey0809$Q32f)

ggplot(survey0809, aes(factor(survey0809$Q32f, levels = c("1", "2", "3", "4", "5", "6", "7", "8")))) + geom_bar() + 
  labs(x = "Rating", y = "Count", title = "Wildlife Pop. Rating on Nat. Forest Lands", subtitle = "Squirrel") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,955))


################################################################################################################################################################


##### 2007-2008 #####

survey0708 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Small Game/HS2007-08_OTH.csv", header = T)

#### Age ####
table(survey0708$Q58)

ggplot(survey0708, aes(survey0708$Q58)) + geom_bar() + labs(x = "Age", y = "Count", title = "Age of Respondents") +
  theme(text = element_text(size = 15))

summary(survey0708$Q58)

#### Gender ####
table(survey0708$Q59)

ggplot(survey0708, aes(factor(survey0708$Q59, levels = c("1", "2")))) + geom_bar() + labs(x = "Gender", y = "Count", title = "Gender of Respondents") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15))


#### Level of Importance for Hunting ####

### Rabbit
ggplot(survey0708, aes(factor(survey0708$Q19e, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Rabbit") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,600))

### Squirrel
ggplot(survey0708, aes(factor(survey0708$Q19f, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Squirrel") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,600))

### Quail
ggplot(survey0708, aes(factor(survey0708$Q19g, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Quail") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,600))

mean(survey0708$Q19g, na.rm = T)

#### With/Without Dogs ####

### Rabbit
table(survey0708$Q21d1)

ggplot(as.data.frame(survey0708$Q21d1), aes(factor(survey0708$Q21d1, levels = "1"))) + geom_bar() + 
  labs(x = "Response", y = "Count", title = "Hunt Rabbit With Dogs?") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


table(survey0708$Q21d2)

ggplot(as.data.frame(survey0708$Q21d2), aes(factor(survey0708$Q21d2, levels = "1"))) + geom_bar() + 
  labs(x = "Response", y = "Count", title = "Hunt Rabbit Without Dogs?") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


dogRab0708 <- data.frame(group = c("With Dogs", "Without Dogs"), value = c(328, 248))
dogRab0708$group <- factor(dogRab0708$group, levels = c("With Dogs", "Without Dogs"))

ggplot(dogRab0708, aes(dogRab0708$group, dogRab0708$value)) + geom_bar(stat = "identity") +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Use of Dogs when Hunting Rabbit") + lims(y = c(0,555))


### Squirrel
table(survey0708$Q21g1)

ggplot(as.data.frame(survey0708$Q21g1), aes(factor(survey0708$Q21g1, levels = "1"))) + geom_bar() + 
  labs(x = "Response", y = "Count", title = "Hunt Squirrel With Dogs?") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

table(survey0708$Q21g2)

ggplot(as.data.frame(survey0708$Q21g2), aes(factor(survey0708$Q21g2, levels = "1"))) + geom_bar() + 
  labs(x = "Response", y = "Count", title = "Hunt Squirrel Without Dogs?") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


dogSqr0708 <- data.frame(group = c("With Dogs", "Without Dogs"), value = c(80, 555))
dogSqr0708$group <- factor(dogSqr0708$group, levels = c("With Dogs", "Without Dogs"))

ggplot(dogSqr0708, aes(dogSqr0708$group, dogSqr0708$value)) + geom_bar(stat = "identity") +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Use of Dogs when Hunting Squirrel") + lims(y = c(0,555))

### Quail
table(survey0708$Q21f1)

ggplot(as.data.frame(survey0708$Q21f1), aes(factor(survey0708$Q21f1, levels = "1"))) + geom_bar() + 
  labs(x = "Response", y = "Count", title = "Hunt Quail With Dogs?") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

table(survey0708$Q21f2)

ggplot(as.data.frame(survey0708$Q21f2), aes(factor(survey0708$Q21f2, levels = "1"))) + geom_bar() + 
  labs(x = "Response", y = "Count", title = "Hunt Quail Without Dogs?") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


dogQal0708 <- data.frame(group = c("With Dogs", "Without Dogs"), value = c(120, 197))
dogQal0708$group <- factor(dogQal0708$group, levels = c("With Dogs", "Without Dogs"))

ggplot(dogQal0708, aes(dogQal0708$group, dogQal0708$value)) + geom_bar(stat = "identity") +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Use of Dogs when Hunting Quail") + lims(y = c(0,555))


### Grouse
table(survey0708$Q21i1)

ggplot(as.data.frame(survey0708$Q21i1), aes(factor(survey0708$Q21i1, levels = "1"))) + geom_bar() + 
  labs(x = "Response", y = "Count", title = "Hunt Rabbit With Dogs?") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

table(survey0708$Q21i2)

ggplot(as.data.frame(survey0708$Q21i2), aes(factor(survey0708$Q21i2, levels = "1"))) + geom_bar() + 
  labs(x = "Response", y = "Count", title = "Hunt Rabbit Without Dogs?") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


dogGro0708 <- data.frame(group = c("With Dogs", "Without Dogs"), value = c(47, 225))
dogGro0708$group <- factor(dogGro0708$group, levels = c("With Dogs", "Without Dogs"))

ggplot(dogGro0708, aes(dogGro0708$group, dogGro0708$value)) + geom_bar(stat = "identity") +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Use of Dogs when Hunting Grouse") + lims(y = c(0,555))


#### How Many Days With Dogs? ####

### Rabbit
table(survey0708$Q22e1)

ggplot(as.data.frame(survey0708$Q22e1), aes(survey0708$Q22e1)) + geom_bar() + 
  labs(x = "Response", y = "Count", title = "How Many Days Hunting Rabbits With Dogs", subtitle = "Last Twelve Months") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

#### How Many Days Without Dogs ####

### Rabbit
table(survey0708$Q22e2)

ggplot(as.data.frame(survey0708$Q22e2), aes(survey0708$Q22e2)) + geom_bar() + 
  labs(x = "Response", y = "Count", title = "How Many Days Hunting Rabbits Without Dogs", subtitle = "Last Twelve Months") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### What do you Think Quail Populations are Doing? ####
table(survey0708$Q26)

ggplot(survey0708, aes(factor(survey0708$Q26, levels = c("1", "2", "3", "4")))) + geom_bar() +
  labs(x = "Response", y = "Count", title = "What are Quail Populations Doing?") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### Did you Hunt Quail ####
table(survey0708$Q27)

ggplot(survey0708, aes(factor(survey0708$Q27, levels = c("1", "2"), labels = c("Yes", "No")))) + geom_bar() +
  labs(x = "Response", y = "Count", title = "Quail Hunt in 2007-08 Season") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### Describe Quail Hunting Activity ####
table(survey0708$Q28)

ggplot(survey0708, aes(factor(survey0708$Q28, levels = c("1", "2", "3", "4")))) + geom_bar() +
  labs(x = "Activity Level", y = "Count", title = "Quail Hunting Activity in VA") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### No Longer Hunt Quail ####
table(survey0708$Q28a1)

### Not Enough Time
ggplot(survey0708, aes(factor(survey0708$Q28a1, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() +
  labs(x = "Rating", y = "Count", title = "Why Stop Hunting Quail?", subtitle = "Not Enough Time") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### Populations too Low
ggplot(survey0708, aes(factor(survey0708$Q28a2, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() +
  labs(x = "Rating", y = "Count", title = "Why Stop Hunting Quail?", subtitle = "Populations Too Low") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,1514))

mean(survey0708$Q28a4, na.rm = T)

### No Place to Hunt
ggplot(survey0708, aes(factor(survey0708$Q28a3, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() +
  labs(x = "Rating", y = "Count", title = "Why Stop Hunting Quail?", subtitle = "No Place to Hunt") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,1514))

### Rather hunt other species
ggplot(survey0708, aes(factor(survey0708$Q28a4, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() +
  labs(x = "Rating", y = "Count", title = "Why Stop Hunting Quail?", subtitle = "Rather Hunt Other Species") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,1514))


#### Willing to Pay for Quail Stamp? ####
table(survey0708$Q29)

ggplot(survey0708, aes(factor(survey0708$Q29, levels = c("1", "2"), labels = c("Yes", "No")))) + geom_bar() +
  labs(x = "Response", y = "Count", title = "Willing to Pay for Quail Stamp?") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### Rate Quail Population Management ####

ggplot(survey0708, aes(factor(survey0708$Q30, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() +
  labs(x = "Rating", y = "Count", title = "Rate VDGIF's Management of Quail Pops.") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### What Kind of Quail Hunter ####
table(survey0708$Q31)

ggplot(survey0708, aes(factor(survey0708$Q31, levels = c("1", "2", "3", "4")))) + geom_bar() +
  labs(x = "Response", y = "Count", title = "What Kind of Quail Hunter?") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### Quality of Quail Hunting ####

ggplot(survey0708, aes(factor(survey0708$Q32, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() +
  labs(x = "Quality Level", y = "Count", title = "Quality of Quail Hunting") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### Hunt Quail on Shooting Preserve ####

ggplot(survey0708, aes(factor(survey0708$Q33, levels = c("1", "2"), labels = c("Yes", "No")))) + geom_bar() +
  labs(x = "Response", y = "Count", title = "Hunt Quail on Shooting Preserve?") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### Indicate Emphasis DGIF should put ####

### Quail Pops on WMA
ggplot(survey0708, aes(factor(survey0708$Q34a, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() +
  labs(x = "Rating", y = "Count", title = "Emphasis DGIF Should Put", subtitle = "Quail Populations on WMAs") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### Tech. Assistance to landowners interested in Quail Management
ggplot(survey0708, aes(factor(survey0708$Q34b, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() +
  labs(x = "Rating", y = "Count", title = "Emphasis DGIF Should Put", subtitle = "Assistance to Landowners for Quail Management") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### Finding Private Lands With Quail Where Hunting is Permitted
ggplot(survey0708, aes(factor(survey0708$Q34c, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() +
  labs(x = "Rating", y = "Count", title = "Emphasis DGIF Should Put", subtitle = "Finding Private Lands for Hunting") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### Quail Habitat Development on Commercial Lands
ggplot(survey0708, aes(factor(survey0708$Q34d, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() +
  labs(x = "Rating", y = "Count", title = "Emphasis DGIF Should Put", subtitle = "Quail Habitat Development on Commercial Lands") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### Info on Shooting Preserves for Quail Hunting
ggplot(survey0708, aes(factor(survey0708$Q34e, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() +
  labs(x = "Rating", y = "Count", title = "Emphasis DGIF Should Put", subtitle = "Info on Shooting Preserves for Quail Hunters") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### Equipment for Landowners Necessary for Managing Habitat
ggplot(survey0708, aes(factor(survey0708$Q34f, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() +
  labs(x = "Rating", y = "Count", title = "Emphasis DGIF Should Put", subtitle = "Equipment for Landowners to Manage Habitat") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### Assistance Such as Seed to Landowners
ggplot(survey0708, aes(factor(survey0708$Q34g, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() +
  labs(x = "Rating", y = "Count", title = "Emphasis DGIF Should Put", subtitle = "Assistance to Landowners Such as Seed") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


############################################################################################################################################

##### 2006-07 #####

survey0607 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Small Game/HS2006-07_OTH.csv", header = T)

#### Age ####
table(survey0607$Q60)

ggplot(survey0607, aes(survey0607$Q60)) + geom_bar() + labs(x = "Age", y = "Count", title = "Age of Respondents") +
  theme(text = element_text(size = 15))

summary(survey0607$Q60)

#### Gender ####
table(survey0607$Q61)

ggplot(survey0607, aes(factor(survey0607$Q61, levels = c("1", "2"), labels = c("Male", "Female")))) + geom_bar() + labs(x = "Gender", y = "Count", title = "Gender of Respondents") +
  theme(text = element_text(size = 15)) + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25)



#### Level of Importance for Hunting ####

### Rabbit
ggplot(survey0607, aes(factor(survey0607$Q19e, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Rabbit") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### Squirrel
ggplot(survey0607, aes(factor(survey0607$Q19f, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Squirrel") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,404))

mean(survey0607$Q19f, na.rm = T)

#### Dog Hunting ####

ggplot(survey0607, aes(factor(survey0607$Q21a, levels = c("1")))) + geom_bar() +
  labs(x = "Response", y = "Count", title = "Hunt with a Dog?") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### Rabbit
ggplot(survey0607, aes(factor(survey0607$Q21d, levels = c("1")))) + geom_bar() +
  labs(x = "Response", y = "Count", title = "Hunt Rabbits with Dogs?") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### Squirrel
ggplot(survey0607, aes(factor(survey0607$Q21f, levels = c("1")))) + geom_bar() +
  labs(x = "Response", y = "Count", title = "Hunt Squirrels with Dogs?") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### Quail
ggplot(survey0607, aes(factor(survey0607$Q21e, levels = c("1")))) + geom_bar() +
  labs(x = "Response", y = "Count", title = "Hunt Quail with Dogs?") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


dog0607 <- data.frame(group = c("Any Game", "Rabbit", "Squirrel", "Quail"), value = c(709, 235, 55, 52))
dog0607$group <- factor(dog0607$group, levels = c("Any Game", "Rabbit", "Squirrel", "Quail"))

ggplot(dog0607, aes(dog0607$group, dog0607$value)) + geom_bar(stat = "identity") +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Hunting Game with Dogs")





#### Rate Population on National Forest Land ####

### Squirrel
ggplot(survey0607, aes(factor(survey0607$Q42f, levels = c("1", "2", "3", "4", "5", "6", "7", "8")))) + geom_bar() + 
  labs(x = "Population Rating", y = "Count", title = "Population Rating on Nat. Forest Land", subtitle = "Squirrel") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,882))

### Grouse
ggplot(survey0607, aes(factor(survey0607$Q42e, levels = c("1", "2", "3", "4", "5", "6", "7", "8")))) + geom_bar() + 
  labs(x = "Population Rating", y = "Count", title = "Population Rating on Nat. Forest Land", subtitle = "Grouse") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


##############################################################################################################################

##### 2005-06 #####

survey0506 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Small Game/HS2005-06_OTH.csv", header = T)

#### Age ####
table(survey0506$Q56)

ggplot(survey0506, aes(survey0506$Q56)) + geom_bar() + labs(x = "Age", y = "Count", title = "Age of Respondents")

summary(survey0506$Q56)

#### Gender ####
table(survey0506$Q57)

ggplot(survey0506, aes(factor(survey0506$Q57, levels = c("1", "2"), labels = c("Male", "Female")))) + geom_bar() +
  labs(x = "Gender", y = "Count", title = "Gender of Respondents") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25)


#### Level of Importance for Hunting ####

### Rabbit
ggplot(survey0506, aes(factor(survey0506$Q19e, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Rabbit") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### Squirrel
ggplot(survey0506, aes(factor(survey0506$Q19f, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Squirrel") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,350))

mean(survey0506$Q19f, na.rm = T)

#### Rate DGIF's management ####

### Rabbit
ggplot(survey0506, aes(factor(survey0506$Q20e, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Rating", y = "Count", title = "Rate DGIF's Management of Rabbit") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,419))

mean(survey0506$Q20f, na.rm = T)

### Squirrel
ggplot(survey0506, aes(factor(survey0506$Q20f, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Rating", y = "Count", title = "Rate DGIF's Management of Squirrel") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### Population Rating on Nat Forest Land ####

### Squirrel
ggplot(survey0506, aes(factor(survey0506$Q31e, levels = c("1", "2", "3", "4", "5", "6", "7","8")))) + geom_bar() + 
  labs(x = "Population Rating", y = "Count", title = "Population Rating on Nat. Forest Land", subtitle = "Squirrel") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,599))

mean(survey0506$Q31d, na.rm = T)

### Grouse
ggplot(survey0506, aes(factor(survey0506$Q31d, levels = c("1", "2", "3", "4", "5", "6", "7","8")))) + geom_bar() + 
  labs(x = "Population Rating", y = "Count", title = "Population Rating on Nat. Forest Land", subtitle = "Grouse") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


##############################################################################################################################


##### 2004-05 #####

survey0405 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Small Game/HS2004-05_OTH.csv", header = T)

#### Age ####
table(survey0405$Q55)

ggplot(survey0405, aes(survey0405$Q55)) + geom_bar() + labs(x = "Age", y = "Count", title = "Age of Respondents")

summary(survey0405$Q55)

#### Gender ####
table(survey0405$Q56)

ggplot(survey0405, aes(factor(survey0405$Q56, levels = c("1", "2"), labels = c("Male","Female")))) + geom_bar() +
  labs(x = "Gender", y = "Count", title = "Gender of Respondents") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25)


#### Level of Importance for Hunting ####

### Rabbit
ggplot(survey0405, aes(factor(survey0405$Q19f, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Rabbit") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,456))

### Squirrel
ggplot(survey0405, aes(factor(survey0405$Q19g, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Squirrel") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,456))

### Quail
ggplot(survey0405, aes(factor(survey0405$Q19e, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Quail") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,456))

### Grouse
ggplot(survey0405, aes(factor(survey0405$Q19h, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Level of Importance for Hunting Grouse") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE) + lims(y = c(0,456))

mean(survey0405$Q19h, na.rm = T)

#### Squirrel Season First Two Weeks of June ####

### Support
table(survey0405$Q20a)
ggplot(survey0405, aes(factor(survey0405$Q20a, levels = c("1", "2", "3")))) + geom_bar() + 
  labs(x = "Support Rating", y = "Count", title = "Support Squirrel Season First Two Weeks of June") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

### Indicate Level of Agreement With

## Too Hot
ggplot(survey0405, aes(factor(survey0405$Q20b, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Squirrel Season First Two Weeks of June", subtitle = "Too Hot to Hunt Squirrels") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

## Pregnant Squirrels Would be Killed
ggplot(survey0405, aes(factor(survey0405$Q20c, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Squirrel Season First Two Weeks of June", subtitle = "Pregnant Squirrels Would be Killed") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

## Numbers Reduced
ggplot(survey0405, aes(factor(survey0405$Q20d, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Squirrel Season First Two Weeks of June", subtitle = "Numbers Would be Reduced") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

## Other Species Would be Disturbed
ggplot(survey0405, aes(factor(survey0405$Q20e, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Squirrel Season First Two Weeks of June", subtitle = "Distubance to Other Species") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)

## Poaching Would Increase
ggplot(survey0405, aes(factor(survey0405$Q20f, levels = c("1", "2", "3", "4", "5", "6", "7")))) + geom_bar() + 
  labs(x = "Importance Rating", y = "Count", title = "Squirrel Season First Two Weeks of June", subtitle = "Poaching Would Increase") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


#### Rabbit Season ####

### Did you Hunt Rabbits Feb 1 - Feb 14
table(survey0405$Q21a)
ggplot(survey0405, aes(factor(survey0405$Q20a, levels = c("1", "2")))) + geom_bar() + 
  labs(x = "Support Rating", y = "Count", title = "Hunt Rabbits Between Feb. 1 and Feb. 14") + 
  geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
  theme(text = element_text(size = 15)) + scale_x_discrete(drop = FALSE)


###########################################################################################################################################

#### Importance Line Graph ####

importance <- matrix(data = c("Rabbit",3.41,3.26,3.50,3.53,3.53,3.17,NA,3.33,NA,3.32,"Squirrel",3.82,3.57,3.77,3.74,3.73,3.46,NA,3.51,NA,3.63,"Quail",2.64,NA,NA,2.86,2.88,2.54,NA,2.58,NA,2.57,"Grouse",2.69,NA,NA,NA,2.83,NA,NA,NA,NA,NA,"Woodcock",NA,NA,NA,NA,2.26,NA,NA,NA,NA,NA), nrow = 5, byrow = T)
importance <- as.data.frame(importance)
colnames(importance) <- c("Animal","04-05","05-06","06-07","07-08","08-09","09-10","10-11","11-12","12-13","13-14")

importance <- melt(importance, id="Animal")
importance$value <- as.numeric(importance$value)

ggplot(importance, aes(x = variable, y = value, group = Animal, col = Animal)) + geom_point(size = 2.5) + geom_line(size = 1.5) + lims(y = c(0,5)) +
  geom_segment(aes(x = "04-05", y = 2.69, xend = "08-09", yend = 2.83), linetype = "dashed", col = "#E41A1C", size = 1.25) +
  geom_segment(aes(x = "04-05", y = 2.64, xend = "07-08", yend = 2.86), linetype = "dashed", col = "#377EB8", size = 1.25) +
  geom_segment(aes(x = "09-10", y = 3.17, xend = "11-12", yend = 3.33), linetype = "dashed", col = "#4DAF4A", size = 1.25) +
  geom_segment(aes(x = "09-10", y = 3.46, xend = "11-12", yend = 3.51), linetype = "dashed", col = "#984EA3", size = 1.25) +
  geom_segment(aes(x = "09-10", y = 2.54, xend = "11-12", yend = 2.58), linetype = "dashed", col = "#377EB8", size = 1.25) +
  geom_segment(aes(x = "11-12", y = 3.33, xend = "13-14", yend = 3.32), linetype = "dashed", col = "#4DAF4A", size = 1.25) +
  geom_segment(aes(x = "11-12", y = 3.51, xend = "13-14", yend = 3.63), linetype = "dashed", col = "#984EA3", size = 1.25) +
  geom_segment(aes(x = "11-12", y = 2.58, xend = "13-14", yend = 2.57), linetype = "dashed", col = "#377EB8", size = 1.25) +
  scale_color_brewer(palette = "Set1") + labs(x = "Year", y = "Importance Rating", title = "Level of Importance for Hunting") +
  theme(text = element_text(size = 15))











