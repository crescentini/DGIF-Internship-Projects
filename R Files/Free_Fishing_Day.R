# Clears Environment
rm(list=ls())

# For Plotting
library(ggplot2)


### How did people hear about the event barcharts ####

# Create dataframe with values and groups (ggplot requires dataframes)
deeprun <- data.frame(value = c(7, 3, 1), group = c("Did Not Hear", "Social Media", "DGIF Email"))

# Set the groups as factors so they stay in the order specified
deeprun$group <- factor(deeprun$group, levels = c("Did Not Hear", "Social Media", "DGIF Email"))

# Plot the barchart
ggplot(deeprun, aes(x = deeprun$group, y = deeprun$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  # manually sets the colors, name stays blank since not using legend
  scale_fill_manual(name = "",
            values = c("#4D4D4D", "#F15854", "#FAA43A"))  +
  # sets value numbers above each bar
  geom_text(aes(label = value),
            position = position_dodge(width = 1),
            vjust = -.25) +
  # sets x-axis labels at 45 degree angle, increase size of all text, remove legend
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 1.0,
      hjust = 1.05
    ),
    text = element_text(size = 15),
    legend.position = "none"
  ) +
  # set graph labels
  labs(x = "", y = "Count", title = "If and How People Heard About the Event")


## Repeat for each location

dorey <- data.frame(group = c("Did Not Hear", "Friends/Family", "Social Media", "Radio", "Website",
                                "DGIF Email", "Other DGIF", "Other"), value = c(31, 13, 7, 1, 1, 1, 3, 2))
dorey$group <- factor(dorey$group, levels = c("Did Not Hear", "Friends/Family", "Social Media", "Radio", "Website",
                                              "DGIF Email", "Other DGIF", "Other"))

ggplot(dorey, aes(dorey$group, dorey$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0", "#F15854", "#B276B2", "#DECF3F"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "If and How People Heard About the Event")



threelakes <- data.frame(group = c("Did Not Hear", "Friends/Family", "Social Media", "Radio", "Website",
                              "DGIF Email", "Other DGIF", "Other"), value = c(19, 5, 5, 3, 7, 2, 3, 4))
threelakes$group <- factor(threelakes$group, levels = c("Did Not Hear", "Friends/Family", "Social Media", "Radio", "Website",
                                                        "DGIF Email", "Other DGIF", "Other"))

ggplot(threelakes, aes(threelakes$group, threelakes$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0", "#F15854", "#B276B2", "#DECF3F"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "If and How People Heard About the Event")



lynch <- data.frame(group = c("Did Not Hear", "Friends/Family", "Social Media",
                                   "DGIF Email", "Other DGIF"), value = c(5, 1, 1, 1, 1))
lynch$group <- factor(lynch$group, levels = c("Did Not Hear", "Friends/Family", "Social Media", "DGIF Email", "Other DGIF"))

ggplot(lynch, aes(lynch$group, lynch$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#5DA5DA", "#FAA43A", "#F15854", "#B276B2"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "If and How People Heard About the Event")



wasena <- data.frame(group = c("Did Not Hear", "Friends/Family", "Website"), value = c(3, 1, 1))
wasena$group <- factor(wasena$group, levels = c("Did Not Hear", "Friends/Family", "Website"))

ggplot(wasena, aes(wasena$group, wasena$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#5DA5DA", "#F17CB0"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "If and How People Heard About the Event")



claytor <- data.frame(group = c("Did Not Hear", "Friends/Family", "Social Media", "Radio", "Website",
                                   "DGIF Email", "Other DGIF", "Other"), value = c(14, 8, 8, 4, 1, 1, 1, 1))
claytor$group <- factor(claytor$group, levels = c("Did Not Hear", "Friends/Family", "Social Media", "Radio", "Website",
                                                  "DGIF Email", "Other DGIF", "Other"))

ggplot(claytor, aes(claytor$group, claytor$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0", "#F15854", "#B276B2", "#DECF3F"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "If and How People Heard About the Event")



hm <- data.frame(group = c("Did Not Hear", "Friends/Family", "Social Media", "Radio", "Website",
                                   "DGIF Email", "Other"), value = c(9, 4, 7, 1, 8, 1, 3))
hm$group <- factor(hm$group, levels = c("Did Not Hear", "Friends/Family", "Social Media", "Radio", "Website",
                                        "DGIF Email", "Other"))

ggplot(hm, aes(hm$group, hm$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0", "#F15854", "#B276B2", "#DECF3F"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "If and How People Heard About the Event")



rr <- data.frame(group = c("Did Not Hear", "Friends/Family", "Social Media", "Radio",
                                   "Other DGIF"), value = c(1, 3, 1, 1, 1))
rr$group <- factor(rr$group, levels = c("Did Not Hear", "Friends/Family", "Social Media", "Radio", "Other DGIF"))

ggplot(rr, aes(rr$group, rr$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#5DA5DA", "#FAA43A", "#60BD68", "#B276B2"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "If and How People Heard About the Event")



lincoln <- data.frame(group = c("Did Not Hear", "Friends/Family", "Social Media", "Website",
                                   "Other DGIF"), value = c(5, 5, 8, 1, 2))
lincoln$group <- factor(lincoln$group, levels = c("Did Not Hear", "Friends/Family", "Social Media", "Website", "Other DGIF"))

ggplot(lincoln, aes(lincoln$group, lincoln$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#5DA5DA", "#FAA43A", "#F17CB0", "#B276B2"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "If and How People Heard About the Event")



wise <- data.frame(group = c("Friends/Family", "Social Media", "Other"), value = c(2, 4, 3))
wise$group <- factor(wise$group, levels = c("Friends/Family", "Social Media", "Other"))

ggplot(wise, aes(wise$group, wise$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#5DA5DA", "#FAA43A", "#DECF3F"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "If and How People Heard About the Event")




burke <- data.frame(group = c("Did Not Hear", "Friends/Family", "Social Media", "Radio",
                                   "DGIF Email", "Other"), value = c(5, 1, 20, 1, 1, 1))
burke$group <- factor(burke$group, levels = c("Did Not Hear", "Friends/Family", "Social Media", "Radio", "DGIF Email", "Other"))

ggplot(burke, aes(burke$group, burke$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#5DA5DA", "#FAA43A", "#60BD68", "#F15854", "#DECF3F"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "If and How People Heard About the Event")



shen <- data.frame(group = c("Did Not Hear", "Friends/Family", "Social Media", "Radio", "Website",
                                   "DGIF Email", "Other DGIF", "Other"), value = c(4, 6, 10, 1, 5, 1, 1, 5))
shen$group <- factor(shen$group, levels = c("Did Not Hear", "Friends/Family", "Social Media", "Radio", "Website",
                                            "DGIF Email", "Other DGIF", "Other"))

ggplot(shen, aes(shen$group, shen$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0", "#F15854", "#B276B2", "#DECF3F"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "If and How People Heard About the Event")



dard <- data.frame(group = c("Did Not Hear", "Friends/Family", "Social Media", "Radio",
                             "DGIF Email", "Other"), value = c(30, 9, 2, 2, 1, 1))
dard$group <- factor(dard$group, levels = c("Did Not Hear", "Friends/Family", "Social Media", "Radio",
                                            "DGIF Email", "Other"))

ggplot(dard, aes(dard$group, dard$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#5DA5DA", "#FAA43A", "#60BD68", "#F15854", "#DECF3F"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "If and How People Heard About the Event")



motts <- data.frame(group = c("Did Not Hear", "Friends/Family", "Social Media", "Website",
                                   "Other DGIF", "Other"), value = c(2, 3, 8, 2, 1, 3))
motts$group <- factor(motts$group, levels = c("Did Not Hear", "Friends/Family", "Social Media", "Website", "Other DGIF", "Other"))

ggplot(motts, aes(motts$group, motts$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#5DA5DA", "#FAA43A", "#F17CB0", "#B276B2", "#DECF3F"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "If and How People Heard About the Event")


## All locations

tot <- data.frame(group = c("Did Not Hear", "Friends/Family", "Social Media", "Radio", "Website",
                  "DGIF Email", "Other DGIF", "Other"), value = c(135, 61, 84, 14, 26, 10, 13, 23))
tot$group <- factor(tot$group, levels = c("Did Not Hear", "Friends/Family", "Social Media", "Radio", "Website",
                                          "DGIF Email", "Other DGIF", "Other"))

ggplot(tot, aes(tot$group, tot$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0", "#F15854", "#B276B2", "#DECF3F"))  +
  geom_text(aes(label = c("135 (40.5%)", "61 (18.3%)", "84 (25.2%)", "14 (4.2%)", "26 (7.8%)", "10 (3.0%)", "13 (3.9%)", "23(6.9%)")), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "If and How People Heard About the Events")



####################################################################################################################
rm(list=ls())


### Who did people come with barcharts ####


library(ggplot2)


# Create dataframe with values and groups
deeprun2 <- data.frame(group = c("Alone", "Friend", "S/O", "Son", "Daughter", "Other Family"),
                       value = c(3, 4, 2, 3, 1, 3))
# Set group as a factor so we get the desired order
deeprun2$group <- factor(deeprun2$group, levels = c("Alone", "Friend", "S/O", "Son", "Daughter", "Other Family"))
# Plot the barchart
ggplot(deeprun2, aes(deeprun2$group, deeprun2$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  # manually select the desired colors
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#407899"))  +
  # put counts on top of each bar
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  # set x-axis labels to 45 degrees, increase size of all text, remove legend
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  # set chart labels
  labs(x = "", y = "Count", title = "Who People Attended the Event With")


## Repeat for all other locations

dorey2 <- data.frame(group = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"),
                       value = c(1, 19, 10, 13, 17, 3, 13))
dorey2$group <- factor(dorey2$group, levels = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"))

ggplot(dorey2, aes(dorey2$group, dorey2$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#9D5BB1", "#407899"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who People Attended the Event With")



tlakes2 <- data.frame(group = c("Alone", "Friend", "S/O", "Son", "Daughter", "Other Family"),
                     value = c(5, 10, 11, 24, 11, 6))
tlakes2$group <- factor(tlakes2$group, levels = c("Alone", "Friend", "S/O", "Son", "Daughter", "Other Family"))

ggplot(tlakes2, aes(tlakes2$group, tlakes2$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#407899"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who People Attended the Event With")



lynch2 <- data.frame(group = c("Alone", "S/O", "Son", "Daughter", "Other Family"),
                     value = c(3, 1, 1, 2, 2))
lynch2$group <- factor(lynch2$group, levels = c("Alone", "S/O", "Son", "Daughter", "Other Family"))

ggplot(lynch2, aes(lynch2$group, lynch2$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#E3B505", "#7A150A", "#FF9EDD", "#407899"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who People Attended the Event With")



wasena2 <- data.frame(group = c("Alone", "S/O", "Daughter"), value = c(4, 1, 1))
wasena2$group <- factor(wasena2$group, levels = c("Alone", "S/O", "Daughter"))

ggplot(wasena2, aes(wasena2$group, wasena2$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#E3B505", "#FF9EDD"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who People Attended the Event With")



claytor2 <- data.frame(group = c("Alone", "Friend", "S/O", "Son", "Daughter", "Other Family"),
                     value = c(4, 11, 10, 10, 4, 12))
claytor2$group <- factor(claytor2$group, levels = c("Alone", "Friend", "S/O", "Son", "Daughter", "Other Family"))

ggplot(claytor2, aes(claytor2$group, claytor2$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#407899"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who People Attended the Event With")



hm2 <- data.frame(group = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"),
                     value = c(2, 4, 3, 9, 5, 3, 13))
hm2$group <- factor(hm2$group, levels = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"))

ggplot(hm2, aes(hm2$group, hm2$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#9D5BB1", "#407899"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who People Attended the Event With")



rr2 <- data.frame(group = c("Friend", "Son", "Daughter", "Other Family"), value = c(4, 1, 2, 2))
rr2$group <- factor(rr2$group, levels = c("Friend", "Son", "Daughter", "Other Family"))

ggplot(rr2, aes(rr2$group, rr2$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#8CD7BF", "#7A150A", "#FF9EDD", "#407899"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who People Attended the Event With")



lincoln2 <- data.frame(group = c("Alone", "Friend", "S/O", "Other Family"), value = c(13, 1, 2, 6))
lincoln2$group <- factor(lincoln2$group, levels = c("Alone", "Friend", "S/O", "Other Family"))

ggplot(lincoln2, aes(lincoln2$group, lincoln2$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#407899"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who People Attended the Event With")



wise2 <- data.frame(group = c("Friend", "S/O", "Daughter", "Other Family"),
                     value = c(2, 3, 2, 3))
wise2$group <- factor(wise2$group, levels = c("Friend", "S/O", "Daughter", "Other Family"))

ggplot(wise2, aes(wise2$group, wise2$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#8CD7BF", "#E3B505", "#FF9EDD", "#407899"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who People Attended the Event With")



burke2 <- data.frame(group = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"),
                     value = c(3, 1, 13, 18, 10, 2, 1))
burke2$group <- factor(burke2$group, levels = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"))

ggplot(burke2, aes(burke2$group, burke2$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#9D5BB1", "#407899"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who People Attended the Event With")



shen2 <- data.frame(group = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"),
                     value = c(1, 5, 9, 12, 7, 1, 7))
shen2$group <- factor(shen2$group, levels = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"))

ggplot(shen2, aes(shen2$group, shen2$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#9D5BB1", "#407899"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who People Attended the Event With")



dard2 <- data.frame(group = c("Alone", "Friend", "S/O", "Son", "Daughter", "Other Family"),
                    value = c(2, 11, 1, 7, 4, 25))
dard2$group <- factor(dard2$group, levels = c("Alone", "Friend", "S/O", "Son", "Daughter", "Other Family"))

ggplot(dard2, aes(dard2$group, dard2$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#407899"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who People Attended the Event With")



motts2 <- data.frame(group = c("Friend", "S/O", "Son", "Daughter", "Other Family"),
                     value = c(3, 4, 12, 6, 1))
motts2$group <- factor(motts2$group, levels = c("Friend", "S/O", "Son", "Daughter", "Other Family"))

ggplot(motts2, aes(motts2$group, motts2$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#407899"))  +
  geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who People Attended the Event With")


## All Locations

tot2 <- data.frame(group = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"),
                     value = c(41, 75, 70, 110, 72, 9, 94))
tot2$group <- factor(tot2$group, levels = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"))

ggplot(tot2, aes(tot2$group, tot2$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#9D5BB1", "#407899"))  +
  geom_text(aes(label = c("41 (12.3%)", "75 (22.5%)", "70 (21.0%)", "110 (33.0%)", "72 (21.6%)", "9 (2.7%)", "94 (28.2)")), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who People Attended the Events With")


######################################################################################################################
rm(list=ls())


### Attendee Zip Code Maps ####


# library for maps; download this first
library(USAboundaries)
# after running first package, it will prompt you to download this one
library(USAboundariesData)
# special features library, needed for two above packages
library(sf)
# for haversine function
library(pracma)


# puts all US zip codes into sf collection
uszipcode <- us_zipcodes()

# read in the survey zips codes separated by location. File path will need to be changed to where you have file
zips1 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/zips1.csv", header = F)
# remove extraneous column
zips1 <- zips1[,-2]

zips2 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/zips2.csv", header = F)
zips2 <- zips2[,-2]

zips3 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/zips3.csv", header = F)
zips3 <- zips3[,-2]

zips5 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/zips5.csv", header = F)
zips5 <- zips5[,-2]

zips6 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/zips6.csv", header = F)
zips6 <- zips6[,-2]

zips7 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/zips7.csv", header = F)
zips7 <- zips7[,-2]

zips8 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/zips8.csv", header = F)
zips8 <- zips8[,-2]

zips9 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/zips9.csv", header = F)
zips9 <- zips9[,-2]

zips10 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/zips10.csv", header = F)
zips10 <- zips10[,-2]

zips11 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/zips11.csv", header = F)
zips11 <- zips11[,-2]

zips12 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/zips12.csv", header = F)
zips12 <- zips12[,-2]

zips13 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/zips13.csv", header = F)
zips13 <- zips13[,-2]

zips14 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/zips14.csv", header = F)
zips14 <- zips14[,-2]

zips15 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/zips15.csv", header = F)
zips15 <- zips15[,-2]

# combine all zip codes into one dataframe
zipstot <- c(zips1, zips2, zips3, zips5, zips6, zips7, zips8, zips9, zips10, zips11, zips12, zips13, zips14, zips15)


## Combine uneven vectors to a dataframe (example)
zips1[12:48] <- 0
x <- as.data.frame(cbind(zips1,zips2))


## Deep Run Park

# Type names of states that you want in map separated by commas
vamap <- us_states(states = c("Virginia"))
# Plot map with title
plot(st_geometry(vamap), main = "Deep Run Park")

# variable initialization
cexVal = 1.0
index1 = 0
coords1 <- matrix(c(0,0), ncol = 2, nrow = length(zips1))
dist1 = 0
# Location of Deep Run Park
drp = c(-77.589685, 37.6253414)

# Loop through all zip codes for this location
for(i in 1:length(zips1)) {
  # Start at size 1.0
  cexVal = 1.0
  # Sizing loop
  for (j in 1:length(zips1)) {
    # If there is more than one of each zip code, make it proportionally larger
    if ((zips1[j] == zips1[i]) && (i != j)) {
      if (cexVal == 1.0) {
        cexVal = 1.2
      }
      else cexVal = cexVal + 0.1
    }
  }
  # Find which entry in uszipcode corresponds to zip code from survey
  index1[i] <- which(uszipcode$zipcode == zips1[i])
  # Plot the entry just found; add allows for multiple points, pch sets shape, cex controls size.
  plot(st_geometry(uszipcode[index1[i],]), add = T, pch = 16, cex = cexVal)
  # Retrieve lat and long from the special feature found above
  coords1[i,] <- st_coordinates(uszipcode[index1[i],])
  # Compute the distance from zip code centroid to lake/park; Earth radius is optimized for latitude of VA
  dist1[i] <- haversine(coords1[i,], drp, 3961)
}

# Add red point for the location of lake/park
points(-77.589685, 37.6253414, col = "red", pch = 20)
# numerical summary of the distance
summary(dist1)



## Dorey Park

# Repeat for all other locations
vamap <- us_states(states = c("Virginia", "Maryland"))
plot(st_geometry(vamap), main = "Dorey Park")

cexVal = 1.0
index2 = 0
coords2 <- matrix(c(0,0), ncol = 2, nrow = length(zips2))
dist2 = 0
dorp = c(-77.3397473, 37.4662142)

for(i in 1:length(zips2)) {
  cexVal = 1.0
  for (j in 1:length(zips2)) {
    if ((zips2[j] == zips2[i]) && (i != j)) {
      if (cexVal == 1.0) {
        cexVal = 1.2
      }
      else cexVal = cexVal + 0.1
    }
  }  
  index2[i] <- which(uszipcode$zipcode == zips2[i])
  plot(st_geometry(uszipcode[index2[i],]), add = T, pch = 16, cex = cexVal)
  coords2[i,] <- st_coordinates(uszipcode[index2[i],])
  dist2[i] <- haversine(coords2[i,], dorp, 3961)
}

points(-77.3397473, 37.4662142, col = "red", pch = 20)
summary(dist2)


## Three Lakes Park

vamap <- us_states(states = c("Virginia", "North Carolina", "South Carolina"))
plot(st_geometry(vamap), main = "Three Lakes Park")

cexVal = 1.0
index3 = 0
coords3 <- matrix(c(0,0), ncol = 2, nrow = length(zips3))
dist3 = 0
tlp = c(-77.4330965, 37.6181797)

for(i in 1:length(zips3)) {
  cexVal = 1.0
  for (j in 1:length(zips3)) {
    if ((zips3[j] == zips3[i]) && (i != j)) {
      if (cexVal == 1.0) {
        cexVal = 1.2
      }
      else cexVal = cexVal + 0.1
    }
  }  
  index3[i] <- which(uszipcode$zipcode == zips3[i])
  plot(st_geometry(uszipcode[index3[i],]), add = T, pch = 16, cex = cexVal)
  coords3[i,] <- st_coordinates(uszipcode[index3[i],])
  dist3[i] <- haversine(coords3[i,], tlp, 3961)
}

points(-77.4330965, 37.6181797, col = "red", pch = 20)
summary(dist3)



## Downtown Waterfront

vamap <- us_states(states = c("Virginia", "West Virginia"))
plot(st_geometry(vamap), main = "Riverwalk Park")

cexVal = 1.0
index5 = 0
coords5 <- matrix(c(0,0), ncol = 2, nrow = length(zips5))
dist5 = 0
dtw = c(-79.1394458, 37.4144207)

for(i in 1:length(zips5)) {
  cexVal = 1.0
  for (j in 1:length(zips5)) {
    if ((zips5[j] == zips5[i]) && (i != j)) {
      if (cexVal == 1.0) {
        cexVal = 1.2
      }
      else cexVal = cexVal + 0.1
    }
  }  
  index5[i] <- which(uszipcode$zipcode == zips5[i])
  plot(st_geometry(uszipcode[index5[i],]), add = T, pch = 16, cex = cexVal)
  coords5[i,] <- st_coordinates(uszipcode[index5[i],])
  dist5[i] <- haversine(coords5[i,], dtw, 3961)
}

points(-79.1394458, 37.4144207, col = "red", pch = 20)
summary(dist5)


## Wasena Park

vamap <- us_states(states = c("Virginia"))
plot(st_geometry(vamap), main = "Wasena Park")

cexVal = 1.0
index6 = 0
coords6 <- matrix(c(0,0), ncol = 2, nrow = length(zips6))
dist6 = 0
wsp = c(-79.9613277, 37.2668339)

for(i in 1:length(zips6)) {
  cexVal = 1.0
  for (j in 1:length(zips6)) {
    if ((zips6[j] == zips6[i]) && (i != j)) {
      if (cexVal == 1.0) {
        cexVal = 1.2
      }
      else cexVal = cexVal + 0.1
    }
  }  
  index6[i] <- which(uszipcode$zipcode == zips6[i])
  plot(st_geometry(uszipcode[index6[i],]), add = T, pch = 16, cex = cexVal)
  coords6[i,] <- st_coordinates(uszipcode[index6[i],])
  dist6[i] <- haversine(coords6[i,], wsp, 3961)
}

points(-79.9613277, 37.2668339, col = "red", pch = 20)
summary(dist6)


## Claytor Lake State Park

vamap <- us_states(states = c("Virginia", "North Carolina", "West Virginia", "Ohio"))
plot(st_geometry(vamap), main = "Claytor Lake")

cexVal = 1.0
index7 = 0
coords7 <- matrix(c(0,0), ncol = 2, nrow = length(zips7))
dist7 = 0
clsp = c(-80.6304205, 37.0575022)

for(i in 1:length(zips7)) {
  cexVal = 1.0
  for (j in 1:length(zips7)) {
    if ((zips7[j] == zips7[i]) && (i != j)) {
      if (cexVal == 1.0) {
        cexVal = 1.2
      }
      else cexVal = cexVal + 0.1
    }
  }  
  index7[i] <- which(uszipcode$zipcode == zips7[i])
  plot(st_geometry(uszipcode[index7[i],]), add = T, pch = 16, cex = cexVal)
  coords7[i,] <- st_coordinates(uszipcode[index7[i],])
  dist7[i] <- haversine(coords7[i,], clsp, 3961)
}

points(-80.6304205, 37.0575022, col = "red", pch = 20)
summary(dist7)


## Hungry Mother State Park

vamap <- us_states(states = c("Virginia", "North Carolina"))
plot(st_geometry(vamap), main = "Hungry Mother")

cexVal = 1.0
index8 = 0
coords8 <- matrix(c(0,0), ncol = 2, nrow = length(zips8))
dist8 = 0
hmsp = c(-81.5285987, 36.8772909)

for(i in 1:length(zips8)) {
  cexVal = 1.0
  for (j in 1:length(zips8)) {
    if ((zips8[j] == zips8[i]) && (i != j)) {
      if (cexVal == 1.0) {
        cexVal = 1.2
      }
      else cexVal = cexVal + 0.1
    }
  }  
  index8[i] <- which(uszipcode$zipcode == zips8[i])       ## one in Texas
  plot(st_geometry(uszipcode[index8[i],]), add = T, pch = 16, cex = cexVal)
  coords8[i,] <- st_coordinates(uszipcode[index8[i],])
  dist8[i] <- haversine(coords8[i,], hmsp, 3961)
}

points(-81.5285987, 36.8772909, col = "red", pch = 20)
summary(dist8)


## Rural Retreat

vamap <- us_states(states = c("Virginia", "North Carolina"))
plot(st_geometry(vamap), main = "Rural Retreat")

cexVal = 1.0
index9 = 0
coords9 <- matrix(c(0,0), ncol = 2, nrow = length(zips9))
dist9 = 0
rr = c(-81.2921546, 36.864965)

for(i in 1:length(zips9)) {
  cexVal = 1.0
  for (j in 1:length(zips9)) {
    if ((zips9[j] == zips9[i]) && (i != j)) {
      if (cexVal == 1.0) {
        cexVal = 1.2
      }
      else cexVal = cexVal + 0.1
    }
  }  
  index9[i] <- which(uszipcode$zipcode == zips9[i])
  plot(st_geometry(uszipcode[index9[i],]), add = T, pch = 16, cex = cexVal)
  coords9[i,] <- st_coordinates(uszipcode[index9[i],])
  dist9[i] <- haversine(coords9[i,], rr, 3961)
}

points(-81.2921546, 36.864965, col = "red", pch = 20)
summary(dist9)


## Lincolnshire Lake

vamap <- us_states(states = c("Virginia", "West Virginia"))
plot(st_geometry(vamap), main = "Lincolnshire Lake")

cexVal = 1.0
index10 = 0
coords10 <- matrix(c(0,0), ncol = 2, nrow = length(zips10))
dist10 = 0
ll = c(-81.500448, 37.1349968)

for(i in 1:length(zips10)) {
  cexVal = 1.0
  for (j in 1:length(zips10)) {
    if ((zips10[j] == zips10[i]) && (i != j)) {
      if (cexVal == 1.0) {
        cexVal = 1.2
      }
      else cexVal = cexVal + 0.1
    }
  }  
  index10[i] <- which(uszipcode$zipcode == zips10[i])
  plot(st_geometry(uszipcode[index10[i],]), add = T, pch = 16, cex = cexVal)
  coords10[i,] <- st_coordinates(uszipcode[index10[i],])
  dist10[i] <- haversine(coords10[i,], ll, 3961)
}
points(-81.500448, 37.1349968, col = "red", pch = 20)
summary(dist10)


## Wise County

vamap <- us_states(states = c("Virginia"))
plot(st_geometry(vamap), main = "Wise County")

cexVal = 1.0
index11 = 0
coords11 <- matrix(c(0,0), ncol = 2, nrow = length(zips11))
dist11 = 0
wc = c(-82.6526902, 36.9317312)

for(i in 1:length(zips11)) {
  cexVal = 1.0
  for (j in 1:length(zips11)) {
    if ((zips11[j] == zips11[i]) && (i != j)) {
      if (cexVal == 1.0) {
        cexVal = 1.2
      }
      else cexVal = cexVal + 0.1
    }
  }
  if (cexVal > 2.0) {
    cexVal = 2.0
  }
  index11[i] <- which(uszipcode$zipcode == zips11[i])
  plot(st_geometry(uszipcode[index11[i],]), add = T, pch = 16, cex = cexVal)
  coords11[i,] <- st_coordinates(uszipcode[index11[i],])
  dist11[i] <- haversine(coords11[i,], wc, 3961)
}
points(-82.6526902, 36.9317312, col = "red", pch = 20)
summary(dist11)


## Burke Lake

vamap <- us_states(states = c("Virginia", "Maryland"))
plot(st_geometry(vamap), main = "Burke Lake")

cexVal = 1.0
index12 = 0
coords12 <- matrix(c(0,0), ncol = 2, nrow = length(zips12))
dist12 = 0
bl = c(-77.3097918, 38.7626027)

for(i in 1:length(zips12)) {
  cexVal = 1.0
  for (j in 1:length(zips12)) {
    if ((zips12[j] == zips12[i]) && (i != j)) {
      if (cexVal == 1.0) {
        cexVal = 1.2
      }
      else cexVal = cexVal + 0.1
    }
  }
  if (cexVal > 2.0) {
    cexVal = 2.0
  }
  index12[i] <- which(uszipcode$zipcode == zips12[i])
  plot(st_geometry(uszipcode[index12[i],]), add = T, pch = 16, cex = cexVal)
  coords12[i,] <- st_coordinates(uszipcode[index12[i],])
  dist12[i] <- haversine(coords12[i,], bl, 3961)
}
points(-77.3097918, 38.7626027, col = "red", pch = 20)
summary(dist12)


## Lake Shenandoah

vamap <- us_states(states = c("Virginia", "North Carolina", "Maryland", "West Virginia", "Delaware"))
plot(st_geometry(vamap), main = "Lake Shenandoah")

cexVal = 1.0
index13 = 0
coords13 <- matrix(c(0,0), ncol = 2, nrow = length(zips13))
dist13 = 0
ls = c(-78.8413721, 38.3817253)

for(i in 1:length(zips13)) {
  cexVal = 1.0
  for (j in 1:length(zips13)) {
    if ((zips13[j] == zips13[i]) && (i != j)) {
      if (cexVal == 1.0) {
        cexVal = 1.2
      }
      else cexVal = cexVal + 0.1
    }
  }
  if (cexVal > 2.0) {
    cexVal = 2.0
  }
  index13[i] <- which(uszipcode$zipcode == zips13[i])
  plot(st_geometry(uszipcode[index13[i],]), add = T, pch = 16, cex = cexVal)
  coords13[i,] <- st_coordinates(uszipcode[index13[i],])
  dist13[i] <- haversine(coords13[i,], ls, 3961)
}
points(-78.8413721, 38.3817253, col = "red", pch = 20)
summary(dist13)


## Darden Towe

vamap <- us_states(states = c("Virginia"))
plot(st_geometry(vamap), main = "Darden Towe")

cexVal = 1.0
index14 = 0
coords14 <- matrix(c(0,0), ncol = 2, nrow = length(zips14))
dist14 = 0
dt = c(-78.451038, 38.0422139)

for(i in 1:length(zips14)) {
  cexVal = 1.0
  for (j in 1:length(zips14)) {
    if ((zips14[j] == zips14[i]) && (i != j)) {
      if (cexVal == 1.0) {
        cexVal = 1.2
      }
      else cexVal = cexVal + 0.1
    }
  }
  if (cexVal > 2.0) {
    cexVal = 2.0
  }
  index14[i] <- which(uszipcode$zipcode == zips14[i])
  plot(st_geometry(uszipcode[index14[i],]), add = T, pch = 16, cex = cexVal)
  coords14[i,] <- st_coordinates(uszipcode[index14[i],])
  dist14[i] <- haversine(coords14[i,], dt, 3961)
}
points(-78.451038, 38.0422139, col = "red", pch = 20)
summary(dist14)


## Motts Run

vamap <- us_states(states = c("Virginia", "North Carolina"))
plot(st_geometry(vamap), main = "Motts Run")

cexVal = 1.0
index15 = 0
coords15 <- matrix(c(0,0), ncol = 2, nrow = length(zips15))
dist15 = 0
mr = c(-77.5631427, 38.3155306)

for(i in 1:length(zips15)) {
  cexVal = 1.0
  for (j in 1:length(zips15)) {
    if ((zips15[j] == zips15[i]) && (i != j)) {
      if (cexVal == 1.0) {
        cexVal = 1.2
      }
      else cexVal = cexVal + 0.1
    }
  }
  if (cexVal > 2.0) {
    cexVal = 2.0
  }
  index15[i] <- which(uszipcode$zipcode == zips15[i])
  plot(st_geometry(uszipcode[index15[i],]), add = T, pch = 16, cex = cexVal)
  coords15[i,] <- st_coordinates(uszipcode[index15[i],])
  dist15[i] <- haversine(coords15[i,], mr, 3961)
}
points(-77.5631427, 38.3155306, col = "red", pch = 20)
summary(dist15)

## All Locations

vamap <- us_states(states = c("Virginia", "North Carolina", "West Virginia", "Maryland", "Delaware", "Ohio"))
plot(st_geometry(vamap), main = "All Locations")

cexVal = 1.0
indexTot = 0
coordsTot <- matrix(c(0,0), ncol = 2, nrow = length(zipstot))

for(i in 1:length(zipstot)) {
  cexVal = 1.0
  for (j in 1:length(zipstot)) {
    if ((zipstot[j] == zipstot[i]) && (i != j)) {
      if (cexVal == 1.0) {
        cexVal = 1.2
      }
      else cexVal = cexVal + 0.1
    }
  }
  if (cexVal > 2.0) {
    cexVal = 2.0
  }
  indexTot[i] <- which(uszipcode$zipcode == zipstot[i])
  plot(st_geometry(uszipcode[indexTot[i],]), add = T, pch = 16, cex = cexVal)
}
points(-77.589685, 37.6253414, col = "red", pch = 20)
points(-77.3397473, 37.4662142, col = "red", pch = 20)
points(-77.4330965, 37.6181797, col = "red", pch = 20)
points(-79.1394458, 37.4144207, col = "red", pch = 20)
points(-79.9613277, 37.2668339, col = "red", pch = 20)
points(-80.6304205, 37.0575022, col = "red", pch = 20)
points(-81.5285987, 36.8772909, col = "red", pch = 20)
points(-81.2921546, 36.864965, col = "red", pch = 20)
points(-81.500448, 37.1349968, col = "red", pch = 20)
points(-82.6526902, 36.9317312, col = "red", pch = 20)
points(-77.3097918, 38.7626027, col = "red", pch = 20)
points(-78.8413721, 38.3817253, col = "red", pch = 20)
points(-77.5631427, 38.3155306, col = "red", pch = 20)
points(-78.451038, 38.0422139, col = "red", pch = 20)
legend("topright", inset = 0.05, legend = c("Event Locations", "Attendee Residence"), fill = c("red","black"), cex = 0.5)


# Combine all distances into one dataframe
distAll <- c(dist1, dist2, dist3, dist5, dist6, dist7, dist8, dist9, dist10, dist11, dist12, dist13, dist14, dist15)
# Numerical summary of all distances
summary(distAll)

# Tells how many distances are greater than 100 miles
which(min(dist1))

round(dist1, 3)

# Decile table for distances travelled
round(quantile(distAll, prob = seq(0, 1, length = 11), type = 5), 2)

# Sort the distances in ascending order
sortAll <- as.data.frame(sort(distAll))
# Add index to aid in plotting
sortAll$Index <- 1:317

     
# Plot the distribtion of distances travelled; geom_point plots with points, geom_line adds a line
ggplot(sortAll, aes(x = sortAll$Index, y = sortAll$`sort(distAll)`)) + geom_point() + geom_line() +
  # Plot axis labels and title
  labs(x = "Index", y = "Distance (Mi.)", title = "Distribution of Distance to Event") +
  # make plot text larger
  theme(text = element_text(size = 15))

#############################################################################################################


### Not In Report - Requires License_Sales_Access.R file ###


## Examining purchasing trends for Burke Lake and Richmond areas ####


# combine distances and zipcodes into one dataframe
zipsDist <- cbind(distAll, zipstot)

# collect zip codes and distances less than 22 miles for Burke Lake
zipsBurke <- zipsDist[205:231,]
zipsBurke <- zipsBurke[-11,]

# Collect zip codes and distances less than 22 miles for Richmond
zipsRich <- zipsDist[1:101,]
zipsRich <- subset(zipsRich, zipsRich[,1] < 22)

# All licenses bought in the zip codes that attended Richmond and Burke Lake events.
# NOTE: license dataframe is from License_Sales_Access.R file
boughtBurke <- license[license$ZipCode %in% zipsBurke,]
boughtRich <- license[license$ZipCode %in% zipsRich,]

# Divide the data into daily data
stanBurke <- table(boughtBurke$Date)
# Standardize by dividing by total number of licenses sold during period
stanBurke <- stanBurke/3281

# Repeat for Richmond
stanRich <- table(boughtRich$Date)
stanRich <- stanRich/5940

# Repeat for rest of state
stanState <- table(license$Date)
stanState <- stanState/81757

# Barplot for Burke Lake area; alpha sets the transparency of the bars. Blue is the area we are examining, red is the rest of the state.
barplot(stanBurke, ylim = c(0.0, 0.05), col = rgb(red = 0, blue = 1, green = 0, alpha = 0.5), main = "Standardized Daily License Sales Data - Burke Lake Area")
barplot(stanState, ylim = c(0.0, 0.05), add = T, col = rgb(red = 1, blue = 0, green = 0, alpha = 0.5))

# Barplot for Richmond area
barplot(stanRich, ylim = c(0.0, 0.05), col = rgb(red = 0, blue = 1, green = 0, alpha = 0.5), main = "Standardized Daily License Sales Data - Richmond Area")
barplot(stanState, ylim = c(0.0, 0.05), add = T, col = rgb(red = 1, blue = 0, green = 0, alpha = 0.5))



### Not In Report ###

## Plotting distances travelled by heard/not heard about the event.
## Does not handle coloring for multiple people from same zip code.

zipsh <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/zips+heard.csv", header = F)

cexVal = 1.0
indexh = 0
coordsh <- matrix(c(0,0), ncol = 2, nrow = length(zipstot))

for(i in 1:length(zipsh$V2)) {
  cexVal = 1.0
  for (j in 1:length(zipsh$V2)) {
    if ((zipsh$V2[j] == zipsh$V2[i]) && (i != j)) {
      if (cexVal == 1.0) {
        cexVal = 1.2
      }
      else cexVal = cexVal + 0.1
    }
  }
  if (cexVal > 2.0) {
    cexVal = 2.0
  }
  if (zipsh$V1[i] == 1) {
    color <- "Green"
  }
  else color <- "Red"
  
  indexh[i] <- which(uszipcode$zipcode == zipsh$V2[i])
  plot(st_geometry(uszipcode[indexh[i],]), add = T, pch = 16, cex = cexVal, col = color)
}



############################################################################################################
rm(list=ls())


## Attendee Last Time Fishing barcharts ####


# Read txt file with Last Time Fishing survey responses. File path needs to be changed.
exper <- read.table(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/experience.txt", header = F, sep = "\n")

# Divide responses based on location
exp1 <- exper[1:11,]
exp2 <- exper[12:68,]
exp3 <- exper[69:113,]
exp5 <- exper[114:121,]
exp6 <- exper[122:126,]
exp7 <- exper[127:157,]
exp8 <- exper[158:181,]
exp9 <- exper[182:187,]
exp10 <- exper[188:208,]
exp11 <- exper[209:217,]
exp12 <- exper[218:246,]
exp13 <- exper[247:271,]
exp14 <- exper[272:316,]
exp15 <- exper[317:333,]

# ggplot requires dataframes to plot
exp1 <- as.data.frame(exp1)
exp2 <- as.data.frame(exp2)
exp3 <- as.data.frame(exp3)
exp5 <- as.data.frame(exp5)
exp6 <- as.data.frame(exp6)
exp7 <- as.data.frame(exp7)
exp8 <- as.data.frame(exp8)
exp9 <- as.data.frame(exp9)
exp10 <- as.data.frame(exp10)
exp11 <- as.data.frame(exp11)
exp12 <- as.data.frame(exp12)
exp13 <- as.data.frame(exp13)
exp14 <- as.data.frame(exp14)
exp15 <- as.data.frame(exp15)
total <- as.data.frame(exper)

# Create barplots and make text larger, drop = FALSE keeps all bins even if empty.
p1 <- ggplot(exp1, aes(factor(exp1, levels = c("First Time","Within Past Year",
            "1-5 Years Ago","6-10 Years Ago","Over 10 Years Ago")))) + geom_bar() + labs(x = "Last Fished",
            y = "Count", title = "Last Time Fishing Freshwater in Virginia") + scale_x_discrete(drop = FALSE) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15))


# repeat for rest of locations
p2 <- ggplot(exp2, aes(factor(exp2, levels = c("First Time","Within Past Year",
           "1-5 Years Ago","6-10 Years Ago","Over 10 Years Ago")))) + geom_bar() + labs(x = "Last Fished",
            y = "Count", title = "Last Time Fishing Freshwater in Virginia") + scale_x_discrete(drop = FALSE) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15))

p3 <- ggplot(exp3, aes(factor(exp3, levels = c("First Time","Within Past Year",
            "1-5 Years Ago","6-10 Years Ago","Over 10 Years Ago")))) + geom_bar() + labs(x = "Last Fished",
            y = "Count", title = "Last Time Fishing Freshwater in Virginia") + scale_x_discrete(drop = FALSE) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15))

p5 <- ggplot(exp5, aes(factor(exp5, levels = c("First Time","Within Past Year",
            "1-5 Years Ago","6-10 Years Ago","Over 10 Years Ago")))) + geom_bar() + labs(x = "Last Fished",
            y = "Count", title = "Last Time Fishing Freshwater in Virginia") + scale_x_discrete(drop = FALSE) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15))

p6 <- ggplot(exp6, aes(factor(exp6, levels = c("First Time","Within Past Year",
            "1-5 Years Ago","6-10 Years Ago","Over 10 Years Ago")))) + geom_bar() + labs(x = "Last Fished",
            y = "Count", title = "Last Time Fishing Freshwater in Virginia") + scale_x_discrete(drop = FALSE) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15)) +
            scale_y_continuous(breaks = c(0,1,2))

p7 <- ggplot(exp7, aes(factor(exp7, levels = c("First Time","Within Past Year",
            "1-5 Years Ago","6-10 Years Ago","Over 10 Years Ago")))) + geom_bar() + labs(x = "Last Fished",
            y = "Count", title = "Last Time Fishing Freshwater in Virginia") + scale_x_discrete(drop = FALSE) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15))

p8 <- ggplot(exp8, aes(factor(exp8, levels = c("First Time","Within Past Year",
            "1-5 Years Ago","6-10 Years Ago","Over 10 Years Ago")))) + geom_bar() + labs(x = "Last Fished",
            y = "Count", title = "Last Time Fishing Freshwater in Virginia") + scale_x_discrete(drop = FALSE) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15))

p9 <- ggplot(exp9, aes(factor(exp9, levels = c("First Time","Within Past Year",
            "1-5 Years Ago","6-10 Years Ago","Over 10 Years Ago")))) + geom_bar() + labs(x = "Last Fished",
            y = "Count", title = "Last Time Fishing Freshwater in Virginia") + scale_x_discrete(drop = FALSE) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15))

p10 <- ggplot(exp10, aes(factor(exp10, levels = c("First Time","Within Past Year",
            "1-5 Years Ago","6-10 Years Ago","Over 10 Years Ago")))) + geom_bar() + labs(x = "Last Fished",
            y = "Count", title = "Last Time Fishing Freshwater in Virginia") + scale_x_discrete(drop = FALSE) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15))

p11 <- ggplot(exp11, aes(factor(exp11, levels = c("First Time","Within Past Year",
            "1-5 Years Ago","6-10 Years Ago","Over 10 Years Ago")))) + geom_bar() + labs(x = "Last Fished",
            y = "Count", title = "Last Time Fishing Freshwater in Virginia") + scale_x_discrete(drop = FALSE) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15))

p12 <- ggplot(exp12, aes(factor(exp12, levels = c("First Time","Within Past Year",
            "1-5 Years Ago","6-10 Years Ago","Over 10 Years Ago")))) + geom_bar() + labs(x = "Last Fished",
            y = "Count", title = "Last Time Fishing Freshwater in Virginia") + scale_x_discrete(drop = FALSE) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15))

p13 <- ggplot(exp13, aes(factor(exp13, levels = c("First Time","Within Past Year",
            "1-5 Years Ago","6-10 Years Ago","Over 10 Years Ago")))) + geom_bar() + labs(x = "Last Fished",
            y = "Count", title = "Last Time Fishing Freshwater in Virginia") + scale_x_discrete(drop = FALSE) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15))

p14 <- ggplot(exp14, aes(factor(exp14, levels = c("First Time","Within Past Year",
            "1-5 Years Ago","6-10 Years Ago","Over 10 Years Ago")))) + geom_bar() + labs(x = "Last Fished",
            y = "Count", title = "Last Time Fishing Freshwater in Virginia") + scale_x_discrete(drop = FALSE) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15))

p15 <- ggplot(exp15, aes(factor(exp15, levels = c("First Time","Within Past Year",
            "1-5 Years Ago","6-10 Years Ago","Over 10 Years Ago")))) + geom_bar() + labs(x = "Last Fished",
            y = "Count", title = "Last Time Fishing Freshwater in Virginia") + scale_x_discrete(drop = FALSE) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15))


# All locations
ptot <- ggplot(total, aes(factor(total$V1, levels = c("First Time","Within Past Year",
            "1-5 Years Ago","6-10 Years Ago","Over 10 Years Ago")))) + geom_bar() + labs(x = "Last Fished",
            y = "Count", title = "Last Time Fishing Freshwater in Virginia") + scale_x_discrete(drop = FALSE) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15)) +
            geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25)

# Displays the plots
p1
p2
p3
p5
p6
p7
p8
p9
p10
p11
p12
p13
p14
p15
ptot


######################################################################################################################
rm(list=ls())


## Misc Visuals ####


# Read in csv file of the entire survey; change file path if needed
survey <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/FFDSurveys.csv", header = T)

# Shows the names of all columns
colnames(survey)

# Rename columns
colnames(survey)[1] <- "Survey Number"

# Get rid of unecessary columns
survey <- survey[,-21]
survey <- survey[,-4]

# divide the surveys into heard and not heard groups
surveyHeard <- subset(survey, Q2..Hear.Free.Fishing == 1)
surveyNotHeard <- subset(survey, Q2..Hear.Free.Fishing == 0)


# output number of people in each category
sum(surveyHeard$Q5.1.Friend)
sum(surveyHeard$Q5.2.S.O)
sum(surveyHeard$Q5.3.Son)
sum(surveyHeard$Q5.4.Daughter)
sum(surveyHeard$Q5.5.Niece.Nephew)
sum(surveyHeard$Q5.6.Other.family)
aloneHeard <- subset(surveyHeard, Q5.1.Friend == 0 & Q5.2.S.O == 0 & Q5.3.Son == 0 & Q5.4.Daughter == 0 & Q5.5.Niece.Nephew == 0 & Q5.6.Other.family == 0)

# output number of people in each category
sum(surveyNotHeard$Q5.1.Friend)
sum(surveyNotHeard$Q5.2.S.O)
sum(surveyNotHeard$Q5.3.Son)
sum(surveyNotHeard$Q5.4.Daughter)
sum(surveyNotHeard$Q5.5.Niece.Nephew)
sum(surveyNotHeard$Q5.6.Other.family)
aloneNotHeard <- subset(surveyNotHeard, Q5.1.Friend == 0 & Q5.2.S.O == 0 & Q5.3.Son == 0 & Q5.4.Daughter == 0 & Q5.5.Niece.Nephew == 0 & Q5.6.Other.family == 0)


## Heard/Not Heard and Brought

# Exact same as the Who People Brought section with different numbers
hearBrought <- data.frame(group = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"),
                    value = c(30, 41, 45, 74, 46, 7, 41))
hearBrought$group <- factor(hearBrought$group, levels = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"))

ggplot(hearBrought, aes(hearBrought$group, hearBrought$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#9D5BB1", "#407899"))  +
  geom_text(aes(label = c("30 (15.2%)", "41 (20.7%)", "45 (22.7%)", "74 (37.4%)", "46 (23.2%)", "7 (3.5%)", "41 (20.7%)")), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who People That Heard Attended With")



NotHearBrought <- data.frame(group = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"),
                          value = c(11, 34, 25, 36, 26, 2, 53))
NotHearBrought$group <- factor(NotHearBrought$group, levels = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"))

ggplot(NotHearBrought, aes(NotHearBrought$group, NotHearBrought$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#9D5BB1", "#407899"))  +
  geom_text(aes(label = c("11 (8.1%)", "34 (25.2%)", "25 (18.5%)", "36 (26.7%)", "26 (19.3%)", "2 (1.5%)", "53 (39.3%)")), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who People That Didn't Hear Attended With") + lims(y = c(0,74))


## Heard/Not Heard Experience

# Exact same as the Last Time Fishing section for all locations
# survey1 is the same as survey except Q1. Last Fished has been changed to word form instead of numbers

survey1 <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/FFDSurveys1.csv", header = T)

# Shows the names of all columns
colnames(survey1)

# Get rid of unecessary columns
survey1 <- survey1[,-21]
survey1 <- survey1[,-4]

# divide the surveys into heard and not heard groups
surveyHeard1 <- subset(survey1, Q2..Hear.Free.Fishing == 1)
surveyNotHeard1 <- subset(survey1, Q2..Hear.Free.Fishing == 0)


## stat = 'count' counts the cases instead of using the numbers provided
ggplot(surveyHeard1, aes(factor(surveyHeard1$Q1..Last.fished, levels = c("First Time","Within Past Year",
           "1-5 Years Ago","6-10 Years Ago","Over 10 Years Ago")))) + geom_bar() + labs(x = "Last Fished",
           y = "Count", title = "Last Time Fishing for People That Heard") + scale_x_discrete(drop = FALSE) +
           theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15)) +
           geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25)

ggplot(surveyNotHeard1, aes(factor(surveyNotHeard1$Q1..Last.fished, levels = c("First Time","Within Past Year",
           "1-5 Years Ago","6-10 Years Ago","Over 10 Years Ago")))) + geom_bar() + labs(x = "Last Fished",
           y = "Count", title = "Last Time Fishing for People That Didn't Heard") + scale_x_discrete(drop = FALSE) +
           theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05),text = element_text(size = 15)) +
           geom_text(stat = 'count', aes(label = ..count.. ), position = position_dodge(width = 1), vjust = -.25) +
           lims(y = c(0,96))


## First Timers vs. Regulars

# separate the heard group based on last time fishing response
firstHeard <- subset(surveyHeard, surveyHeard$Q1..Last.fished == 0)
regularHeard <- subset(surveyHeard, surveyHeard$Q1..Last.fished == 1)

#separate not heard group based on last time fishing response
firstNotHeard <- subset(surveyNotHeard, surveyNotHeard$Q1..Last.fished == 0)
regularNotHeard <- subset(surveyNotHeard, surveyNotHeard$Q1..Last.fished == 1)


## First Timers Heard/Not Heard Brought


# Same as above
sum(firstHeard$Q5.1.Friend)
sum(firstHeard$Q5.2.S.O)
sum(firstHeard$Q5.3.Son)
sum(firstHeard$Q5.4.Daughter)
sum(firstHeard$Q5.5.Niece.Nephew)
sum(firstHeard$Q5.6.Other.family)
aloneFirstHeard <- subset(firstHeard, Q5.1.Friend == 0 & Q5.2.S.O == 0 & Q5.3.Son == 0 & Q5.4.Daughter == 0 & Q5.5.Niece.Nephew == 0 & Q5.6.Other.family == 0)

# Same as above
sum(firstNotHeard$Q5.1.Friend)
sum(firstNotHeard$Q5.2.S.O)
sum(firstNotHeard$Q5.3.Son)
sum(firstNotHeard$Q5.4.Daughter)
sum(firstNotHeard$Q5.5.Niece.Nephew)
sum(firstNotHeard$Q5.6.Other.family)
aloneFirstNotHeard <- subset(firstNotHeard, Q5.1.Friend == 0 & Q5.2.S.O == 0 & Q5.3.Son == 0 & Q5.4.Daughter == 0 & Q5.5.Niece.Nephew == 0 & Q5.6.Other.family == 0)


# Same as Who People Brought section with different numbers
firstHearBrought <- data.frame(group = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"),
                          value = c(4, 7, 15, 20, 9, 1, 13))
firstHearBrought$group <- factor(firstHearBrought$group, levels = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"))

ggplot(firstHearBrought, aes(firstHearBrought$group, firstHearBrought$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#9D5BB1", "#407899"))  +
  geom_text(aes(label = c("4 (7.8%)", "7 (13.7%)", "15 (29.4%)", "20 (39.2%)", "9 (17.6%)", "1 (2.0%)", "13 (25.5%)")), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who First Timers That Heard Attended With") + lims(y = c(0,22))



firstNotHearBrought <- data.frame(group = c("Alone", "Friend", "S/O", "Son", "Daughter", "Other Family"),
                          value = c(3, 17, 8, 13, 14, 22))
firstNotHearBrought$group <- factor(firstNotHearBrought$group, levels = c("Alone", "Friend", "S/O", "Son", "Daughter", "Other Family"))

ggplot(firstNotHearBrought, aes(firstNotHearBrought$group, firstNotHearBrought$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#407899"))  +
  geom_text(aes(label = c("3 (5.8%)", "17 (32.7%)", "8 (15.4%)", "13 (25%)", "14 (26.9%)", "22 (42.3%)")), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who First Timers That Didn't Hear Attended With")


## Regulars Heard/Not Heard Brought


sum(regularHeard$Q5.1.Friend)
sum(regularHeard$Q5.2.S.O)
sum(regularHeard$Q5.3.Son)
sum(regularHeard$Q5.4.Daughter)
sum(regularHeard$Q5.5.Niece.Nephew)
sum(regularHeard$Q5.6.Other.family)
aloneRegularHeard <- subset(regularHeard, Q5.1.Friend == 0 & Q5.2.S.O == 0 & Q5.3.Son == 0 & Q5.4.Daughter == 0 & Q5.5.Niece.Nephew == 0 & Q5.6.Other.family == 0)


sum(regularNotHeard$Q5.1.Friend)
sum(regularNotHeard$Q5.2.S.O)
sum(regularNotHeard$Q5.3.Son)
sum(regularNotHeard$Q5.4.Daughter)
sum(regularNotHeard$Q5.5.Niece.Nephew)
sum(regularNotHeard$Q5.6.Other.family)
aloneRegularNotHeard <- subset(regularNotHeard, Q5.1.Friend == 0 & Q5.2.S.O == 0 & Q5.3.Son == 0 & Q5.4.Daughter == 0 & Q5.5.Niece.Nephew == 0 & Q5.6.Other.family == 0)


RegHearBrought <- data.frame(group = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"),
                               value = c(18, 24, 21, 31, 25, 6, 20))
RegHearBrought$group <- factor(RegHearBrought$group, levels = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"))

ggplot(RegHearBrought, aes(RegHearBrought$group, RegHearBrought$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#9D5BB1", "#407899"))  +
  geom_text(aes(label = c("18 (18.8%)", "24 (25%)", "21 (21.9%)", "31 (32.3%)", "25 (26.0%)", "6 (6.3%)", "20 (20.8%)")), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who Active Anglers That Heard Attended With")



regNotHearBrought <- data.frame(group = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"),
                                  value = c(7, 7, 11, 12, 6, 1, 19))
regNotHearBrought$group <- factor(regNotHearBrought$group, levels = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"))

ggplot(regNotHearBrought, aes(regNotHearBrought$group, regNotHearBrought$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#9D5BB1", "#407899"))  +
  geom_text(aes(label = c("7 (14.9%)", "7 (14.9%)", "11 (23.4%)", "12 (25.5%)", "6 (12.8%)", "1 (2.1%)", "19 (40.4%)")), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who Active Anglers That Didn't Hear Attended With") + lims(y = c(0,31))


## How first timers heard about the event

sum(firstHeard$Q3.1.Friends.Family)
sum(firstHeard$Q3.2.Social.media)
sum(firstHeard$Q3.3.Radio)
sum(firstHeard$Q3.4.Website)
sum(firstHeard$Q3.5.Email)
sum(firstHeard$Q3.6.Other.DGIF)
firstHeard$Q3.7.Other == "" # = 9

firstHeardHow <- data.frame(group = c("Friends/Family", "Social Media", "Radio", "Website",
                              "DGIF Email", "Other DGIF", "Other"), value = c(15, 21, 3, 3, 2, 1, 9))
firstHeardHow$group <- factor(firstHeardHow$group, levels = c("Did Not Hear", "Friends/Family", "Social Media", "Radio", "Website",
                                              "DGIF Email", "Other DGIF", "Other"))

ggplot(firstHeardHow, aes(firstHeardHow$group, firstHeardHow$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0", "#F15854", "#B276B2", "#DECF3F"))  +
  geom_text(aes(label = c("15 (29.4%)", "21 (41.2%)", "3 (5.9%)", "3 (5.9%)", "2 (3.9%)", "1 (2.0%)", "9 (17.6%)")), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "How First Timers Heard About Events") + lims(y = c(0,41))


sum(regularHeard$Q3.1.Friends.Family)
sum(regularHeard$Q3.2.Social.media)
sum(regularHeard$Q3.3.Radio)
sum(regularHeard$Q3.4.Website)
sum(regularHeard$Q3.5.Email)
sum(regularHeard$Q3.6.Other.DGIF)
regularHeard$Q3.7.Other == ""# = 9

regularHeardHow <- data.frame(group = c("Friends/Family", "Social Media", "Radio", "Website",
                                      "DGIF Email", "Other DGIF", "Other"), value = c(29, 41, 5, 17, 7, 7, 9))
regularHeardHow$group <- factor(regularHeardHow$group, levels = c("Friends/Family", "Social Media", "Radio", "Website",
                                                              "DGIF Email", "Other DGIF", "Other"))

ggplot(regularHeardHow, aes(regularHeardHow$group, regularHeardHow$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0", "#F15854", "#B276B2", "#DECF3F"))  +
  geom_text(aes(label = c("29 (30.2%)", "41 (42.7%)", "5 (5.2%)", "17 (17.7%)", "7 (7.3%)", "7 (7.3%)", "9 (9.4%)")), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "How Active Anglers Heard About Events")


# Separate inactives (last fished > 1 year ago) into heard and not heard groups

inactiveHeard <- subset(surveyHeard, surveyHeard$Q1..Last.fished == 2 | surveyHeard$Q1..Last.fished == 3 | surveyHeard$Q1..Last.fished == 4)
inactiveNotHeard <- subset(surveyNotHeard, surveyNotHeard$Q1..Last.fished == 2 | surveyNotHeard$Q1..Last.fished == 3 | surveyNotHeard$Q1..Last.fished == 4)


sum(inactiveHeard$Q3.1.Friends.Family)
sum(inactiveHeard$Q3.2.Social.media)
sum(inactiveHeard$Q3.3.Radio)
sum(inactiveHeard$Q3.4.Website)
sum(inactiveHeard$Q3.5.Email)
sum(inactiveHeard$Q3.6.Other.DGIF)
inactiveHeard$Q3.7.Other == ""# = 5

inactiveHeardHow <- data.frame(group = c("Friends/Family", "Social Media", "Radio", "Website",
                                        "DGIF Email", "Other DGIF", "Other"), value = c(17, 22, 6, 6, 1, 5, 5))
inactiveHeardHow$group <- factor(inactiveHeardHow$group, levels = c("Friends/Family", "Social Media", "Radio", "Website",
                                                                  "DGIF Email", "Other DGIF", "Other"))

ggplot(inactiveHeardHow, aes(inactiveHeardHow$group, inactiveHeardHow$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0", "#F15854", "#B276B2", "#DECF3F"))  +
  geom_text(aes(label = c("17 (33.3%)", "22 (43.1%)", "6 (11.8%)", "6 (11.8%)", "1 (2.0%)", "5 (9.8%)", "5 (9.8%)")), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "How Inactive Anglers Heard About Events") + lims(y = c(0,41))


sum(inactiveNotHeard$Q5.1.Friend)
sum(inactiveNotHeard$Q5.2.S.O)
sum(inactiveNotHeard$Q5.3.Son)
sum(inactiveNotHeard$Q5.4.Daughter)
sum(inactiveNotHeard$Q5.5.Niece.Nephew)
sum(inactiveNotHeard$Q5.6.Other.family)
aloneInactiveNotHeard <- subset(inactiveNotHeard, Q5.1.Friend == 0 & Q5.2.S.O == 0 & Q5.3.Son == 0 & Q5.4.Daughter == 0 & Q5.5.Niece.Nephew == 0 & Q5.6.Other.family == 0)


InactiveNotHearBrought <- data.frame(group = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"),
                                  value = c(1, 10, 6, 11, 6, 1, 12))
InactiveNotHearBrought$group <- factor(InactiveNotHearBrought$group, levels = c("Alone", "Friend", "S/O", "Son", "Daughter", "Niece/Nephew", "Other Family"))

ggplot(InactiveNotHearBrought, aes(InactiveNotHearBrought$group, InactiveNotHearBrought$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#9D5BB1", "#407899"))  +
  geom_text(aes(label = c("1 (2.8%)", "10 (27.8%)", "6 (16.7%)", "11 (30.6%)", "6 (16.7%)", "1 (2.8%)", "12 (33.3%)")), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who Inactives That Didn't Hear Attended With") + lims(y = c(0,23))


sum(inactiveHeard$Q5.1.Friend)
sum(inactiveHeard$Q5.2.S.O)
sum(inactiveHeard$Q5.3.Son)
sum(inactiveHeard$Q5.4.Daughter)
sum(inactiveHeard$Q5.5.Niece.Nephew)
sum(inactiveHeard$Q5.6.Other.family)
aloneInactiveHeard <- subset(inactiveHeard, Q5.1.Friend == 0 & Q5.2.S.O == 0 & Q5.3.Son == 0 & Q5.4.Daughter == 0 & Q5.5.Niece.Nephew == 0 & Q5.6.Other.family == 0)


InactiveHearBrought <- data.frame(group = c("Alone", "Friend", "S/O", "Son", "Daughter", "Other Family"),
                                     value = c(8, 10, 9, 23, 12, 8))
InactiveHearBrought$group <- factor(InactiveHearBrought$group, levels = c("Alone", "Friend", "S/O", "Son", "Daughter", "Other Family"))

ggplot(InactiveHearBrought, aes(InactiveHearBrought$group, InactiveHearBrought$value, fill = group)) + geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(name = "", values = c("#4D4D4D", "#8CD7BF", "#E3B505", "#7A150A", "#FF9EDD", "#407899"))  +
  geom_text(aes(label = c("8 (15.7%)", "10 (19.6%)", "9 (17.6%)", "23 (45.1%)", "12 (23.5%)", "8 (15.7%)")), position = position_dodge(width = 1), vjust = -.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.05), text = element_text(size = 15), legend.position = "none") + 
  labs(x = "", y = "Count", title = "Who Inactives That Heard Attended With")




####################################################################################################################################################
rm(list=ls())


### Not In Report - Requires License_Sales_Access.R ###


## Matches up emails and phone numbers provided at the FFD events with those in the license database ####


personal <- read.csv(file = "C:/Users/wxc36638/Desktop/Projects/Free Fishing Day/CSV/FFD_Personal_data.csv", header = T)

# License dataframe requires License_Sales_Access.R
boughtEmail <- license[license$EmailAddress %in% personal$EMAIL,]

boughtPhone <- license[license$PhoneNumber %in% personal$PHONE,]

unique(personal$EMAIL)

count = 0
for (i in 1:34) {
  for (j in 1:13) {
    if(boughtEmail[j,1] == boughtPhone[i,1] & boughtEmail[j,4] == boughtPhone[i,4]) {
      count = count+1
    
    }
  }
}


