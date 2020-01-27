rm(list=ls())

#############################################################################################################
# This script takes a vertical, comma separated list of zip codes copied from Excel, plots the centroid     #
# of the zip code on a map, and calculates the Haversine distance of each zip code from the given location. #
# Must edit areas marked with ** **.                                                                        #
#                                                                                                           #
# Chandler Crescentini                                                                                      #
#############################################################################################################


## Required packages
library(USAboundaries)
library(USAboundariesData)
library(sf)
library(pracma)

## List of all US zip codes
uszipcode <- us_zipcodes()

## Read in array of zips 
zips <- read.csv(file = "**File location here**", header = F)
zips <- zips[,-2]

## Type full name of states as comma separated list
map <- us_states(states = c("Virginia"))
plot(st_geometry(map), main = "**Plot Name**")

## Initializations; Default dot size is 1
cexVal = 1.0
index = 0
coords <- matrix(c(0,0), ncol = 2, nrow = length(zips))
dist = 0

## Can look up location in Google Maps. Copy following the @ in URL and switch the positions. Must be (long, lat).
loc = c(**long**, **lat**)

## Plots latitude and longitude of zip code centroids; increasing in size for each duplicate, and calculates Haversine
for(i in 1:length(zips)) {
  cexVal = 1.0
  for (j in 1:length(zips)) {
    if ((zips[j] == zips[i]) && (i != j)) {
      if (cexVal == 1.0) {
        cexVal = 1.2
      }
      else cexVal = cexVal + 0.1
    }
  }
  if (cexVal > 2.0) {
    cexVal = 2.0
  }
  index[i] <- which(uszipcode$zipcode == zips[i])
  plot(st_geometry(uszipcode[index[i],]), add = T, pch = 16, cex = cexVal)
  
  coords[i,] <- st_coordinates(uszipcode[index[i],])
  dist[i] <- haversine(coords[i,], loc, 3961)
}

## Plots single red point of reference in (long, lat) format
points(loc[1], loc[2], col = "red", pch = 20)

## Summary of distances calculated using Haversine formula with Earth radius optimized for Virginia
summary(dist)
