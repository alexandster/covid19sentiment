library(sf)
library(tidyverse)

#set workspace
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load dataframe
load("dat.Rdata")

#convert dataframe to sf object
dat_geo <- st_as_sf(
  dat,
  coords = c("x", "y"),
  agr = "constant",
  crs = 4269,
  stringsAsFactors = FALSE
)

#read county geometries
geom <- st_read('CONUS_counties.shp')

#coordinate system
st_crs(geom)

#drop columns
geom <- subset(geom, select = -c(COUNTYFP,COUNTYNS,AFFGEOID,LSAD,CENTROID_X,CENTROID_Y,AREA_GEO))

#find points within polygons
tweet_in_county <- st_join(dat_geo, geom, join = st_within)

#count adverse tweets per county
tweet_county_sum <- aggregate(tweet_in_county$adverse, by=list(Category=tweet_in_county$GEOID), FUN=sum) %>% na.omit()

#count total tweets per county
tweet_county_count <- count(tweet_in_county, GEOID) %>% 
  na.omit() 
st_geometry(tweet_county_count) <- NULL

#join tweets back to geom
geom <- left_join(left_join(geom,tweet_county_count, by = c("GEOID")),tweet_county_sum, by = c("GEOID" = "Category"))

names(geom)[names(geom)=="n"] <- "totalTweets"
names(geom)[names(geom)=="x"] <- "adverTweets"










