library(sf)
library(tmap)
library(dplyr)

load("sf_sp.Rda") 

df_sp <- subset(df_sp, select = c("FIPS", "Pop_o_60", "Pop_m", "ETHHET", "Unemployed", "Poverty", "RUCC", 
                                  "BIDEN", "PCP", "covid_cases", "Total_tw"))

#read geometries
geom <- st_read("CONUS_counties.shp") %>%
  subset(., select = c("GEOID")) 

#join geometries
df_sp <- left_join(df_sp, geom, by = c("FIPS" = "GEOID")) %>%
  st_as_sf(.)

# zeros to NA
df_sp[df_sp$Total_tw==0,c(2,3,4,5,6,7,8,9, 10)] <- NA

#--------------------variable panel------------------------------------
colnames(df_sp)[which(names(df_sp) == "Pop_o_60")] <- "Population over 60"
colnames(df_sp)[which(names(df_sp) == "Pop_m")] <- "Male population"
colnames(df_sp)[which(names(df_sp) == "ETHHET")] <- "Ethnic"
colnames(df_sp)[which(names(df_sp) == "Unemployed")] <- "Unemployment"
colnames(df_sp)[which(names(df_sp) == "BIDEN")] <- "Biden"
colnames(df_sp)[which(names(df_sp) == "covid_cases")] <- "Case rate"

#read states geometries
states <- st_read("CONUS_states.shp") %>%
  st_transform(., crs = 2163)

df_sp <- st_transform(df_sp, crs = 2163)

#ETHHET variable
df_sp$Ethnic <- scales::rescale(df_sp$Ethnic, to=c(0,1))


#=================================================================================

# make some bbox magic
bbox_new <- st_bbox(df_sp) # current bounding box
xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
bbox_new[4] <- bbox_new[4] + (0.1 * yrange) # ymax - top
bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

#Map 1
coef_map <- 
  tm_shape(df_sp, bbox = bbox_new) +
  tm_fill(c("Population over 60", "Male population", "Ethnic"),
          style=c("quantile","quantile","quantile"),
          n = 5,
          palette=list("Blues", "Greens", "Reds"),
  ) +
  tm_facets(nrow = 3, ncol=1) +
  tm_borders() +
  tm_layout(
    frame = FALSE,
    legend.position = c("right", "bottom"),
    legend.format = list(digits = 2),
  ) +
  tm_shape(states) +
  tm_borders(col="black")
tmap_save(coef_map, "variable panel 1.png", height = 8.5, width = 6.5, units = "in")

#Map 2
coef_map <- 
  tm_shape(df_sp, bbox = bbox_new) +
  tm_fill(c("RUCC", "Unemployment", "Poverty"),
          style=c("fixed","quantile","quantile"),
          n = 5,
          palette=list("Oranges", "Purples", "Blues"),
  ) +
  tm_facets(nrow = 3, ncol=1) +
  tm_borders() +
  tm_layout(
    frame = FALSE,
    legend.position = c("right", "bottom"),
    legend.format = list(digits = 2),
  ) +
  tm_shape(states) +
  tm_borders(col="black")
tmap_save(coef_map, "variable panel 2.png", height = 8.5, width = 6.5, units = "in")

#Map 3
coef_map <- 
  tm_shape(df_sp, bbox = bbox_new) +
  tm_fill(c("Biden", "PCP", "Case rate"),
          style=c("quantile","quantile","quantile"),
          n = 5,
          palette=list("Greens", "Reds", "Oranges"),
  ) +
  tm_facets(nrow = 3, ncol=1) +
  tm_borders() +
  tm_layout(
    frame = FALSE,
    legend.position = c("right", "bottom"),
    legend.format = list(digits = 2),
  ) +
  tm_shape(states) +
  tm_borders(col="black")
tmap_save(coef_map, "variable panel 3.png", height = 8.5, width = 6.5, units = "in")
