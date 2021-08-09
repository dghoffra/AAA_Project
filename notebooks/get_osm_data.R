# check https://dominicroye.github.io/en/2018/accessing-openstreetmap-data-with-r/


rm(list=ls())

library(tidyverse)
library(sf)
library(ggmap)
library(osmdata)
library(data.table)
library(geojsonR)
library(rgdal)

# get osm data
Chicago <- c(-88.1983,41.6344,-87.2665,42.0335) #West, South, East, North

available_features()

q <- Chicago %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature("aeroway", "aerodrome") %>% 
  add_osm_feature("amenity", "bar") %>% 
  add_osm_feature("amenity", "pub") %>% 
  add_osm_feature("amenity", "restaurant") %>% 
  add_osm_feature("amenity", "University") %>% 
  add_osm_feature("amenity", "taxi") %>% 
  add_osm_feature("amenity", "clinic") %>% 
  add_osm_feature("amenity", "hospital") %>%
  add_osm_feature("amenity", "arts_centre") %>% 
  add_osm_feature("amenity", "brothel") %>% 
  add_osm_feature("amenity", "casino") %>% 
  add_osm_feature("amenity", "cinema") %>%
  add_osm_feature("amenity", "conference_centre") %>% 
  add_osm_feature("amenity", "events_venue") %>% 
  add_osm_feature("amenity", "love_hotel") %>% 
  add_osm_feature("amenity", "nightclub") %>% 
  add_osm_feature("amenity", "stripclub") %>% 
  add_osm_feature("amenity", "theatre") %>%
  add_osm_feature("amenity", "townhall") %>% 
  add_osm_feature("amenity", "grave_yard") %>% 
  add_osm_feature("amenity", "place_of_worship") %>% 
  add_osm_feature("leisure", "stadium") %>% 
  add_osm_feature("leisure", "park") %>% 
  add_osm_feature("leisure", "water_park") %>% 
  add_osm_feature("leisure", "beach_resort") %>% 
  add_osm_feature("tourism", "aquarium") %>%
  add_osm_feature("tourism", "artwork") %>%
  add_osm_feature("tourism", "attraction") %>%
  add_osm_feature("tourism", "gallery") %>%
  add_osm_feature("tourism", "guest_house") %>%
  add_osm_feature("tourism", "motel") %>%
  add_osm_feature("tourism", "hotel") %>%
  add_osm_feature("tourism", "hostel") %>%
  add_osm_feature("tourism", "museum") %>%
  add_osm_feature("tourism", "theme_park") %>%
  add_osm_feature("tourism", "zoo") %>%
  add_osm_feature("sport", "american_football") %>%
  add_osm_feature("sport", "basketball") %>%
  add_osm_feature("sport", "ice_hockey")
  # feature oder tag spezifizieren

feature <- c("aeroway", rep("amenity",20), rep("leisure",4), rep("tourism",11), rep("sport", 3))
key <- c("aerodrome", "bar", "pub", "restaurant", "university", "taxi", "clinic","hospital","arts_centre", "brothel", "casino", "cinema", "conference_centre",
         "events_venue", "love_hotel", "nightclub","stripclub", "theatre", "townhall","grave_yard", "place_of_worship", "stadium", "park", "water_park", "beach_resort", 
         "aquarium", "artwork", "attraction", "gallery", "guest_house", "motel", "hotel","hostel", "museum", "theme_park", "zoo", "american_football", "basketball", "ice_hockey")

osm_features <- tibble(feature,key)
POI <- tibble()

for (i in 1:nrow(osm_features)){
  #crete querry
  q <- Chicago %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature(osm_features$feature[i], osm_features$key[i])
  
  # send querry
  df_temp <-osmdata_sf(q)
  # get coordinates
  df_temp_info <- df_temp$osm_points[,1]
  # add feature and value
  df_temp_info <- df_temp_info %>% mutate(feature = osm_features$feature[i],
                        key = osm_features$key[i])
  # combine
  df_temp_info <- setDT(df_temp_info, keep.rownames = FALSE)[]
  POI =rbind(POI, df_temp_info)
}

## save data
# con<-file("C:/Users/Felix/Documents/AAA/Data/POI_data.csv",encoding="UTF-8")
# write.table(POI, file=con, row.names=F, sep = ",")
# 
# st_write(POI, "C:/Users/Felix/Documents/AAA/Data/POI_data.shp")
# 
# save(POI, file = "C:/Users/Felix/Documents/AAA/Data/POI_data.RData")

# ggplot(df1$osm_points)+
#   geom_sf(colour = "#08519c",
#           fill = "#08306b",
#           alpha = .5,
#           size = 1,
#           shape = 21)+
#   theme_void()

# test <- readOGR("C:/Users/Felix/Documents/AAA/Data/POI_data.shp") #stringsAsFactors = FALSE)

## Plot data ##

my_map <- get_map(getbb("Chicago"), maptype = "toner-background")

#final map
ggmap(my_map)+
  geom_sf(data = POI$geometry,
          aes(fill = POI$feature),
          inherit.aes = FALSE,
          alpha = .3,
          size = 1,
          shape = 21)+
  labs(x = "", y = "")

# long -> y, lat -> x

table(POI$feature)
table(POI$key)


# analyzinz POI
length(unique(POI$osm_id[POI$key=="park"]))
# every park consists of all points of the perimiter
# and there are load.
# Drop park
# also sport information does not help.
# Place of worship...vielleicht nur church?
# place of worship is huge too

#drop those:
POI_cleaned <- POI[!POI$feature == "sport",]
POI_cleaned <- POI_cleaned[!POI_cleaned$key == "park",]
POI_cleaned <- POI_cleaned[!POI_cleaned$key == "place_of_worship",]
table(POI_cleaned$key)

# plot again

#final map
ggmap(my_map)+
  geom_sf(data = POI_cleaned$geometry,
          aes(fill = POI_cleaned$feature),
          inherit.aes = FALSE,
          alpha = .3,
          size = 1,
          shape = 21)+
  labs(x = "", y = "")


# Linking to 
# chicago geo data
chicago_community_area = FROM_GeoJson(url_file_string = "C:/Users/Felix/Documents/AAA/Data/chicago-community-areas.geojson")
chicago_area = FROM_GeoJson(url_file_string = "C:/Users/Felix/Documents/AAA/Data/chicago.geojson")
Ch_h3 <- read_delim("C:/Users/Felix/Documents/AAA/Data/h3_census_tracts.csv", delim = ";")

# transform to sf
Ch_h3_sf <- sf::st_as_sf( Ch_h3, wkt = "h3_geo_boundary", crs=4326)

# transform geometry data for csv later
POI_cleaned <- POI_cleaned %>%
  mutate(lat = unlist(map(POI_cleaned$geometry,1)),
         lng = unlist(map(POI_cleaned$geometry,2)))

# transform ponts 
POI_cleaned <- st_as_sf(POI_cleaned, crs = st_crs(Ch_h3_sf))

# find intersections
POI_cleaned <- POI_cleaned %>% mutate(
  intersection = as.integer(st_intersects(geometry,Ch_h3_sf))
  , area = if_else(is.na(intersection), '', Ch_h3_sf$h3_id[intersection])
) 



ggmap(my_map)+
  geom_sf(data = Ch_h3_sf$h3_geo_boundary,
          inherit.aes = FALSE,
          alpha = .3,
          size = 1,
          shape = 21)+
  geom_sf(data = POI_cleaned$geometry,
          aes(fill = POI_cleaned$feature),
          inherit.aes = FALSE,
          alpha = .3,
          size = 1,
          shape = 21)+
  labs(x = "lat", y = "long")


df_POI <- POI_cleaned
st_geometry(df_POI) <- NULL

# delete point if same OSM_id appears twice in one h3
df_POI <- df_POI[!duplicated(df_POI$osm_id,df_POI$area),]

con<-file("C:/Users/Felix/Documents/AAA/Data/POI_per_H3.csv",encoding="UTF-8")
write.table(df_POI, file=con, row.names=F, sep = ",")

key_per_h3 <- df_POI %>% group_by(area) %>% count(key)
key_per_h3_wide <- pivot_wider(key_per_h3, values_from = n,names_from = key,names_prefix = "key_")
key_per_h3_wide[is.na(key_per_h3_wide)] <- 0


feature_per_h3 <- df_POI %>% group_by(area) %>% count(feature)
feature_per_h3_wide <- pivot_wider(feature_per_h3, values_from = n,names_from = feature,names_prefix = "feature_")
feature_per_h3_wide[is.na(feature_per_h3_wide)] <- 0

key_and_feature <- left_join(feature_per_h3_wide, key_per_h3_wide)

con<-file("C:/Users/Felix/Documents/AAA/Data/POI_feature_and_key_per_H3.csv",encoding="UTF-8")
write.table(key_and_feature, file=con, row.names=F, sep = ",")


# ###### NOTES ##########
# 
# ggmap(mad_map)+
#   geom_sf(data = cinema$osm_points,
#           inherit.aes = FALSE,
#           colour = "#238443",
#           fill = "#004529",
#           alpha = .5,
#           size = 4,
#           shape = 21)+
#   labs(x = "", y = "")
# 
# #bounding box for the Iberian Peninsula
# m <- c(-10, 30, 5, 46)
# 
# #building the query
# q <- m %>% 
#   opq (timeout = 25*100) %>%
#   add_osm_feature("name", "Mercadona") %>%
#   add_osm_feature("shop", "supermarket")
# 
# #query
# mercadona <- osmdata_sf(q)
# 
# #final map
# ggplot(mercadona$osm_points)+
#   geom_sf(colour = "#08519c",
#           fill = "#08306b",
#           alpha = .5,
#           size = 1,
#           shape = 21)+
#   theme_void()
