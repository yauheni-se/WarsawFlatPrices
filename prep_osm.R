# Presets ----
setwd("C:/Projects/WarsawFlatPrices/")

library(sf)
library(leaflet)
library(tidyverse)

# Buildings ----
st_layers("data/osm/gis_osm_buildings_a_free_1.shp")
df <- st_read("data/osm/gis_osm_buildings_a_free_1.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df[!is.na(df$type) & str_detect(df$type, "abandoned|construction|dormitory|factory|garage|industrial|manufacture|office"), ]
df <- df %>% as_Spatial()
write_rds(df, "data/clean/buildings.rds")


# Landuse ----
st_layers("data/osm/gis_osm_landuse_a_free_1.shp")
df <- st_read("data/osm/gis_osm_landuse_a_free_1.shp")
df <- st_transform(nature, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/landuse.rds")

# Nature ----
st_layers("data/osm/gis_osm_natural_free_1.shp")
df <- st_read("data/osm/gis_osm_natural_free_1.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/nature.rds")

# Beaches ----
st_layers("data/osm/gis_osm_natural_a_free_1.shp")
df <- st_read("data/osm/gis_osm_natural_a_free_1.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/beaches.rds")

# Places ----
st_layers("data/osm/gis_osm_places_a_free_1.shp")
df <- st_read("data/osm/gis_osm_places_a_free_1.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/places.rds")

# Population ----
st_layers("data/osm/gis_osm_places_free_1.shp")
df <- st_read("data/osm/gis_osm_places_free_1.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/population.rds")

# Churches ----
st_layers("data/osm/gis_osm_pofw_a_free_1.shp")
df <- st_read("data/osm/gis_osm_pofw_a_free_1.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/churches.rds")

# Churches2 ----
st_layers("data/osm/gis_osm_pofw_free_1.shp")
df <- st_read("data/osm/gis_osm_pofw_a_free_1.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/churches2.rds")

# Parks ----
st_layers("data/osm/gis_osm_pois_a_free_1.shp")
df <- st_read("data/osm/gis_osm_pois_a_free_1.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/parks.rds")

# Places2 ----
st_layers("data/osm/gis_osm_pois_free_1.shp")
df <- st_read("data/osm/gis_osm_pois_free_1.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/places2.rds")

# Railways ----
st_layers("data/osm/gis_osm_railways_free_1.shp")
df <- st_read("data/osm/gis_osm_railways_free_1.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/railways.rds")

# Roads ----
st_layers("data/osm/gis_osm_roads_free_1.shp")
df <- st_read("data/osm/gis_osm_roads_free_1.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/roads.rds")

# Traffic ----
st_layers("data/osm/gis_osm_traffic_a_free_1.shp")
df <- st_read("data/osm/gis_osm_traffic_a_free_1.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/traffic.rds")

# Traffic2 ----
st_layers("data/osm/gis_osm_traffic_free_1.shp")
df <- st_read("data/osm/gis_osm_traffic_free_1.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/traffic2.rds")

# Transport ----
st_layers("data/osm/gis_osm_transport_a_free_1.shp")
df <- st_read("data/osm/gis_osm_transport_a_free_1.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/transport.rds")

# Transport2 ----
st_layers("data/osm/gis_osm_transport_free_1.shp")
df <- st_read("data/osm/gis_osm_transport_free_1.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/transport2.rds")

# Water ----
st_layers("data/osm/gis_osm_water_a_free_1.shp")
df <- st_read("data/osm/gis_osm_water_a_free_1.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/water.rds")

# Waterways ----
st_layers("data/osm/gis_osm_waterways_free_1.shp")
df <- st_read("data/osm/gis_osm_waterways_free_1.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/waterways.rds")

# Regions ----
st_layers("data/dzielnicy/dzielnice_Warszawy.shp")
df <- st_read("data/dzielnicy/dzielnice_Warszawy.shp")
df <- st_transform(df, "+init=epsg:4326")
df <- df %>% as_Spatial()
df@data %>% as_tibble()
write_rds(df, "data/clean/regions.rds")