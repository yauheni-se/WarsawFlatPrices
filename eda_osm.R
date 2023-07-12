# Presets ----
setwd("C:/Projects/WarsawInterest/")

library(sf)
library(leaflet)
library(tidyverse)

filne_name <- "waterways" # places2 #places.rds
data_poland <- read_rds("data/poland.rds")
data_warsaw <- data_poland[data_poland$NAME_3 == "Warszawa",]
rm(data_poland)
regions <- read_rds("data/clean/regions.rds")

regions %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  addPolygons(stroke = 1, opacity = 0.1, color = "black") %>% 
  setView(lng = 21.05, lat = 52.23, zoom = 11)

df <- read_rds(paste0("data/clean/", filne_name, ".rds"))

# Keep only data from Warsaw, not whole voivodeship ----
df_centers <- data.frame(rgeos::gCentroid(df, byid=TRUE)) %>% 
  as_tibble() %>% 
  mutate(
    object_type = df@data$fclass,
    object_name = df@data$name
  ) %>% 
  mutate(id = row_number()) %>%
  select(id, object_type, object_name, x, y)

df_dict <- df_centers %>% 
  as.data.frame() %>% 
  sf::st_as_sf(coords = c(4, 5)) %>% 
  sf::st_set_crs(4326) %>% 
  sf::st_transform(2163) %>% 
  st_join(st_as_sf(data_warsaw) %>% st_set_crs(4326) %>% st_transform(2163)) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  filter(!is.na(NAME_3)) %>% 
  select(id, object_type, object_name)

df_centers <- df_dict %>% 
  left_join(df_centers %>% select(id, x, y), by = "id")


# Reduce categories ----
df_centers$object_type %>% unique() %>% sort()

drop_lst <- c(
  "atm", "camera_surveillance", "caravan_site", "chalet", "comms_tower", "public_building",
  "bench", "toilet", "bench", "drinking_water", "hunting_stand", "newsagent", "tourist_info",
  "travel_agent", "waste_basket", "post_box", "vending_any", "wayside_shrine", "wayside_cross",
  "water_well", "vending_machine", "shelter", "cliff", "commercial", "industrial", "recreation_ground",
  "retail", "scrub", "quarry", "ice_rink", "telephone", "track", "weir", "pier", "marina",
  "street_lamp", "lock_gate", "mini_roundabout", "crossing", "dam", "slipway", "speed_camera",
  "traffic_signals", "turning_circle", "waterfall", "helipad", "ferry_terminal"
)

rename_objects <- function(x) {
  switch(
    x,
    "car_dealership" = "car service",
    "car_rental" = "car service",
    "car_sharing" = "car service",
    "car_wash" = "car service",
    
    "chemist" = "apteka",
    "pharmacy" = "apteka",

    "convenience" = "food shop",
    "butcher" = "food shop",
    "general" = "food shop",
    "bakery" = "food shop",
    "greengrocer" = "food shop",
    "supermarket" = "food shop",
    "kiosk" = "food shop",
    "beverages" = "food shop",
    
    "clinic" = "health",
    "dentist" = "health",
    "doctors" = "health",
    "hospital" = "health",
    "optician" = "health",
    "veterinary" = "health",
    
    "furniture_shop" = "shop",
    "garden_centre" = "shop",
    "outdoor_shop" = "shop",
    "gift_shop" = "shop",
    "mobile_phone_shop" = "shop",
    "clothes" = "shop",
    "bicycle_shop" = "shop",
    "bookshop" = "shop",
    "shoe_shop" = "shop",
    "sports_shop" = "shop",
    "stationery" = "shop",
    "toy_shop" = "shop",
    "video_shop" = "shop",
    "florist" = "shop",
    "doityourself" = "shop",
    
    "recycling_clothes" = "recycling",
    "recycling_glass" = "recycling",
    "recycling_metal" = "recycling",
    "recycling_paper" = "recycling",
    
    "sports_centre" = "sport",
    "swimming_pool" = "sport",
    "golf_course" = "sport",
    "pitch" = "sport",
    "stadium" = "sport",
    
    "library" = "public office",
    "embassy" = "public office",
    "courthouse" = "public office",
    "town_hall" = "public office",
    "post_office" = "public office",
    
    "hostel" = "renthouse",
    "hotel" = "renthouse",
    "guesthouse" = "renthouse",
    "motel" = "renthouse",
    
    "memorial" = "attraction",
    "monument" = "attraction",
    "ruins"  = "attraction",
    "fort" = "attraction",
    "fountain" = "attraction",
    "observation_tower" = "attraction",
    "viewpoint" = "attraction",
    "tower" = "attraction",
    "castle" = "attraction",
    
    "archaeological" = "cultural",
    "arts_centre" = "cultural",
    "artwork" = "cultural",
    "museum" = "cultural",
    "theatre"= "cultural",
    "cinema" = "cultural",
    "zoo" = "cultural",
    "community_centre" = "cultural",
    "theme_park" = "cultural",
    
    "college" = "educational",
    "university" = "educational",
    
    "school" = "educational children",
    "kindergarten"= "educational children",

    "computer_shop" = "service",
    "laundry" = "service",
    "hairdresser" = "service",
    "water_works" = "service",
    
    "fire_station" = "public service",
    "police" = "public service",
    
    "mall" = "shopping",
    "department_store" = "shopping",
    "market_place" = "shopping",
    
    "biergarten" = "bar",
    "pub" = "bar",
    "cafe" = "restaurant",
    
    "food_court" = "restaurant",
    "picnic_site" = "park",
    "dog_park" = "park",
    "camp_site" = "park",
    
    "christian" = "temple",
    "christian_lutheran" = "temple",
    "muslim_sunni" = "temple",
    "christian_orthodox" = "temple",
    "jewish" = "temple",
    "buddhist" = "temple",
    
    "orchard" = "farm",
    "allotments" = "farm",
    "farmland" = "farm",
    "farmyard" = "farm",
    
    "meadow" = "green",
    "nature_reserve" = "green",
    "forest" = "green",
    "bus_station" = "bus_stop",
    "railway_station" = "railway_halt",
    x
  )
}

df_centers <- df_centers %>% 
  filter(!object_type %in% drop_lst) %>% 
  mutate(object_type = map_chr(object_type, rename_objects)) %>% 
  filter(!object_type %in% c("recycling"))

# Group by regions ----
df_by_reg <- df_centers %>% 
  as.data.frame() %>% 
  sf::st_as_sf(coords = c(4, 5)) %>% 
  sf::st_set_crs(4326) %>% 
  sf::st_transform(2163) %>% 
  st_join(st_as_sf(regions) %>% st_set_crs(4326) %>% st_transform(2163)) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  filter(!is.na(nazwa_dzie)) %>% 
  select(id, object_type, object_name, nazwa_dzie)

df_by_reg$object_type %>% unique() %>% sort()

df_by_reg_dict <- df_by_reg %>%
  group_by(nazwa_dzie, object_type) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = object_type, values_from = n)

regions@data <- regions@data %>% 
  left_join(df_by_reg_dict, by = "nazwa_dzie")



# Visualization presets ----
measures_lst <- colnames(regions@data)

ccolors <- RColorBrewer::brewer.pal(5, "RdYlGn")#"Blues"

switch_color <- function(x, vvals){
  case_when(
    x<vvals[1] ~ ccolors[1],
    x<vvals[2] ~ ccolors[2],
    x<vvals[3] ~ ccolors[3],
    x<vvals[4] ~ ccolors[4],
    is.na(x) ~ "gray",
    TRUE ~ ccolors[5]
  )
}

switch_label <- function(x, vvals){
  case_when(
    x<vvals[1] ~ paste0("< ", vvals[1]),
    x<vvals[2] ~ paste0("< ", vvals[2]),
    x<vvals[3] ~ paste0("< ", vvals[3]),
    x<vvals[4] ~ paste0("< ", vvals[4]),
    is.na(x) ~ "NA",
    TRUE ~ paste0("> ", vvals[4])
  )
}

# Visualizations ----
vvals <- c(25, 50, 75, 100)
plt_lst <- list()

for (i in seq_along(measures_lst)) {
  var_sel <- measures_lst[i]
  
  regions@data <- regions@data %>% 
    mutate(n = !!sym(var_sel)) %>% 
    mutate(regcolor = switch_color(n, vvals))
  
  plt_lst[measures_lst[i]][[1]] <- regions %>% 
    leaflet() %>% 
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
    addPolygons(
      stroke = 0.5,
      opacity = 1,
      fillColor = regions@data$regcolor,
      weight = 1,
      color = "black",
      popupOptions = paste0(regions@data$nazwa_dzie)
    ) %>%
    addLegend(
      position = "topright",
      colors = c("gray", ccolors),
      labels = c("NA", switch_label(c(0, vvals), vvals)),
      title = paste0("Number of ", var_sel, "s in Warsaw in May-2023")
    ) %>% 
    setView(lng = 21.05, lat = 52.23, zoom = 11)
}

# Explore plots ----
regions@data %>% 
  as_tibble() %>% 
  select(-nazwa_dzie, -n, -regcolor) %>% 
  cor(use = "complete.obs")

plt_lst$drain
regions@data %>% as_tibble()

# Save ----
write_rds(df_centers, paste0("data/clean/points", filne_name, ".rds"))
write_rds(regions, paste0("data/clean/by regions", filne_name, ".rds"))