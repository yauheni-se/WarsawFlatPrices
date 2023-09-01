# Presets ----
setwd("C:/Projects/WarsawFlatPrices/")

library(sf)
library(leaflet)
library(tidyverse)
library(lubridate)
library(geosphere)
library(tictoc)
library(doParallel)
options(scipen = 999)

rename_objects <- function(x) {
  switch(
    x,
    "beauty_shop" = "beauty shop",
    "bicycle_rental" = "bike rent",
    "educational" = "college/university",
    "educational children" = "school/kindergarden",
    "fast_food" = "fast food",
    "parking" = "street parking",
    "parking_bicycle" = "bike parking",
    "parking_multistorey" = "parking",
    "parking_underground" = "parking",
    "vending_parking" = "parking",
    "railway_halt" = "train stop",
    "tram_stop" = "tram stop",
    "bus_stop" = "bus stop",
    "subway" = "subway stop",
    "health" = "healthcare institution",
    "public office" = "public institution",
    "nursing_home" = "healthcare institution",
    "fuel" = "gas station",
    "apteka" = "pharmacy",
    "wetland"  ="swamps",
    "water" = "water object",
    "reservoir" = "water object",
    "shopping" = "shopping mall",
    "sport" = "sport object",
    "factory" = "industrial",
    "manufacture" = "industrial",
    "wastewater_plant" = "industrial",
    "garages" = "garage",
    x
  )
}
regions <- read_rds("data/clean/regions.rds")

df_centers <- data.frame(rgeos::gCentroid(regions, byid=TRUE)) %>% 
  as_tibble() %>% 
  rename(center_x = x, center_y = y)
df_centers[11, 1] <- 21.018

sel_icon <- makeIcon(iconUrl = "C:/Projects/WarsawFlatPrices/icons/circle.png", iconWidth = 5, iconHeight = 5)

# Read & clean spatial objects ----
roads <- read_rds("data/clean/points/roads.rds")

roads <- roads %>% 
  mutate(object_type = str_replace(object_type, "_link", "")) %>% 
  mutate(object_type = ifelse(str_detect(object_type, "track_grade"), "track_grade", object_type)) %>% 
  mutate(object_type = ifelse(object_type=="bridleway", "path", object_type)) %>% 
  mutate(object_type = ifelse(object_type=="pedestrian", "footway", object_type)) %>% 
  mutate(object_type = ifelse(object_type=="living_street", "residential", object_type)) %>% 
  filter(object_type != "unclassified") %>% 
  mutate(object_type = paste0("road_", object_type))


beaches <- read_rds("data/clean/points/beaches.rds")
beaches <- beaches %>% 
  mutate(object_type_ext = ifelse(!is.na(object_name), "beach_known", "beach")) %>% 
  mutate(object_type_ext = ifelse(str_detect(object_name, "natury"), "beach_nude", object_type_ext)) %>% 
  select(-object_type) %>% 
  rename(object_type = object_type_ext)

churches <- read_rds("data/clean/points/churches.rds")
churches <- churches %>% 
  filter(!is.na(object_name)) %>% 
  mutate(
    object_type2 = ifelse(
      str_detect(iconv(object_name, to='ASCII//TRANSLIT'), "KoL>ciAl|Parafia|Kaplica|Katedra|Sanktuarium|kaplica|Zakon|Bazylika|Archikatedra|Apostolat|Matki"),
      "temple_catholic",
      "temple_other")
  ) %>% 
  select(-object_type) %>% 
  rename(object_type = object_type2)

trees <- read_rds("data/clean/points/nature.rds")
trees <- trees %>% 
  mutate(object_type = ifelse(!is.na(object_name), "attraction", object_type))

places <- read_rds("data/clean/points/parks.rds")
places2 <- read_rds("data/clean/points/places2.rds")
railways <- read_rds("data/clean/points/railways.rds")
railways <- railways %>% 
  filter(!is.na(object_name)) %>% 
  filter(str_detect(object_name, "WKD|Linia metra")) %>% 
  mutate(object_type = ifelse(object_type == "rail", "railway_stop", "subway_entrance"))

traffic <- read_rds("data/clean/points/traffic.rds")
traffic2 <- read_rds("data/clean/points/traffic2.rds")
transport2 <- read_rds("data/clean/points/transport2.rds")
transport <- read_rds("data/clean/points/transport.rds") %>% 
  filter(object_type != "apron") %>% 
  mutate(object_type = ifelse(object_type == "airfield", "airport", object_type))
water <- read_rds("data/clean/points/water.rds")
waterways <- read_rds("data/clean/points/waterways.rds")
buildings <- read_rds("data/clean/points/buildings.rds")

# Unite all spatial objects ----
df <- places %>% 
  bind_rows(places2) %>% 
  distinct(.keep_all = TRUE) %>% 
  bind_rows(beaches, churches, trees, transport2, water) %>% 
  bind_rows(
    traffic2 %>% 
      filter(!object_type %in% c("motorway_junction", "stop")) %>% 
      bind_rows(traffic) %>% 
      distinct(.keep_all = TRUE)
  ) %>% 
  bind_rows(waterways %>% filter(!object_type %in% c("canal", "river", "stream", "riverbank"))) %>%
  bind_rows(railways) %>%
  bind_rows(roads) %>%
  bind_rows(buildings) %>% 
  bind_rows(transport) %>% 
  distinct(.keep_all = TRUE) %>%
  mutate(object_type = map_chr(object_type, rename_objects)) %>%
  filter(!object_type %in% c("taxi", "water_tower", "riverbank", "drain", "abandoned", "beach_known", "beach_nude", "beach")) %>%
  filter(!is.na(object_type)) %>% 
  mutate(id = paste0("obj_", row_number()))

subways <- df %>% 
  filter(object_type == "bus stop") %>% 
  filter(!is.na(object_name)) %>% 
  filter(str_detect(object_name, "Metro|Dworzec Gda")) %>% 
  mutate(object_name = str_replace_all(object_name, "[0-9]", "") %>% str_trim()) %>% 
  distinct(object_name, .keep_all = TRUE) %>% 
  mutate(object_type = "subway_entrance")

df <- df %>% 
  filter(object_type != "subway_entrance") %>% 
  bind_rows(subways) %>% 
  mutate(id = paste0("obj_", row_number())) %>% 
  distinct(object_type, object_name, x, y, .keep_all = TRUE) %>% 
  mutate(object_type = str_replace(object_type, "\\/| ", "_"))

rm(beaches, churches, trees, places, places2, railways, traffic, traffic2, transport2, water, waterways, buildings, transport, subways, roads)


# Assign region to spatial objects ----
df <- df %>% 
  as.data.frame() %>% 
  sf::st_as_sf(coords = c(4, 5)) %>% 
  sf::st_set_crs(4326) %>% 
  sf::st_transform(2163) %>% 
  st_join(st_as_sf(regions) %>% st_set_crs(4326) %>% st_transform(2163)) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  left_join(df %>% select(id, x, y), by = "id") %>% 
  select(-geometry) %>% 
  filter(!is.na(nazwa_dzie)) %>% 
  rename(region = nazwa_dzie)

# Join spatial objects with map ----
df_by_region <- df %>%
  group_by(region, object_type) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = object_type, values_from = n)# %>% 
  #mutate_if(is.numeric, function(x) {ifelse(is.na(x), 0, x)})# group number of each objects by regions

regions@data <- regions@data %>% 
  rename(region = nazwa_dzie) %>% 
  left_join(df_by_region, by = "region") %>% 
  bind_cols(df_centers) %>%
  mutate(
    pop = c(58633, 186834, 225916, 151158, 51172, 26380, 86399, 49280, 151432, 101979, 60855, 67373, 124240, 24670, 80988, 133478, 153100, 129169),
    area = c(8.47, 22.38, 35.42, 19.26, 36.73, 22.94, 79.71, 28.63, 43.79, 15.57, 11.31, 9.35, 24.33, 19.3, 9.72, 32.34, 73.00, 24.95)
  ) # join with map, add info about population and area


# Read & clean GUS data ----
replace_reg <- function(x) {str_replace(x, " \\- dzielnica \\(8\\)", "")}

df_age <- readxl::read_excel("data/gus/age.xlsx") %>% 
  mutate(reg = replace_reg(reg)) %>% 
  rename(`70+`=`70 i wiêcej`) %>% 
  mutate(
    `0-19`= `0-14`+`15-19`,
    `20-29` = `20-24`+`25-29`,
    `30-39` = `30-34`+`35-39`,
    `40-59` = `40-44`+`45-49`+`50-54`+`55-59`,
    `60+` = `60-64`+`65-69`+`70+`,
    .keep = "unused"
  ) %>%
  rowwise() %>% 
  mutate(total = sum(c_across(colnames(.)[colnames(.)!="reg"]))) %>% 
  ungroup() %>% 
  mutate_at(vars(-total, -reg), funs(. / total*100)) %>% 
  select(-total) %>% 
  `colnames<-`(paste0("age_", colnames(.))) %>% 
  rename(region = age_reg) %>% 
  mutate_if(is.numeric, round, 2)

df_growth <- readxl::read_excel("data/gus/growth.xlsx") %>% 
  mutate(reg = replace_reg(reg)) %>% 
  rename(region = reg)

df_marriage <- readxl::read_excel("data/gus/marriage.xlsx") %>% 
  mutate(reg = replace_reg(reg)) %>% 
  rename(region = reg)

df_sex <- readxl::read_excel("data/gus/sex.xlsx") %>% 
  mutate(reg = replace_reg(reg)) %>% 
  rename(region = reg) %>% 
  mutate(
    t = m+f
  ) %>% 
  mutate(
    male = round(m/t, 4)*100,
    female = round(f/t, 4)*100,
    .keep = "unused"
  )

df_ent <- readxl::read_excel("data/gus/os_fiz.xlsx") %>% 
  mutate(reg = replace_reg(reg)) %>% 
  rename(region = reg, enterpreneir=`osoby fizyczne`)

df_unemp <- readxl::read_excel("data/gus/bezrob.xls") %>% 
  mutate(reg = replace_reg(reg)) %>% 
  mutate(`employment percentage` = 100-`unemp_35-44`) %>%
  select(region = reg, emp = `employment percentage`)

df_sp <- readxl::read_excel("data/gus/spolk.xlsx") %>% 
  mutate(reg = replace_reg(reg)) %>% 
  mutate(company = panstwowe+Spoldzielnie+Handlowe+Cywilne, .keep = "unused") %>% 
  rename(region = reg)

# Join GUS data with map ----
regions@data <- regions@data %>% 
  left_join(df_age, by = "region") %>% 
  left_join(df_growth, by = "region") %>% 
  left_join(df_marriage, by = "region") %>% 
  left_join(df_sex, by = "region") %>% 
  left_join(df_ent, by = "region") %>% 
  left_join(df_unemp, by = "region") %>% 
  left_join(df_sp, by = "region")


by_areas <- regions@data %>%
  select(-c(region, center_x, center_y)) %>% 
  mutate_at(vars(-area), funs(. / area)) %>% 
  select(-area) %>%
  mutate_all(round, 2) %>% 
  `colnames<-`(paste0(colnames(.), " per sq.km"))

by_pops <- regions@data %>%
  select(-c(region, center_x, center_y, area)) %>% 
  mutate_at(vars(-pop), funs(. / pop* 1000)) %>% 
  select(-pop) %>%
  mutate_all(round, 2) %>% 
  `colnames<-`(paste0(colnames(.), " per 1k person"))

regions@data <- regions@data %>% 
  bind_cols(by_pops) %>%
  bind_cols(by_areas)

measures_lst <- colnames(regions@data)

# Visualize spatial and GUS data per atm ----
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
plt_lst <- list()
for (i in 2:length(measures_lst)) {
  var_sel <- measures_lst[i]
  
  tmp_markers <- df %>% filter(object_type == str_replace(var_sel, " per sq.km| per 1k person", ""))
  
  regions@data <- regions@data %>% 
    mutate(n = !!sym(var_sel))
  
  vvals <- quantile(regions@data$n, names = FALSE, probs = c(0.20, 0.4, 0.6, 0.8), na.rm = TRUE) %>% round(2)
  
  regions@data <- regions@data%>% 
    mutate(regcolor = switch_color(n, vvals))
  
  plt_lst[measures_lst[i]][[1]] <- regions %>% 
    leaflet() %>% 
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
    addPolygons(
      stroke = 0.5,
      opacity = 1,
      fillColor = regions@data$regcolor,
      weight = 0.8,
      color = "black",
      smoothFactor = 0.8,
      highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = TRUE),
      popup = paste0(regions@data$region)
    ) %>%
    addMarkers(
      lng = tmp_markers$x,
      lat = tmp_markers$y,
      icon = sel_icon
    ) %>% 
    addLabelOnlyMarkers(
      lng = regions@data$center_x,
      lat = regions@data$center_y,
      label = regions@data$n,
      labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE)
    ) %>%
    addLegend(
      position = "topright",
      colors = c("gray", ccolors),
      labels = c("NA", switch_label(c(0, vvals), vvals)),
      title = paste0(var_sel, " distribution in Warsaw in June-2023")
    ) %>% 
    setView(lng = 21.05, lat = 52.23, zoom = 11)
  
  regions@data$n <- NULL
  regions@data$regcolor <- NULL
}

# Explore visuals ----
df$object_type %>% unique() %>% sort()

plt_lst[[163]]  # 119-163 - per sq. km #155 

#plt_lst$garage # footway service residential

# Read & clean flats data ----
flats <- read_rds("data/clean/flats.rds")

flats <- flats %>% 
  mutate(price_per_m = price/area) %>% 
  filter(!is.na(price_per_m)) %>% 
  mutate(is_first_time = ifelse(date_created==date_created_first, 1L, 0L)) %>% 
  mutate(year_created_first = year(date_created_first)) %>% 
  mutate(has_name = ifelse(!is.na(object_name), 1L, 0L)) %>% 
  mutate(free_from_year = year(free_from)) %>% #group_by(free_from_year) %>% summarise(n = n()) %>% arrange(desc(n))
  select(-c(date_created_first, date_created, object_name, agency_name, price, free_from)) %>%
  select(-c(loc, loc_label, district)) %>%  # ?
  rename(x=lon, y=lat)
  
regions <- read_rds("data/clean/regions.rds")
regions@data <- regions@data %>% rename(region = nazwa_dzie)

flats <- flats %>% 
  select(id, x, y, everything()) %>% 
  as.data.frame() %>% 
  sf::st_as_sf(coords = c(2, 3)) %>% 
  sf::st_set_crs(4326) %>% 
  sf::st_transform(2163) %>% 
  st_join(st_as_sf(regions) %>% st_set_crs(4326) %>% st_transform(2163)) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  left_join(flats %>% select(id, x, y), by = "id") %>% 
  filter(!is.na(region))
# Calculate distances ----
write_rds(flats, "data/clean/points/flats.rds")
write_rds(df, "data/clean/points/objects.rds")
calc_geo_dist <- function(x_ref, y_ref, x, y) {distGeo(c(x, y), c(x_ref, y_ref))}
calc_dist_min <- function(x, y, o_type) {
  ref_x <- df %>% filter(object_type == o_type) %>% pull(x)
  ref_y <- df %>% filter(object_type == o_type) %>% pull(y)
  mapply(calc_geo_dist, ref_x, ref_y, MoreArgs = list(x=x, y=y), USE.NAMES = FALSE) %>% min(na.rm = TRUE)
}
calc_dist_800 <- function(x, y, o_type) {
  ref_x <- df %>% filter(object_type == o_type) %>% pull(x)
  ref_y <- df %>% filter(object_type == o_type) %>% pull(y)
  mapply(calc_geo_dist, ref_x, ref_y, MoreArgs = list(x=x, y=y), USE.NAMES = FALSE) %>% .[.<=800] %>% length()
}
calc_dist_400 <- function(x, y, o_type) {
  ref_x <- df %>% filter(object_type == o_type) %>% pull(x)
  ref_y <- df %>% filter(object_type == o_type) %>% pull(y)
  mapply(calc_geo_dist, ref_x, ref_y, MoreArgs = list(x=x, y=y), USE.NAMES = FALSE) %>% .[.<=400] %>% length()
}
calc_dist_1200 <- function(x, y, o_type) {
  ref_x <- df %>% filter(object_type == o_type) %>% pull(x)
  ref_y <- df %>% filter(object_type == o_type) %>% pull(y)
  mapply(calc_geo_dist, ref_x, ref_y, MoreArgs = list(x=x, y=y), USE.NAMES = FALSE) %>% .[.<=1200] %>% length()
}

# Calculate distances to all objects besides roads ----
object_types <- df %>% filter(!str_detect(object_type, "road_|tree")) %>% .$object_type %>% unique() %>% sort()
tic()
cl <- makeCluster(13)
registerDoParallel(cl)
flats_dist_lst <- foreach(
  i = 1:nrow(flats),
  .inorder = TRUE,
  combine = rbind,
  .packages = c("tidyverse", "geosphere"),
  .export = c("df"),
  .verbose = TRUE
  ) %dopar% {
    tmp_flats <- flats %>% slice(i)
    for (j in object_types) {
      tmp_flats[[paste0("dist_", j)]] <- mapply(calc_dist_min, tmp_flats$x, tmp_flats$y, MoreArgs = list(o_type = j))
    }
    tmp_flats <- list(tmp_flats)
    return(tmp_flats)
  }
stopCluster(cl)
toc() # 2 hours
flats_dist <- unlist(flats_dist_lst, recursive = FALSE, use.names = FALSE) %>% 
  data.table::rbindlist(use.names = FALSE) %>% 
  as_tibble()
write_rds(flats_dist, "data/clean/flats_dist.rds")


# Calculate distances to roads ----
object_types <- df %>% filter(str_detect(object_type, "road_")) %>% .$object_type %>% unique() %>% sort()
tic()
cl <- makeCluster(13)
registerDoParallel(cl)
flats_dist_roads_lst <- foreach(
  i = 1:nrow(flats_dist),
  .inorder = TRUE,
  combine = rbind,
  .packages = c("tidyverse", "geosphere"),
  .export = c("df"),
  .verbose = TRUE
) %dopar% {
  tmp_flats <- flats_dist %>% slice(i)
  for (j in object_types) {
    tmp_flats[[paste0("dist_", j)]] <- mapply(calc_dist_min, tmp_flats$x, tmp_flats$y, MoreArgs = list(o_type = j))
  }
  tmp_flats <- list(tmp_flats)
  return(tmp_flats)
}
stopCluster(cl) 
toc() # 23h
flats_dist_roads <- unlist(flats_dist_roads_lst, recursive = FALSE, use.names = FALSE) %>% 
  data.table::rbindlist(use.names = FALSE) %>% 
  as_tibble()
write_rds(flats_dist_roads, "data/clean/flats_dist_roads.rds")


# Calculate n objects within 800 meters ----
object_types <- df %>% filter(!str_detect(object_type, "road_|tree")) %>% .$object_type %>% unique() %>% sort()
tic()
cl <- makeCluster(13)
registerDoParallel(cl)
flats_n_lst <- foreach(
  i = 1:nrow(flats),
  .inorder = TRUE,
  combine = rbind,
  .packages = c("tidyverse", "geosphere"),
  .export = c("df"),
  .verbose = TRUE
) %dopar% {
  tmp_flats <- flats_dist_roads %>% slice(i)
  for (j in object_types) {
    tmp_flats[[paste0(j, "s_800")]] <- mapply(calc_dist_800, tmp_flats$x, tmp_flats$y, MoreArgs = list(o_type = j))
  }
  tmp_flats <- list(tmp_flats)
  return(tmp_flats)
}
stopCluster(cl)
toc() # 5 hours
flats_n <- unlist(flats_n_lst, recursive = FALSE, use.names = FALSE) %>% 
  data.table::rbindlist(use.names = FALSE) %>% 
  as_tibble()
write_rds(flats_n, "data/clean/flats_n.rds")
# Calculate n objects within 400 meters ----
write_rds(flats_dist_lst, "data/clean/list1.rds")
write_rds(flats_dist_roads_lst, "data/clean/list2.rds")
write_rds(flats_n_lst, "data/clean/list3.rds")
rm(flats_dist_lst, flats_dist_roads_lst, flats_n_lst)
gc()


object_types <- df %>% filter(!str_detect(object_type, "road_|tree")) %>% .$object_type %>% unique() %>% sort()
tic()
cl <- makeCluster(13)
registerDoParallel(cl)
flats_n_lst2 <- foreach(
  i = 1:nrow(flats),
  .inorder = TRUE,
  combine = rbind,
  .packages = c("tidyverse", "geosphere"),
  .export = c("df"),
  .verbose = TRUE
) %dopar% {
  tmp_flats <- flats_n %>% slice(i)
  for (j in object_types) {
    tmp_flats[[paste0(j, "s_400")]] <- mapply(calc_dist_400, tmp_flats$x, tmp_flats$y, MoreArgs = list(o_type = j))
  }
  tmp_flats <- list(tmp_flats)
  return(tmp_flats)
}
stopCluster(cl)
toc() # 2h
flats_n2 <- unlist(flats_n_lst2, recursive = FALSE, use.names = FALSE) %>% 
  data.table::rbindlist(use.names = FALSE) %>% 
  as_tibble()
write_rds(flats_n2, "data/clean/flats_n2.rds")

# Calculate n objects within 1200 meters ----
object_types <- df %>% filter(!str_detect(object_type, "road_|tree")) %>% .$object_type %>% unique() %>% sort()
tic()
cl <- makeCluster(13)
registerDoParallel(cl)
flats_n_lst3 <- foreach(
  i = 1:nrow(flats),
  .inorder = TRUE,
  combine = rbind,
  .packages = c("tidyverse", "geosphere"),
  .export = c("df"),
  .verbose = TRUE
) %dopar% {
  tmp_flats <- flats_n2 %>% slice(i)
  for (j in object_types) {
    tmp_flats[[paste0(j, "s_1200")]] <- mapply(calc_dist_1200, tmp_flats$x, tmp_flats$y, MoreArgs = list(o_type = j))
  }
  tmp_flats <- list(tmp_flats)
  return(tmp_flats)
}
stopCluster(cl)
toc() # 2.3
flats_n3 <- unlist(flats_n_lst3, recursive = FALSE, use.names = FALSE) %>% 
  data.table::rbindlist(use.names = FALSE) %>% 
  as_tibble()
write_rds(flats_n3, "data/clean/flats_n3.rds")
# Calculate distance to the river ----
write_rds(flats_n_lst2, "data/clean/list4.rds")
write_rds(flats_n_lst3, "data/clean/list5.rds")
rm(flats_n_lst2, flats_n_lst3)

calc_dist_river <- function(x, y) {
  ref_x <- df_river %>% pull(x)
  ref_y <- df_river %>% pull(y)
  mapply(calc_geo_dist, ref_x, ref_y, MoreArgs = list(x=x, y=y), USE.NAMES = FALSE) %>% min(na.rm = TRUE)
}

ww <- read_rds("data/clean/waterways.rds")
ww <- ww[ww$fclass== "river", ]
ww <- ww[ww$name == "Wis³a", ]
rivers_lst <- lapply(ww@lines, function(x) {x@Lines[[1]]@coords %>% as.data.frame() %>% `colnames<-`(c("x", "y"))})
df_river <- rivers_lst %>% 
  data.table::rbindlist() %>% 
  as_tibble()

tic()
flats_n4 <- flats_n3 %>% mutate(dist_river = mapply(calc_dist_river, .$x, .$y))
toc()# 9 min

write_rds(flats_n4, "data/clean/flats_n4.rds")

# Calculate distance to the CBD ----
calc_dist_cbd <- function(x, y) {
  ref_x <- df_cbd %>% pull(x)
  ref_y <- df_cbd %>% pull(y)
  mapply(calc_geo_dist, ref_x, ref_y, MoreArgs = list(x=x, y=y), USE.NAMES = FALSE) %>% min(na.rm = TRUE)
}
df_cbd <- df %>% filter(str_detect(object_name, "Kultury i Nauki"))
flats_n5 <- flats_n4 %>% mutate(dist_cbd = mapply(calc_dist_cbd, .$x, .$y))

write_rds(flats_n5, "data/clean/flats_n5.rds")
# Misc visuals ----

df %>% 
  filter(object_type != "tree") %>% 
  mutate(object_type = categorise_objects(object_type)) %>% 
  mutate(object_type = ifelse(str_detect(object_type, "road|stop|bus_statio|entrance|airport"), "drogi_konumikacja", object_type)) %>% 
  mutate(object_type = ifelse(str_detect(object_type, "parking|bike|car|garage"), "samochody_rowery", object_type)) %>% 
  group_by(object_type) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))


categorise_objects <- function(x) {
  case_when(
    x %in% c('food_shop', 'beauty_shop', 'pharmacy', 'jeweller', 'shopping_mall', 'shop') ~ "sklepy",
    x %in% c('fast_food', 'bar', 'restaurant') ~ "gastronomia",
    x %in% c('health_institution', 'public_institution', 'public_service', 'college_university', 'school_kindergarden', 'dormitory', 'temple_catholic', 'temple_other') ~ "public",
    x %in% c('sport_object', 'cultural', 'attraction', 'nightclub') ~ "rekreacja",
    x %in% c('swamps', 'water_object', 'river', 'park') ~ "przyroda",
    x %in% c('car_service', 'garage', 'gas_station', 'parking', 'bike parking', 'bike_rent') ~ "cars",
    x %in% c('playground', 'office', 'industrial', 'construction', 'service', 'bank', 'renthouse', 'graveyard', 'prison') ~ "inne",
    TRUE ~ x
  )
}

regions %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  addPolygons(
    stroke = 0.5,
    opacity = 1,
    fillColor = fregions@data$regcolor,
    weight = 0.8,
    color = "black",
    smoothFactor = 0.8,
    highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = TRUE),
    popup = paste0(fregions@data$region)
  ) %>%
  addMarkers(
    lng = flats$x,
    lat = flats$y,
    icon = sel_icon
  ) %>% 
  addLabelOnlyMarkers(
    lng = fregions@data$center_x,
    lat = fregions@data$center_y,
    label = fregions@data$price_per_m,
    labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE)
  ) %>% 
  addLegend(
    position = "topright",
    colors = c("gray", ccolors),
    labels = c("NA", switch_label(c(0, vvals), vvals)),
    title = paste0("Number of primary market's flats for sale in Warsaw in June-2023")
  ) %>%
  setView(lng = 21.05, lat = 52.23, zoom = 11)
