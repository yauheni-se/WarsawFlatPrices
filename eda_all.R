# Presets ----
setwd("C:/Projects/HousePricesWarsaw/")

library(sf)
library(leaflet)
library(tidyverse)
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
    x
  )
}

regions <- read_rds("data/clean/regions.rds")

# Unite tables ----
beaches <- read_rds("data/clean/points/beaches.rds")
churches <- read_rds("data/clean/points/churches.rds")
trees <- read_rds("data/clean/points/nature.rds")
places <- read_rds("data/clean/points/parks.rds")
places2 <- read_rds("data/clean/points/places2.rds")
railways <- read_rds("data/clean/points/railways.rds")
traffic <- read_rds("data/clean/points/traffic.rds")
traffic2 <- read_rds("data/clean/points/traffic2.rds")
transport2 <- read_rds("data/clean/points/transport2.rds")
water <- read_rds("data/clean/points/water.rds")
waterways <- read_rds("data/clean/points/waterways.rds")

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
  bind_rows(railways %>% filter(object_type == "subway")) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(object_type = map_chr(object_type, rename_objects)) %>%
  filter(!object_type %in% c("taxi", "water_tower", "wastewater_plant", "riverbank")) %>%
  mutate(id = paste0("obj_", row_number()))
rm(beaches, churches, trees, places, places2, railways, traffic, traffic2, transport2, water, waterways)


# Add assigned region ----
df %>%
  group_by(object_type) %>% 
  summarise(n = n()) %>% 
  arrange(n)

df <- df %>% 
  as.data.frame() %>% 
  sf::st_as_sf(coords = c(4, 5)) %>% 
  sf::st_set_crs(4326) %>% 
  sf::st_transform(2163) %>% 
  st_join(st_as_sf(regions) %>% st_set_crs(4326) %>% st_transform(2163)) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  left_join(df %>% select(id, x, y), by = "id") %>% 
  select(-geometry)

# Prepare data for visualizations ----
df_by_reg_dict <- df %>%
  group_by(nazwa_dzie, object_type) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = object_type, values_from = n)

df_centers <- data.frame(rgeos::gCentroid(regions, byid=TRUE)) %>% 
  as_tibble() %>% 
  rename(center_x = x, center_y = y)

df_centers[11, 1] <- 21.018

regions@data <- regions@data %>% 
  left_join(df_by_reg_dict, by = "nazwa_dzie") %>% 
  bind_cols(df_centers) %>%
  mutate(
    pop = c(58633, 186834, 225916, 151158, 51172, 26380, 86399, 49280, 151432, 101979, 60855, 67373, 124240, 24670, 80988, 133478, 153100, 129169),
    area = c(8.47, 22.38, 35.42, 19.26, 36.73, 22.94, 79.71, 28.63, 43.79, 15.57, 11.31, 9.35, 24.33, 19.3, 9.72, 32.34, 73.00, 24.95)
  )
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
  rename(nazwa_dzie = age_reg) %>% 
  mutate_if(is.numeric, round, 2)

df_growth <- readxl::read_excel("data/gus/growth.xlsx") %>% 
  mutate(reg = replace_reg(reg)) %>% 
  rename(nazwa_dzie = reg)

df_marriage <- readxl::read_excel("data/gus/marriage.xlsx") %>% 
  mutate(reg = replace_reg(reg)) %>% 
  rename(nazwa_dzie = reg)

df_sex <- readxl::read_excel("data/gus/sex.xlsx") %>% 
  mutate(reg = replace_reg(reg)) %>% 
  rename(nazwa_dzie = reg) %>% 
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
  rename(nazwa_dzie = reg, enterpreneir=`osoby fizyczne`)

df_unemp <- readxl::read_excel("data/gus/bezrob.xls") %>% 
  mutate(reg = replace_reg(reg)) %>% 
  mutate(`employment percentage` = 100-`unemp_35-44`) %>%
  select(nazwa_dzie = reg, emp = `employment percentage`)

df_sp <- readxl::read_excel("data/gus/spolk.xlsx") %>% 
  mutate(reg = replace_reg(reg)) %>% 
  mutate(company = panstwowe+Spoldzielnie+Handlowe+Cywilne, .keep = "unused") %>% 
  rename(nazwa_dzie = reg)

regions@data <- regions@data %>% 
  left_join(df_age, by = "nazwa_dzie") %>% 
  left_join(df_growth, by = "nazwa_dzie") %>% 
  left_join(df_marriage, by = "nazwa_dzie") %>% 
  left_join(df_sex, by = "nazwa_dzie") %>% 
  left_join(df_ent, by = "nazwa_dzie") %>% 
  left_join(df_unemp, by = "nazwa_dzie") %>% 
  left_join(df_sp, by = "nazwa_dzie")


by_areas <- regions@data %>%
  select(-c(nazwa_dzie, center_x, center_y)) %>% 
  mutate_at(vars(-area), funs(. / area)) %>% 
  select(-area) %>%
  mutate_all(round, 2) %>% 
  `colnames<-`(paste0(colnames(.), " per sq.km"))

by_pops <- regions@data %>%
  select(-c(nazwa_dzie, center_x, center_y, area)) %>% 
  mutate_at(vars(-pop), funs(. / pop* 1000)) %>% 
  select(-pop) %>%
  mutate_all(round, 2) %>% 
  `colnames<-`(paste0(colnames(.), " per 1k person"))

regions@data <- regions@data %>% 
  bind_cols(by_pops) %>%
  bind_cols(by_areas)

measures_lst <- colnames(regions@data)

# Funcs for visualizations ----
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
  
  regions@data <- regions@data %>% 
    mutate(n = !!sym(var_sel))
  
  #vvals <- c(25, 50, 75, 100)
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
      popup = paste0(regions@data$nazwa_dzie)
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
      title = paste0(var_sel, " distribution in Warsaw in May-2023")
    ) %>% 
    setView(lng = 21.05, lat = 52.23, zoom = 11)
  
  regions@data$n <- NULL
  regions@data$regcolor <- NULL
}

# Explore visuals ----
plt_lst$`company per 1k person`

#87+
#46+

# subway stops: definitely corrupted
# swamps too?
# tram stops: wola?
# trees: wola?
# beaches: corrupted?
# take park area, not parks; the same with parking, etc
# Add otodom data ----
mode <- function(x){
  x_levels <- x %>% as.factor() %>% levels()
  x_mode_index <- x %>% as.factor() %>% tabulate() %>% which.max()
  x_levels[x_mode_index]
}

switch_color <- function(x, vvals){
  case_when(
    x<=vvals[1] ~ ccolors[1],
    x<=vvals[2] ~ ccolors[2],
    x<=vvals[3] ~ ccolors[3],
    x<=vvals[4] ~ ccolors[4],
    is.na(x) ~ "gray",
    TRUE ~ ccolors[5]
  )
}

flats <- read_rds("data/clean/flats.rds")

flats <- flats %>% 
  filter(market == "primary") %>% 
  mutate(price_per_m = price/area) %>% 
  select(-c(date_created, date_created_first, loc, loc_label, agency_name, description, price, free_from, market, district, object_name)) %>% 
  rename(x=lon, y=lat) %>% 
  mutate_all(as.character) %>% 
  mutate(across(all_of(c("area", "price_per_m", "build_year", "rent", "x", "y")), as.numeric)) %>% 
  mutate_if(is.character, function(x){ifelse(is.na(x)|x=="", "unknown", x)}) %>% 
  mutate_if(is.character, function(x){ifelse(x=="1", "yes", ifelse(x=="0", "no", x))}) %>% 
  select(id, x, y, everything())

regions <- read_rds("data/clean/regions.rds")

flats <- flats %>% 
  as.data.frame() %>% 
  sf::st_as_sf(coords = c(2, 3)) %>% 
  sf::st_set_crs(4326) %>% 
  sf::st_transform(2163) %>% 
  st_join(st_as_sf(regions) %>% st_set_crs(4326) %>% st_transform(2163)) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  left_join(flats %>% select(id, x, y), by = "id") %>% 
  select(-geometry)

flats_gr1 <- flats %>% 
  group_by(nazwa_dzie) %>% 
  summarise_if(is.character, mode) %>% 
  mutate(across(all_of(c("rooms_num", "floor_no", "building_floors_num")), function(x){ifelse(x=="yes", "1", x)})) %>% 
  select(-id)
flats_gr2 <- flats %>% 
  group_by(nazwa_dzie) %>% 
  summarise_if(is.numeric, median, na.rm = TRUE) %>% 
  select(-x, -y) %>% 
  mutate_if(is.numeric, round, 0)
flats_gr3 <- flats %>% 
  group_by(nazwa_dzie) %>%
  summarise(number_of_offers = n())

flats <- flats_gr1 %>% 
  left_join(flats_gr2, by = "nazwa_dzie") %>% 
  left_join(flats_gr3, by = "nazwa_dzie")

df_centers <- data.frame(rgeos::gCentroid(regions, byid=TRUE)) %>% 
  as_tibble() %>% 
  rename(center_x = x, center_y = y)
df_centers[11, 1] <- 21.018

regions@data <- regions@data %>% 
  bind_cols(df_centers)


measures_lst <- colnames(flats) %>% .[. != "nazwa_dzie"]


# Visualize otodom ----
plt_lst <- list()
for (i in seq_along(measures_lst)) {
  
  var_type <- flats[[measures_lst[i]]] %>% typeof()
 
  
  if (var_type == "character") {
    cols_unique_n <- flats[[measures_lst[i]]] %>% n_distinct()
    ccolors <- RColorBrewer::brewer.pal(max(3, cols_unique_n), "RdYlGn") %>% rev()
    vvals <- flats[[measures_lst[i]]] %>% unique()
  } else {
    ccolors <- RColorBrewer::brewer.pal(5, "RdYlGn")
    if (measures_lst[i] %in% c("price_per_m", "rent")) {ccolors <- ccolors %>% rev()}
    vvals <- quantile(flats[[measures_lst[i]]], names = FALSE, probs = c(0.20, 0.4, 0.6, 0.8), na.rm = TRUE) %>% round(2)
  }
  
  regions@data <- regions@data %>% left_join(flats %>% select(nazwa_dzie, n = measures_lst[i]), by = "nazwa_dzie")
  regions@data <- regions@data %>% mutate(regcolor = switch_color(n, vvals))
  
  plt_lst[[i]] <- regions %>% 
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
      popup = paste0(regions@data$nazwa_dzie)
    ) %>%
    addLabelOnlyMarkers(
      lng = regions@data$center_x,
      lat = regions@data$center_y,
      label = regions@data$n,
      labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE)
    ) %>% 
    addLegend(
      position = "topright",
      colors = NULL,#c("gray", ccolors),
      labels = NULL,#c("NA", switch_label(c(0, vvals), vvals)),
      title = paste0(ifelse(var_type == "character", "dominant ", "median "), measures_lst[[i]], " in Warsaw in June-2023")
    ) %>%
    setView(lng = 21.05, lat = 52.23, zoom = 11)
  
  regions@data$n <- NULL
  regions@data$regcolor <- NULL
}
# Explore visuals ----
plt_lst[[5]]

# ----
# FLATS 2-----
flats <- read_rds("data/clean/flats.rds")
flats <- flats %>% filter(market == "primary") %>% select(id, price_per_m, x=lon, y=lat)


sel_icon <- makeIcon(iconUrl = "C:/Projects/HousePricesWarsaw/icons/circle.png", iconWidth = 5, iconHeight = 5)

flats %>% group_by(x, y) %>% summarise(n = n()) %>% ungroup() %>% arrange(desc(n))

fregions@data %>% select(region = nazwa_dzie, center_x, center_y) %>% slice(1)

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
    popup = paste0(fregions@data$nazwa_dzie)
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
