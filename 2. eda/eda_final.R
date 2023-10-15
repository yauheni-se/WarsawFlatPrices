# Presets ----
setwd("C:/Projects/WarsawFlatPrices/")

library(sf)
library(leaflet)
library(tidyverse)
library(lubridate)
library(geosphere)
library(tictoc)
library(doParallel)
library(mapview)
library(plotly)
library(GGally)
library(leafsync)
library(htmltools)
library(htmlwidgets)
library(webshot)
library(gridExtra)
library(RColorBrewer)
options(scipen = 999)

regions <- read_rds("data/clean/regions.rds")
poland <- read_rds("data/poland.rds")
warsaw <- poland[poland$NAME_3 == "Warszawa",]

df_centers <- data.frame(rgeos::gCentroid(regions, byid=TRUE)) %>% 
  as_tibble() %>% 
  rename(center_x = x, center_y = y)
df_centers[11, 1] <- 21.018

sel_icon <- makeIcon(iconUrl = "C:/Projects/WarsawFlatPrices/icons/circle.png", iconWidth = 5, iconHeight = 5)

flats <- read_rds("data/clean/pre_cleaned/flats_n5.rds")

# NUM 0 ----
custom_icons <- iconList(
  red1 = makeIcon("icons/red1.png", 4, 4),
  red2 = makeIcon("icons/red2.png", 4, 4),
  red3 = makeIcon("icons/red3.png", 4, 4),
  red4 = makeIcon("icons/red4.png", 4, 4),
  red5 = makeIcon("icons/red5.png", 4, 4)
)
create_color <- function(x) {
  case_when(
    x >= quantile(x, probs = 0.8, names = FALSE, na.rm = TRUE) ~ "red1",
    x >= quantile(x, probs = 0.6, names = FALSE, na.rm = TRUE) ~ "red2",
    x >= quantile(x, probs = 0.4, names = FALSE, na.rm = TRUE) ~ "red3",
    x >= quantile(x, probs = 0.2, names = FALSE, na.rm = TRUE) ~ "red4",
    TRUE ~ "red5"
  )
}

create_label <- function(x) {
  c(
    quantile(x, probs = 0.8, names = FALSE, na.rm = TRUE),
    quantile(x, probs = 0.6, names = FALSE, na.rm = TRUE),
    quantile(x, probs = 0.4, names = FALSE, na.rm = TRUE),
    quantile(x, probs = 0.2, names = FALSE, na.rm = TRUE),
    quantile(x, probs = 0.2, names = FALSE, na.rm = TRUE)
  ) %>% round(2) %>% 
    paste0(c(">", ">", ">", ">", "<"), .)
}


# NUM I ----
cols_num <- flats %>% select(contains("800"), contains("dist"), price_per_m) %>% colnames() %>% sort() 
plt_lst <- list()
if (FALSE) {
  for (i in seq_along(cols_num)) {
    flats_vis <- flats %>% mutate(color = create_color(flats[[cols_num[i]]]))
    plt_lst[[cols_num[i]]] <- regions %>% 
      leaflet(options = leafletOptions(attributionControl=FALSE)) %>% 
      addProviderTiles(providers$Stadia.OSMBright) %>% #CartoDB.PositronNoLabels
      addPolygons(
        stroke = 0.5,
        opacity = 1,
        fillColor = "black",
        weight = 0.8,
        color = "black",
        smoothFactor = 0.8,
        highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = TRUE),
      ) %>%
      addMarkers(
        lng = flats$x,
        lat = flats$y,
        icon = ~custom_icons[flats_vis$color]
      ) %>% 
      addLegend(
        position = "topright",
        colors = c("#bd0026", "#f03b20", "#fd8d3c", "#fecc5c", "#ffffb2"),
        labels = flats[[cols_num[i]]] %>% create_label(),
        title = paste0("Number of primary market's flats for sale in Warsaw in June-2023")
      ) %>%
      setView(lng = 21.05, lat = 52.23, zoom = 11)
  }
}

#plt_lst[[1]]
#' rubbish:
#' 400m
#' 1200m
#' "airports_800", "banks_800", "bars_800", "bike_rents_800" "bus_stations_800" "car_services_800" "college_universitys_800" "constructions_800" "culturals_800"
#' "dormitorys_800", "garages_800" (corrupted variable), "gas_stations_800", "graveyards_800", "healthcare_institutions_800" "jewellers_800", "nightclubs_800", 
#' "parkings_800", "parks_800"(total green_area within 800m instead), "prisons_800", "public_services_800", "railway_stops_800", "shopping_malls_800", "subway_entrances_800",
#' "swampss_800", "temple_catholics_800", "temple_others_800", "train_stops_800", "tram_stops_800"

excluded_vars <- c(
  "airports_800", "banks_800", "bars_800", "bike_rents_800", "bus_stations_800", "car_services_800", "college_universitys_800", "constructions_800", "culturals_800",
  "dormitorys_800", "garages_800", "gas_stations_800", "graveyards_800", "healthcare_institutions_800", "jewellers_800", "nightclubs_800", 
  "parkings_800", "parks_800", "prisons_800", "public_services_800", "railway_stops_800", "shopping_malls_800", "subway_entrances_800",
  "swampss_800", "temple_catholics_800", "temple_others_800", "train_stops_800", "tram_stops_800"
)


flats_cl <- flats %>% 
  select(-ends_with("1200"), -ends_with("400"), -all_of(excluded_vars), -description)

# NUM II ----
cols_num <- flats_cl %>% select(area, price_per_m, starts_with("dist"), contains("year"), ends_with("_800")) %>% colnames() %>% sort() 
plt_lst <- list()
for (i in seq_along(cols_num)) {
  plt_lst[[cols_num[i]]] <- plot_ly(y = flats_cl[[cols_num[i]]], type = "box", name = cols_num[i])# %>% layout(xaxis = list(title = cols_num[i]))
}
plt_lst$area


flats_cl <- flats_cl %>% 
  filter(area <= 17000) %>% 
  mutate(
    build_year = ifelse(build_year < 1560, 1900+build_year, build_year)
  )
# NUM III ----
cols_explore <- flats_cl %>% colnames() %>% .[str_detect(., "_800")] %>% str_replace("s_800", "")
cor_lst <- list()
for (i in seq_along(cols_explore)) {
  cor_lst[[i]] <- flats_cl %>% 
    select(contains(cols_explore[i]), price_per_m) %>% 
    cor() %>% 
    as_tibble(rownames = "var")
}

# NUM DENSITY PLOTS ----
contour_fill <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    stat_density_2d(geom = "polygon", contour = TRUE,
                    aes(fill = after_stat(level)), colour = "black", bins = 6) +
    scale_fill_distiller(palette = "Reds", direction = 1)
}

cor_func <- function(data, mapping, method, symbol, ...){
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  corr <- cor(x, y, method=method, use='complete.obs')
  colFn <- colorRampPalette(c("dodgerblue", "white", "brown1"), interpolate ='spline')
  fill <- colFn(100)[findInterval(corr, seq(-1, 1, length = 100))]
  
  ggally_text(
    label = paste(symbol, as.character(round(corr, 2))), 
    mapping = aes(),
    xP = 0.5, yP = 0.5,
    color = 'black',
    ...
  ) + 
    theme(panel.background = element_rect(fill = fill), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}

vars_to_log <- c(
  "beauty_shops_800", "bike_parkings_800", "fast_foods_800", "industrials_800", "offices_800", "public_institutions_800", "renthouses_800",
  "restaurants_800", "school_kindergardens_800", "shops_800", "water_objects_800", "services_800", "attractions_800", "pharmacys_800", "price_per_m", "area",
  "dist_attraction", "dist_bank", "dist_bar", "dist_beauty_shop", "dist_bike_parking", "dist_bike_rent", "dist_college_university", "dist_cultural",
  "dist_dormitory", "dist_fast_food", "dist_food_shop", "dist_healthcare_institution", "dist_nightclub", "dist_park", "dist_pharmacy", "dist_playground",
  "dist_bus_stop", "dist_car_service", "dist_bus_station", "dist_construction", "dist_gas_station", "dist_industrial",
  "dist_parking", "dist_jeweller", "dist_public_institution", "dist_public_service", "dist_renthouse", "dist_restaurant", "dist_school_kindergarden",
  "dist_service", "dist_shop", "dist_shopping_mall", "dist_sport_object", "dist_subway_entrance", "dist_office", "dist_swamps",
  "dist_temple_catholic", "dist_temple_other", "dist_tram_stop", "dist_water_object", "dist_road_cycleway", "dist_road_footway",
  "dist_road_primary", "dist_road_path", "dist_road_residential", "dist_road_secondary", "dist_road_service", "dist_road_steps", "dist_road_tertiary", "dist_train_stop"
  
)

flats_cl2 <- flats_cl %>% 
  select(-dist_garage, -dist_railway_stop) %>% 
  mutate(across(all_of(vars_to_log), function(x){log(x+1)})) %>% 
  mutate(free_from_year = ifelse(is.na(free_from_year), 2023, free_from_year))

# N OBJECTS WITHIN WALK DISTANCE
n_plt <- flats_cl2 %>% 
  select(bike_parkings_800, restaurants_800, food_shops_800, price_per_m) %>% 
  ggpairs(
    diag = list(continuous = wrap("barDiag", bins = 30, color = "black")),
    lower = list(continuous = wrap(contour_fill, color = "black")),
    upper = list(continuous = wrap(cor_func, method = 'spearman', symbol = expression('\u03C1 =')))
  )
#n_plt


n_plt <- flats_cl2 %>% 
  select(contains("_800"), price_per_m) %>% 
  select(-c(bike_parkings_800, food_shops_800, pharmacys_800, shops_800, attractions_800, services_800)) %>% 
  #select(c(restaurants_800)) %>% 
  ggpairs(
    diag = list(continuous = wrap("barDiag", bins = 30, color = "black")),
    lower = list(continuous = wrap(contour_fill, color = "black")),
    upper = list(continuous = wrap(cor_func, method = 'spearman', symbol = expression('\u03C1 =')))
  )
#n_plt

# AREA
n_plt <- flats_cl2 %>% 
  select(area, price_per_m) %>% 
  ggpairs(
    diag = list(continuous = wrap("barDiag", bins = 40, color = "black")),
    lower = list(continuous = wrap(contour_fill, color = "black")),
    upper = list(continuous = wrap(cor_func, method = 'spearman', symbol = expression('\u03C1 =')))
  )
#n_plt

# DISTANCES
n_plt <- flats_cl2 %>% 
  select(contains("dist"), price_per_m) %>%
  select(c(dist_industrial, dist_graveyard, dist_gas_station, dist_swamps, dist_road_path)) %>% 
  #select(41:57, price_per_m) %>% # 1:20 21:40 41:57
  ggpairs(
    diag = list(continuous = wrap("barDiag", bins = 40, color = "black")),
    lower = list(continuous = wrap(contour_fill, color = "black")),
    upper = list(continuous = wrap(cor_func, method = 'spearman', symbol = expression('\u03C1 =')))
  )
#n_plt

# YEAR
n_plt <- flats_cl2 %>% 
  select(build_year, price_per_m) %>%
  ggpairs(
    diag = list(continuous = wrap("barDiag", bins = 40, color = "black")),
    lower = list(continuous = wrap(contour_fill, color = "black")),
    upper = list(continuous = wrap(cor_func, method = 'spearman', symbol = expression('\u03C1 =')))
  )
#n_plt


# RENT
n_plt <- flats_cl2 %>% 
  select(build_year, area, rent, price_per_m) %>%
  ggpairs(
    diag = list(continuous = wrap("barDiag", bins = 40, color = "black")),
    lower = list(continuous = wrap(contour_fill, color = "black")),
    upper = list(continuous = wrap(cor_func, method = 'spearman', symbol = expression('\u03C1 =')))
  )
#n_plt

# WHICH TO CHOOSE?
cols_both <- flats_cl2 %>% 
  select(contains("800")) %>% 
  select(-c(bike_parkings_800, food_shops_800, pharmacys_800, shops_800, attractions_800, services_800)) %>% 
  colnames() %>% 
  str_remove("s_800") %>% 
  .[.!= "industrial"] %>% 
  .[.!= "playground"] %>% 
  .[.!= "bus_stop"]
n_plt <- flats_cl2 %>% 
  select(contains(cols_both), price_per_m) %>% 
  ggpairs(
    diag = list(continuous = wrap("barDiag", bins = 40, color = "black")),
    lower = list(continuous = wrap(contour_fill, color = "black")),
    upper = list(continuous = wrap(cor_func, method = 'spearman', symbol = expression('\u03C1 =')))
  )
#n_plt


# DROP UNUSED
# industrial_800 - complex matter, keep it
# year_created_first: 2023, 2022, "earlier"
# free_from_year: now, earlier, later


# NUM MAP PLOTS ----
flats_cl3 <- flats_cl2 %>% 
  select(-c(rent, area, build_year)) %>% 
  select(-c(dist_industrial, dist_graveyard, dist_gas_station, dist_swamps, dist_road_path, dist_nightclub, dist_bus_stop, dist_playground, dist_road_steps)) %>% 
  select(-c(bike_parkings_800, food_shops_800, pharmacys_800, shops_800, attractions_800, services_800)) %>% 
  select(-c(dist_parking, water_objects_800, renthouses_800)) %>% 
  mutate(
    year_created_first = ifelse(year_created_first<2022, "earlier", as.character(year_created_first)),
    free_from = ifelse(free_from_year==2023, "now", ifelse(free_from_year<2023, "earlier", "later"))
  ) %>% 
  select(-free_from_year)

df_sel <- flats_cl3#%>% filter(area <= 17000) 

cols_num <- df_sel %>% select(contains("800"), contains("dist"), price_per_m) %>% colnames()
plt_lst <- list()
if (FALSE) {
  for (i in seq_along(cols_num)) {
    name_beautiful <- cols_num[i] %>% str_replace("s_800", " within 800 m")
    
    flats_vis <- df_sel %>% mutate(color = create_color(df_sel[[cols_num[i]]]))
    plt_lst[[cols_num[i]]] <- regions %>% 
      leaflet(options = leafletOptions(attributionControl=FALSE)) %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(minZoom = 11, maxZoom = 11)) %>% 
      addPolygons(
        stroke = 0.5,
        opacity = 1,
        fillColor = "black",
        weight = 0.8,
        color = "black",
        smoothFactor = 0.8,
        highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = TRUE),
      ) %>%
      addMarkers(
        lng = df_sel$x,
        lat = df_sel$y,
        icon = ~custom_icons[flats_vis$color]
      ) %>% 
      addLegend(
        position = "topright",
        colors = c("#bd0026", "#f03b20", "#fd8d3c", "#fecc5c", "#ffffb2"),
        labels = df_sel[[cols_num[i]]] %>% create_label(),
        title = glue::glue("{cols_num[i]}")
      ) %>%
      setView(lng = 21.05, lat = 52.23, zoom = 11)
    saveWidget(plt_lst[[cols_num[i]]], "temp.html", selfcontained = FALSE)
    webshot("temp.html", file = glue::glue("plots_single/{cols_num[i]}.png"), cliprect = "viewport", vwidth = 700, vheight = 680)
  }
}



# CAT 0 ----
flats_cl3 %>% select(a = heating) %>% group_by(a) %>% summarise(n = n())

cols_to_drop <- c("is_exclusive_offer", "is_promoted", "is_two_level", "is_security_blinds", "is_separate_kitchen", "is_stove", "is_tv")

flats_cl4 <- flats_cl3 %>% 
  select(-all_of(cols_to_drop)) %>% 
  #select(-c(dist_parking, water_objects_800, renthouses_800))
  filter(!floor_no %in% c("cellar", "garret")) %>% 
  mutate(
    rooms_num = ifelse(rooms_num %in% c("6", "7", "8", "10", "more", "5"), "4+", rooms_num),
    building_type = ifelse(is.na(building_type), "unknown", building_type),
    floor_no = ifelse(floor_no == "ground_floor", "0", ifelse(floor_no %in% c("higher_10", "10"), "9+", floor_no)),
    heating = ifelse(heating %in% c("electrical", "boiler_room", "gas"), "other", heating),
    building_ownership = ifelse(building_ownership %in% c("cooperative", "cooperative_register", "share"), "coop", building_ownership),
    building_floors_num = ifelse(as.numeric(building_floors_num)>9, "9+", building_floors_num)
  ) %>% 
  mutate(
    floor_no = ifelse(floor_no == "", "unknown", floor_no),
    heating = ifelse(heating == "", "unknown", heating),
    construction_status = ifelse(construction_status == "", "ready_to_use", construction_status),
    building_ownership = ifelse(is.na(building_ownership), "unknown", building_ownership),
    building_material = ifelse(is.na(building_material), "unknown", building_material),
    building_floors_num = ifelse(is.na(building_floors_num), "unknown", building_floors_num)
  ) %>% 
  mutate(
    is_kitchen_fursnished = ifelse(is_freezer==1L&is_oven==1L&is_dishwasher==1L, 1L, 0L),
    is_media = ifelse(is_kable_tv==1L&is_internet==1L, 1L, 0L)
  ) %>% 
  select(-c(is_freezer, is_oven, is_dishwasher, is_kable_tv, is_internet)) %>% 
  rename(is_named = has_name) %>% 
  mutate(is_market_primary = ifelse(market == "primary", 1L, 0L)) %>% 
  select(-market) %>% 
  mutate(
    building_type = ifelse(building_type == "unknown", "other", building_type),
    is_building_concrete = ifelse(building_material == "concrete", 1L, 0L),
    is_heating_urban = ifelse(heating=="other", 0L, 1L),
    is_to_renovation = ifelse(construction_status == "to_renovation", 1L, 0L),
    is_ownership_full = ifelse(building_ownership == "coop", 0L, 1L)
  ) %>% 
  select(-heating, -construction_status, -building_ownership, building_material)


# CAT DENSITY PLOTS ----
calc_p_val <- function(x, y) {
  if (n_distinct(x)==2) {
    p_val <- t.test(df_sel$is_private_owner, df_sel$price_per_m, alternative = "two.sided", var.equal = FALSE)$p.value
    p_val <- p_val %>% round(3)
  } else {
    p_val <- aov(y ~ x) %>% summary()
    p_val <- p_val[[1]]$`Pr(>F)`[1] %>% round(3)
  }
  return(p_val)
}

calc_tau_val <- function(x, y) {
  x <- x %>% as.factor() %>% as.numeric()
  y <- y %>% as.factor() %>% as.numeric()
  tau_val <- cor.test(x, y, method="kendall")
  tau_val <- tau_val$estimate
  names(tau_val) <- NULL
  tau_val <- round(tau_val, 3)
  return(tau_val)
}

cor_d_func <- function(data, mapping, method, symbol, ...){
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  if (is.factor(x)) {
    corr <- calc_tau_val(x, y)
    text_to_show <- paste0("\u03C4: ", corr)
    colFn <- colorRampPalette(c("white", "brown1"), interpolate ='spline')
    fill <- colFn(100)[findInterval(corr, seq(-1, 1, length = 100))]
  } else {
    corr <- calc_p_val(y, x)
    text_to_show <- paste0("p-value: ", corr)
    colFn <- colorRampPalette(c("brown1", "white"), interpolate ='spline')
    fill <- colFn(100)[findInterval(corr, seq(-1, 1, length = 100))]
  }

  ggally_text(
    label = text_to_show,
    mapping = aes(),
    xP = 0.5, yP = 0.5,
    color = 'black',
    ...
  ) + 
    theme(panel.background = element_rect(fill = fill), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}

box_custom <- function(data, mapping, ...){
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  print(y)[1]
  df <- tibble(x=x, y=y)
  df %>% 
    mutate(x = as.factor(x)) %>% 
    ggplot(aes(x=x, y=y, fill=x))+
    geom_boxplot(alpha=0.7, outlier.shape = NA) +#
    stat_summary(fun.y=mean, colour="black", fill = "black", geom="point", size = 2) +
    scale_y_continuous(limits = quantile(df$y, c(0.1, 0.9))) +
    theme(legend.position="none")
}

table_custom <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  df <- tibble(x=x, y=y)
  df <- df %>% 
    table() %>% 
    as.data.frame.matrix() %>% 
    as_tibble(rownames = "x|y") %>% 
    mutate(x = c(0.4, 1), y = c(0.4, 1))
  ggplot(df) +
    geom_point(aes(x = x, y = y), color = "#ebebeb") +
    annotation_custom(tableGrob(df %>% select(-x, -y), rows=NULL), xmin=0.4, xmax=0.8, ymin=0.4, ymax=0.8) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())
}

hist_custom <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  df <- tibble(xex = x)
  ggplot(data=df, aes(x=xex)) +
    geom_bar() +
    geom_text(stat='count', aes(label=..count..), vjust=+1.25, size = 1) +
    theme(
      axis.text.x = element_text(angle = 90)
    )
}

hist_custom_2d <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  df <- tibble(xex = x, yey = y)
  ggplot(data=df, aes(x=xex, fill = yey)) +
    geom_bar() +
    theme(
      axis.text.x = element_text(angle = 90)
    )
    #geom_text(stat='count', aes(label=..count..), size = 1)
}

df_sel <- flats_cl4 %>% 
  select(contains("is_"), contains("has_"), select_if(., is.character) %>% colnames(), building_floors_num, price_per_m)


n_plt <- df_sel %>% 
  mutate(across(colnames(.)[colnames(.)!="price_per_m"], as.character)) %>% 
  select(building_type, is_building_concrete, is_heating_urban, is_to_renovation, is_ownership_full, price_per_m) %>% 
  ggpairs(
    cardinality_threshold = 18,
    diag = list(
      continuous = wrap("barDiag", bins = 30, color = "black"),
      discrete = wrap(hist_custom)
    ),
    upper = list(
      discrete = wrap(cor_d_func),
      combo = wrap(cor_d_func)
    ),
    lower = list(
      discrete = wrap(hist_custom_2d),
      combo = wrap(box_custom)
    )
  ) +
  theme(axis.text.x = element_text(angle = 90))
#n_plt



n_plt <- df_sel %>% 
  mutate(across(colnames(.)[colnames(.)!="price_per_m"], as.character)) %>% 
  select(-building_material, -owner_type, -year_created_first, -price_per_m) %>% 
  ggpairs(
    cardinality_threshold = 18,
    diag = list(
      continuous = "blank", #wrap("barDiag", bins = 30, color = "black"),
      discrete = wrap(hist_custom)#wrap("barDiag", bins = 30, color = "black")
    ),
    upper = list(
      discrete = "blank",#wrap(cor_d_func)
      combo = "blank"#wrap(cor_d_func)
    ),
    lower = list(
      discrete = wrap(cor_d_func), #wrap(hist_custom_2d)
      combo = "blank"#wrap(box_custom)
    )
  ) +
  theme(axis.text.x = element_text(angle = 90))
#n_plt

# CAT MAP PLOTS ----
flats_cl5 <- flats_cl4 %>% 
  select(-building_material, -owner_type, -year_created_first) %>% 
  mutate(windows_type = ifelse(windows_type == "", "unknown", windows_type))


# YES-NO ---- 
custom_icons <- iconList(
  red = makeIcon("icons/red1.png", 4, 4),
  blue = makeIcon("icons/blue.png", 4, 4)
)

#
cols_2 <- flats_cl5 %>% 
  select(contains("is_"), contains("has_"), select_if(., is.character) %>% colnames(), building_floors_num) %>% 
  mutate_all(function(x) {n_distinct(x)}) %>% 
  head(1) %>% 
  pivot_longer(cols = colnames(.)) %>% 
  filter(value == 2) %>% 
  pull(name)
create_color <- function(x) {case_when(x == 0 ~ "red", x == 1 ~ "blue")}

df_sel <- flats_cl5

if (FALSE) {
  for (i in seq_along(cols_2)) {
    flats_vis <- df_sel %>% mutate(color = create_color(df_sel[[cols_2[i]]]))
    plt_lst[[cols_2[i]]] <- regions %>% 
      leaflet(options = leafletOptions(attributionControl=FALSE)) %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(minZoom = 11, maxZoom = 11)) %>% 
      addPolygons(
        stroke = 0.5,
        opacity = 1,
        fillColor = "black",
        weight = 0.8,
        color = "black",
        smoothFactor = 0.8,
        highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = TRUE),
      ) %>%
      addMarkers(
        lng = df_sel$x,
        lat = df_sel$y,
        icon = ~custom_icons[flats_vis$color]
      ) %>% 
      addLegend(
        position = "topright",
        colors = c('#00a8f3', '#ec1c24'),
        labels = c('yes', 'no'),
        title = glue::glue("{cols_2[i]}")
      ) %>%
      setView(lng = 21.05, lat = 52.23, zoom = 11)
    saveWidget(plt_lst[[cols_2[i]]], "temp.html", selfcontained = FALSE)
    webshot("temp.html", file = glue::glue("plots_single_cat/{cols_2[i]}.png"), cliprect = "viewport")
  }
}

# MORE THAN 2 ----
custom_icons <- iconList(
  red1 = makeIcon("icons/red.png", 4, 4),
  red2 = makeIcon("icons/blue.png", 4, 4),
  red3 = makeIcon("icons/yellow.png", 4, 4),
  red4 = makeIcon("icons/green.png", 4, 4),
  red5 = makeIcon("icons/orange.png", 4, 4)
)

cols_2 <- c("building_type", "windows_type", "free_from")

create_color <- function(x) {
  x <- as.factor(x)
  lvls <- levels(x)
  case_when(
    x == lvls[1] ~ "red1",
    x == lvls[2] ~ "red2",
    x == lvls[3] ~ "red3",
    x == lvls[4] ~ "red4",
    TRUE ~ "red5"
  )
}

switch_color <- function(x) {
  case_when(
    x == "red1" ~ "#ec1c24",   
    x == "red2" ~ "#00a8f3", 
    x == "red3" ~ "#ffca18",
    x == "red4" ~ "#0ed145",
    TRUE ~ "#ff7f27"
  )
}

df_sel <- flats_cl5
plt_lst <- list()

if (FALSE) {
  for (i in seq_along(cols_2)) {
    flats_vis <- df_sel %>% mutate(color = create_color(df_sel[[cols_2[i]]]))
    plt_lst[[cols_2[i]]] <- regions %>% 
      leaflet(options = leafletOptions(attributionControl=FALSE)) %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(minZoom = 11, maxZoom = 11)) %>% 
      addPolygons(
        stroke = 0.5,
        opacity = 1,
        fillColor = "black",
        weight = 0.8,
        color = "black",
        smoothFactor = 0.8,
        highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = TRUE),
      ) %>%
      addMarkers(
        lng = df_sel$x,
        lat = df_sel$y,
        icon = ~custom_icons[flats_vis$color]
      ) %>% 
      addLegend(
        position = "topright",
        colors = df_sel[[cols_2[i]]] %>% create_color() %>% unique() %>% sort() %>% switch_color(),
        labels = df_sel[[cols_2[i]]] %>% as.factor() %>% levels(),
        title = glue::glue("{cols_2[i]}")
      ) %>%
      setView(lng = 21.05, lat = 52.23, zoom = 11)
    saveWidget(plt_lst[[cols_2[i]]], "temp.html", selfcontained = FALSE)
    webshot("temp.html", file = glue::glue("plots_single_cat/{cols_2[i]}.png"), cliprect = "viewport")
  }
}

# MORE THAN 10 ----
cols_2 <- c("building_floors_num", "floor_no", "rooms_num")

custom_icons <- iconList(
  redd0 = makeIcon("icons/redd0.png", 4, 4),
  redd1 = makeIcon("icons/redd1.png", 4, 4),
  redd2 = makeIcon("icons/redd2.png", 4, 4),
  redd3 = makeIcon("icons/redd3.png", 4, 4),
  redd4 = makeIcon("icons/redd4.png", 4, 4),
  redd5 = makeIcon("icons/redd5.png", 4, 4),
  redd6 = makeIcon("icons/redd6.png", 4, 4),
  redd7 = makeIcon("icons/redd7.png", 4, 4),
  redd8 = makeIcon("icons/redd8.png", 4, 4),
  redd9 = makeIcon("icons/redd9.png", 4, 4),
  redd99 = makeIcon("icons/black.png", 4, 4),
  greyy = makeIcon("icons/grey.png", 4, 4)
)

create_color <- function(x) {
  case_when(
    x == "0" ~ "redd0",
    x == "1" ~ "redd1",
    x == "2" ~ "redd2",
    x == "3" ~ "redd3",
    x == "4" ~ "redd4",
    x == "4+" ~ "redd5",
    x == "5" ~ "redd5",
    x == "6" ~ "redd6",
    x == "7" ~ "redd7",
    x == "8" ~ "redd8",
    x == "9" ~ "redd9",
    x == "9+" ~ "redd99",
    x == "unknown" ~ "greyy",
  )
}

f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)


df_sel <- flats_cl5
plt_lst <- list()

if (FALSE) {
  for (i in seq_along(cols_2)) {
    flats_vis <- df_sel %>% mutate(color = create_color(df_sel[[cols_2[i]]]))
    
    colors_sel <- if (!"0" %in% df_sel[[cols_2[i]]] %>% unique() %>% sort()) {c(f("Reds"), "black", "#c3c3c3")} else {c("white", f("Reds"), "black", "#c3c3c3")}
    
    if (df_sel[[cols_2[i]]] %>% n_distinct() == 5) {
      colors_sel <- f("Reds")[1:5]
    }
    
    plt_lst[[cols_2[i]]] <- regions %>% 
      leaflet(options = leafletOptions(attributionControl=FALSE)) %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(minZoom = 11, maxZoom = 11)) %>% 
      addPolygons(
        stroke = 0.5,
        opacity = 1,
        fillColor = "black",
        weight = 0.8,
        color = "black",
        smoothFactor = 0.8,
        highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = TRUE),
      ) %>%
      addMarkers(
        lng = df_sel$x,
        lat = df_sel$y,
        icon = ~custom_icons[flats_vis$color]
      ) %>% 
      addLegend(
        position = "topright",
        colors = colors_sel,
        labels = df_sel[[cols_2[i]]] %>% unique() %>% sort(),
        title = glue::glue("{cols_2[i]}")
      ) %>%
      setView(lng = 21.05, lat = 52.23, zoom = 11)
    saveWidget(plt_lst[[cols_2[i]]], "temp.html", selfcontained = FALSE)
    webshot("temp.html", file = glue::glue("plots_single_cat/{cols_2[i]}.png"), cliprect = "viewport")
  }
}


# FINAL ----
flats_cl6 <- flats_cl5 %>% 
  select(-c(is_named, free_from, is_wash_machine, is_private_owner)) %>% 
  mutate(id =  paste0("id_", row_number()))

to_numeric_lst <- flats_cl6 %>% colnames() %>% .[str_detect(., "is_")]

n_plt <- flats_cl6 %>% 
  mutate(across(all_of(to_numeric_lst), as.character)) %>% 
  select(-c(id, x, y, region, building_floors_num, floor_no, rooms_num, building_type, windows_type)) %>%
  select(everything(), price_per_m) %>% 
  ggpairs(
    cardinality_threshold = 18,
    diag = list(
      continuous = "blank",
      discrete = "blank",
      combo = "blank"
    ),
    upper = list(
      continuous = "blank",
      discrete = "blank",
      combo = wrap(cor_d_func)
    ),
    lower = list(
      continuous = "blank",
      discrete = "blank",
      combo = "blank"#,
    )
  ) +
  theme(axis.text.x = element_text(angle = 90))
n_plt

q25 <- function(x) {quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)}
q75 <- function(x) {quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)}

list_of_stats <- list(
  min = min,
  q25 = q25,
  median = median,
  q75 = q75,
  max = max,
  IQR = IQR,
  mean = mean,
  sd = sd,
  kurtosis = moments::kurtosis,
  skewness = moments::skewness
)

stats_lst <- c("Zmienna", "Minimum", 'Q25', 'Mediana', 'Q75', 'Maksimum', 'Rozstęp ćwiartkowy',' Średnia', 'Odchylenie standardowe', 'Kurtoza', 'Skośność')

cols_to_mutate <- flats_cl6 %>% 
  select_if(is.numeric) %>% 
  select(-starts_with("is_"), -x, -y) %>% 
  colnames() %>% 
  .[!. %in% c("dist_airport", "dist_prison", "dist_river", "dist_road_track_grade", "dist_cbd", "dist_road_trunk")] %>% 
  .[!str_detect(., "_800")]


flats_cl6 %>% 
  select_if(is.numeric) %>% 
  select(-starts_with("is_"), -x, -y) %>% 
  #select(price_per_m, dist_airport) %>% 
  mutate(across(all_of(cols_to_mutate), exp)) %>% 
  pivot_longer(cols = colnames(.)) %>% 
  group_by(name) %>% 
  summarise_all(.funs = list_of_stats) %>% 
  `colnames<-`(stats_lst) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  DT::datatable(rownames = FALSE, options = list(pageLength = 100))



cols_to_char <- flats_cl6 %>% select(-id) %>% select_if(is.character) %>% colnames()


count_categories <- function(x) {
  x %>% 
    as_tibble() %>% 
    group_by(value) %>% 
    summarise(n = n()) %>% 
    mutate(val_new = paste0(value, " (", n, ")")) %>% 
    arrange(desc(n)) %>% 
    pull(val_new) %>% 
    paste0(., collapse = ", ")
}


flats_cl6 %>% 
  select(starts_with("is_"), all_of(cols_to_char)) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = colnames(.)) %>% 
  group_by(name) %>% 
  summarise_all(.funs = list(count_categories)) %>% 
  DT::datatable(rownames = FALSE, options = list(pageLength = 50))


write_rds(flats_cl6, "data/clean/dataset_final.rds")