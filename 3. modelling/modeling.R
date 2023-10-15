# Presets ----
setwd("C:/Projects/WarsawFlatPrices/")
options(scipen = 999, java.parameters="-Xmx8g")
library(tidyverse)
library(lubridate)
library(sf)
library(spdep)
library(geosphere)
library(tictoc)
library(doParallel)
library(spatialreg)
library(caret)
library(leaflet)
library(bsreg)
library(spgwr)
library(gwann)
library(plotly)
set.seed(42)

rect_grid <- read_rds("data/clean/rect_grid.rds")

rect_centers <- data.frame(rgeos::gCentroid(rect_grid, byid=TRUE)) %>% 
  as_tibble()

df <- rect_grid@data %>% 
  as_tibble() %>% 
  rename(id = PolIDS) %>% 
  bind_cols(rect_centers)

poland <- read_rds("data/poland.rds")
warsaw <- poland[poland$NAME_3 == "Warszawa",]


# Calculate weight matrix ----
create_w_lst <- function(spatial_data, id_col = "PolIDS", type="dist", k=5, d1=0.01, d2=2) {
  if (type =="dist") {
    distance <- distm(coordinates(spatial_data), fun = distGeo) / 1000
    rownames(distance) <- spatial_data@data[[id_col]]
    colnames(distance) <- spatial_data@data[[id_col]]
    N <- nrow(spatial_data@data)
    gamma <- 1
    W <- 1 / (distance ^ gamma)
    diag(W) <- 0
    W <- W / as.matrix(rowSums(W)) %*% matrix(1, nrow = 1, ncol = N)
    w_list <- mat2listw(W, style="W")
  } else if (type == "knn") {
    W <- knearneigh(coordinates(spatial_data), k = k, longlat = TRUE)
    W <- knn2nb(W)
    w_list <- nb2listw(W, style = "W")
  } else if (type == "mixed") {
    W <- dnearneigh(coordinates(spatial_data), d1=d1, d2=d2, row.names = spatial_data@data[[id_col]], longlat = TRUE)
    w_list <- nb2listw(W, style = "W")
  } else {
    stop("incorrect W matrix type")
  }
  return(w_list)
}
W_list <- create_w_lst(rect_grid, type="dist")  #mixed, knn, dist
W_matrix <- listw2mat(W_list)

# Visualization functions ----
create_color <- function(x) {
  case_when(
    x > 0 & x >= quantile(x[x>0], probs = 0.9, names = FALSE, na.rm = TRUE) ~ "#67001F",   
    x > 0 & x >= quantile(x[x>0], probs = 0.7, names = FALSE, na.rm = TRUE) ~ "#B2182B", 
    x > 0 & x >= quantile(x[x>0], probs = 0.5, names = FALSE, na.rm = TRUE) ~ "#D6604D",
    x > 0 & x >= quantile(x[x>0], probs = 0.3, names = FALSE, na.rm = TRUE) ~ "#F4A582",
    x > 0 & x >= quantile(x[x>0], probs = 0.1, names = FALSE, na.rm = TRUE) ~ "#FDDBC7",
    x < 0 & x >= quantile(x[x<0], probs = 0.9, names = FALSE, na.rm = TRUE) ~ "#D1E5F0",   
    x < 0 & x >= quantile(x[x<0], probs = 0.7, names = FALSE, na.rm = TRUE) ~ "#92C5DE", 
    x < 0 & x >= quantile(x[x<0], probs = 0.5, names = FALSE, na.rm = TRUE) ~ "#4393C3",
    x < 0 & x >= quantile(x[x<0], probs = 0.3, names = FALSE, na.rm = TRUE) ~ "#2166AC",
    x < 0 & x >= quantile(x[x<0], probs = 0.1, names = FALSE, na.rm = TRUE) ~ "#053061",
    TRUE ~ "#F7F7F7"
  )
}

create_label <- function(x) {
  replace_na_chr <- function(x) {ifelse(x == "NA"|is.na(x)|x=="NA> "|x=="> NA"|x=="< NA", " ", x)}
  replace_na_0 <- function(x) {ifelse(x == "NA"|is.na(x)|x=="NA> "|x=="> NA", "0", x)}
  lbls1 <- c(
    quantile(x[x>0], probs = 0.9, names = FALSE, na.rm = TRUE),
    quantile(x[x>0], probs = 0.7, names = FALSE, na.rm = TRUE),
    quantile(x[x>0], probs = 0.5, names = FALSE, na.rm = TRUE),
    quantile(x[x>0], probs = 0.3, names = FALSE, na.rm = TRUE),
    quantile(x[x>0], probs = 0.1, names = FALSE, na.rm = TRUE)
  ) %>% round(2) %>% paste0("> ", .) %>% replace_na_chr()
  lbls3 <- c(
    quantile(x[x<0], probs = 0.9, names = FALSE, na.rm = TRUE),
    quantile(x[x<0], probs = 0.7, names = FALSE, na.rm = TRUE),
    quantile(x[x<0], probs = 0.5, names = FALSE, na.rm = TRUE),
    quantile(x[x<0], probs = 0.3, names = FALSE, na.rm = TRUE),
    quantile(x[x<0], probs = 0.1, names = FALSE, na.rm = TRUE)
  ) %>% round(2) %>% paste0("< ", .) %>% replace_na_chr()
  
  lbls2 <- paste0(
    "[",
    quantile(x[x>0], probs = 0.1, names = FALSE, na.rm = TRUE) %>% round(2) %>% replace_na_0(),
    " ; ",
    quantile(x[x<0], probs = 0.9, names = FALSE, na.rm = TRUE) %>% round(2) %>% replace_na_0(),
    "]",
    collapse = " "
  )
  
  ress <- c(lbls1, lbls2, lbls3)
  return(ress)
}

show_residuals <- function(spatial_data, res_vec, model_name) {
  colors_used <- c(
    "#67001F",
    "#B2182B",
    "#D6604D",
    "#F4A582",
    "#FDDBC7",
    "#F7F7F7",
    "#D1E5F0",
    "#92C5DE",
    "#4393C3",
    "#2166AC",
    "#053061"
  )
  spatial_data$residuals <- res_vec
  spatial_data %>% 
    leaflet(options = leafletOptions(attributionControl=FALSE)) %>% 
    addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(minZoom = 11, maxZoom = 11)) %>% 
    addPolygons(
      stroke = 0.1,
      opacity = 0.1,
      fillColor = "grey",
      weight = 0.2,
      color = "grey",
      smoothFactor = 0.8,
      data = warsaw
    ) %>% 
    addPolygons(
      stroke = 0.9,
      opacity = 1,
      fillColor = create_color(spatial_data$residuals),
      fillOpacity = 1,
      weight = 0.6,
      color = "black",
      smoothFactor = 0.8,
      highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = TRUE),
      data = rect_grid
    ) %>% 
    addLegend(
      position = "topright",
      colors = colors_used,
      opacity = 1,
      labels = create_label(spatial_data$residuals),
      title = glue::glue("{model_name}") #residuals()
    ) %>%
    setView(lng = 21.05, lat = 52.23, zoom = 11)
}

show_residuals_point <- function(res, name) {
  df <- tibble(y = res) %>% mutate(x = row_number())
  ggplot(df, aes(x=x, y=y)) +
    geom_point()+
    labs(title = name, x="", y="")+
    theme(
      plot.background = element_rect(fill = "gray90"),
      panel.background = element_rect(fill = "gray90",colour = "gray90", size = 0.5, linetype = "solid")
    )
}

# LM ----
df_fit <- df %>% 
  select(-c(
    id, building_floors_num, windows_type, is_first_time, is_phone, is_market_primary, is_security_windows,
    is_garage, is_media, is_domophone, is_ownership_full, is_furniture,
    dist_jeweller, school_kindergardens_800, dist_tram_stop, dist_beauty_shop, dist_train_stop, dist_airport,
    public_institutions_800, dist_bike_parking, dist_water_object, dist_school_kindergarden, fast_foods_800,
    dist_bar, offices_800, dist_road_primary, dist_road_footway, industrials_800, dist_bike_rent, dist_fast_food, x,
    dist_road_tertiary, dist_renthouse, dist_shopping_mall, dist_office, dist_road_secondary, dist_bank, dist_attraction, dist_dormitory,
    dist_road_residential, dist_pharmacy, dist_temple_catholic, dist_road_track_grade, dist_river, dist_park, dist_sport_object,
    dist_public_institution, dist_shop, dist_restaurant, is_utility_room, dist_college_university, bus_stops_800, beauty_shops_800,
    playgrounds_800, is_closed_territory
  )) %>% 
  mutate(floor_no = ifelse(floor_no %in% c("1", "2"), "1-2", floor_no)) %>% 
  mutate(floor_no = ifelse(floor_no %in% c("4", "5"), "4-5", floor_no)) %>% 
  mutate(rooms_num = ifelse(rooms_num %in% c("1", "2"), "1-2", rooms_num)) %>% 
  select(-floor_no)
LM <- lm(price_per_m ~ ., data = df_fit)

# Diagnostics:
summary(LM)
coeffs <- summary(LM)$coefficients[, 4]
which.max(coeffs)

lm.morantest(LM, W_list, alternative = "greater")
geary.test(LM$residuals, W_list)
joincount.test(as.factor(LM$residuals > 0), listw = W_list)
lm.LMtests(LM, W_list, test = "all")

local_moran_res <- localmoran(LM$residuals, W_list)
local_moran_res %>% as_tibble() %>% filter(`Pr(z != E(Ii))` < 0.05)

par(mar=c(4,4,1,1))
moran.plot(LM$residuals, W_list, ylab = "Spatial lag of residuals", xlab= "Residuals", pch = 1, main = "", col = "#053061", mar=c(5,5,5,5))
show_residuals(rect_grid, LM$residuals, "LM")
show_residuals_point(LM$residuals)

# SAR 0 ----
df_sar_0 <- df %>% select(-c(x, y, id))
SAR_0 <- spautolm(price_per_m ~ 1, data = df_sar_0, listw = W_list)

# Diagnostics:
summary(SAR_0)
res_sar_0 <- SAR_0$fit$residuals
moran.test(res_sar_0, listw = W_list)
LR1.Sarlm(SAR_0)
show_residuals(rect_grid, res_sar_0, "pure SAR")
show_residuals_point(res_sar_0)


calc_metrics2 <- function(estimate, mtype){
  truth <- df$price_per_m
  estimate <- df$price_per_m+estimate
  tibble(
    model = mtype,
    MSE = mse_vec(truth, estimate),
    RMSE = yardstick::rmse_vec(truth, estimate),
    MPE = yardstick::mpe_vec(truth, estimate),
    MAPE = yardstick::mape_vec(truth, estimate),
    sMAPE = yardstick::smape_vec(truth, estimate)
  )
}
calc_metrics2(res_sar_0, "sar0")


# SAR ----
df_sar <- df %>% 
  select(-c(x, y, id)) %>% 
  select(-c(
    building_floors_num, floor_no, windows_type, is_first_time, is_phone, is_market_primary, is_security_windows, is_garage,
    is_domophone, is_media, is_ownership_full, is_utility_room, is_furniture, is_balcony,
    school_kindergardens_800, dist_bar, industrials_800, fast_foods_800, dist_water_object, dist_road_footway, dist_bike_rent,
    dist_jeweller, dist_shopping_mall, dist_beauty_shop, dist_road_primary, dist_bike_parking, dist_school_kindergarden,
    dist_bank, dist_road_tertiary, dist_attraction, dist_fast_food, dist_airport, dist_road_secondary, public_institutions_800,
    dist_renthouse, offices_800, dist_train_stop, dist_road_residential, dist_road_track_grade, dist_pharmacy, dist_public_institution,
    dist_temple_catholic, dist_tram_stop, dist_college_university, dist_river, dist_park, dist_sport_object, dist_office, dist_shop,
    bus_stops_800, beauty_shops_800, dist_restaurant, dist_temple_other, dist_cbd
  )) %>% 
  mutate(rooms_num = ifelse(rooms_num %in% c("1", "2", "3"), "1-3", rooms_num))

# Diagnostics:
SAR <- lagsarlm(price_per_m ~ ., data = df_sar, listw = W_list)
summary(SAR)
coeffs <- summary(SAR)$Coef[, 4] %>% .[names(.)!="(Intercept)"]
which.max(coeffs)
res_sar <- SAR$residuals
moran.test(res_sar, listw = W_list)
LR1.Sarlm(SAR)
Wald1.Sarlm(SAR)
show_residuals(rect_grid, res_sar, "SAR")
show_residuals_point(res_sar)


# SEM ----
df_sem <- df %>% 
  select(-c(x, y, id)) %>% 
  select(-c(
    building_floors_num, floor_no, windows_type, is_first_time, is_phone, is_security_windows, is_market_primary, is_garage,
    is_domophone, is_media, is_ownership_full, is_utility_room, is_furniture, is_balcony,
    dist_water_object, school_kindergardens_800, industrials_800, dist_road_footway, dist_jeweller, dist_fast_food,
    dist_train_stop, dist_beauty_shop, dist_bar, dist_bike_rent, dist_school_kindergarden, fast_foods_800,
    dist_bank, dist_attraction, dist_bike_parking, dist_road_primary, dist_road_tertiary, dist_renthouse, dist_office,
    dist_shopping_mall, dist_airport, public_institutions_800, dist_river, dist_tram_stop, dist_road_residential, dist_road_secondary,
    dist_road_track_grade, dist_temple_catholic, dist_pharmacy, dist_college_university, dist_public_institution, playgrounds_800,
    dist_park, dist_shop, dist_sport_object, dist_restaurant, beauty_shops_800, dist_temple_other, offices_800, bus_stops_800
  )) %>% 
  mutate(rooms_num = ifelse(rooms_num %in% c("1", "2", "3"), "1-3", rooms_num))

# Diagnostics:
SEM <- errorsarlm(price_per_m ~ ., data = df_sem, listw = W_list)
summary(SEM)
coeffs <- summary(SEM)$Coef[, 4] %>% .[names(.)!="(Intercept)"]
which.max(coeffs)

res_sem <- SEM$residuals
moran.test(res_sem, listw = W_list)
LR1.Sarlm(SEM)
Wald1.Sarlm(SEM)
show_residuals(rect_grid, res_sem, "SEM")
show_residuals_point(res_sem)


# SLX ----
df_slx <- df %>% 
  select(-c(x, y, id)) %>% 
  select(-c(
    building_floors_num, floor_no, windows_type, is_first_time, is_phone, is_security_windows, is_market_primary, is_garage,
    is_domophone, is_media, is_ownership_full, is_utility_room, is_furniture, is_balcony, is_kitchen_fursnished,
    dist_dormitory, dist_attraction, fast_foods_800, bus_stops_800, dist_park, dist_bike_rent, dist_bar, industrials_800,
    school_kindergardens_800, dist_restaurant, dist_bike_parking, dist_temple_other, dist_prison, dist_water_object,
    dist_train_stop, dist_bus_station, playgrounds_800, dist_school_kindergarden, dist_tram_stop, dist_bank, dist_jeweller,
    offices_800, dist_road_track_grade, dist_shopping_mall, dist_renthouse, dist_college_university, dist_car_service,
    dist_road_trunk, dist_road_trunk, dist_road_residential, dist_beauty_shop, dist_shop, beauty_shops_800, dist_subway_entrance,
    dist_fast_food, dist_pharmacy, dist_road_footway, dist_construction, dist_temple_catholic, dist_road_primary, dist_road_tertiary,
    dist_public_service, dist_office, dist_public_institution, public_institutions_800, dist_service, dist_healthcare_institution,
    dist_river, dist_cultural
  )) %>% 
  mutate(rooms_num = ifelse(rooms_num %in% c("1", "2", "3"), "1-3", rooms_num)) %>% 
  mutate(rooms_num = ifelse(rooms_num %in% c("4", "4+"), "more", rooms_num))

# Diagnostics:
SLX <- lmSLX(price_per_m ~ ., data = df_slx, listw = W_list)
summary(SLX)
coeffs <- summary(SLX)$coefficients[, 4] %>% .[names(.)!="(Intercept)"] %>% .[!str_detect(names(.), "lag\\.")]
which.max(coeffs)

res_slx <- SLX$residuals
lm.morantest(SLX, listw = W_list)
lm.LMtests(SLX, listw = W_list, test = "all")
show_residuals(rect_grid, res_slx, "SLX")
show_residuals_point(res_slx)


# SARAR ----
df_sarar <- df %>% 
  select(-c(x, y, id)) %>% 
  select(-c(
    building_floors_num, floor_no, windows_type, is_first_time, is_phone, is_security_windows, is_market_primary, is_garage,
    is_domophone, is_media, is_ownership_full, is_utility_room, is_furniture, is_balcony,
    school_kindergardens_800, dist_bar, industrials_800, dist_road_footway, dist_water_object, fast_foods_800, dist_bike_rent,
    dist_jeweller, dist_shopping_mall, dist_beauty_shop, dist_road_primary, dist_bike_parking, dist_school_kindergarden,
    dist_bank, dist_road_tertiary, dist_attraction, dist_fast_food, dist_airport, dist_road_secondary, public_institutions_800,
    dist_renthouse, offices_800, dist_train_stop, dist_road_residential, dist_road_track_grade, dist_pharmacy, dist_public_institution,
    dist_temple_catholic, dist_tram_stop, dist_college_university, dist_river, dist_park, dist_sport_object, dist_office, dist_shop,
    bus_stops_800, beauty_shops_800, dist_restaurant, dist_temple_other, dist_cbd, dist_construction, playgrounds_800
  )) %>% 
  mutate(rooms_num = ifelse(rooms_num %in% c("1", "2", "3"), "1-3", rooms_num))

SARAR <- sacsarlm(price_per_m ~ ., data = df_sarar, listw = W_list)
summary(SARAR)
coeffs <- summary(SARAR)$Coef[, 4] %>% .[names(.)!="(Intercept)"]
which.max(coeffs)

res_sarar <- SARAR$residuals
moran.test(res_sarar, listw = W_list)
LR1.Sarlm(SARAR)
Wald1.Sarlm(SARAR)
show_residuals(rect_grid, res_sarar, "SARAR")
show_residuals_point(res_sarar)


# SDM ----
df_sdm <- df %>% 
  select(-c(x, y, id)) %>% 
  select(-c(
    building_floors_num, floor_no, windows_type, is_first_time, is_phone, is_security_windows, is_market_primary, is_garage,
    is_domophone, is_media, is_ownership_full, is_utility_room, is_furniture, is_balcony, is_kitchen_fursnished,
    dist_dormitory, dist_attraction, fast_foods_800, bus_stops_800, dist_park, dist_bike_rent, dist_bar, industrials_800,
    school_kindergardens_800, dist_restaurant, dist_bike_parking, dist_temple_other, dist_prison, dist_water_object,
    dist_train_stop, dist_bus_station, playgrounds_800, dist_school_kindergarden, dist_tram_stop, dist_bank, dist_jeweller,
    offices_800, dist_road_track_grade, dist_shopping_mall, dist_renthouse, dist_college_university, dist_car_service,
    dist_road_trunk, dist_road_trunk, dist_road_residential, dist_beauty_shop, dist_shop, beauty_shops_800, dist_subway_entrance,
    dist_fast_food, dist_pharmacy, dist_road_footway, dist_construction, dist_temple_catholic, dist_road_primary, dist_road_tertiary,
    dist_public_service, dist_office, dist_public_institution, public_institutions_800, dist_river, dist_service, dist_cultural,
    dist_healthcare_institution
  )) %>% 
  mutate(rooms_num = ifelse(rooms_num %in% c("1", "2", "3"), "1-3", rooms_num)) %>% 
  mutate(rooms_num = ifelse(rooms_num %in% c("4", "4+"), "more", rooms_num))

SDM <- lagsarlm(price_per_m ~ ., data = df_sdm, listw = W_list, type = "Durbin")
summary(SDM)
coeffs <- summary(SDM)$Coef[, 4] %>% .[names(.)!="(Intercept)"] %>% .[!str_detect(names(.), "lag\\.")]
which.max(coeffs)

res_sdm <- SDM$residuals
moran.test(res_sdm, listw = W_list)
LR1.Sarlm(SDM)
Wald1.Sarlm(SDM)
show_residuals(rect_grid, res_sdm, "SDM")
show_residuals_point(res_sdm)



# SDEM ----
df_sdem <- df %>% 
  select(-c(x, y, id)) %>% 
  select(-c(
    building_floors_num, floor_no, windows_type, is_first_time, is_phone, is_security_windows, is_market_primary, is_garage,
    is_domophone, is_media, is_ownership_full, is_utility_room, is_furniture, is_balcony,
    dist_water_object, school_kindergardens_800, industrials_800, dist_road_footway, dist_jeweller, dist_fast_food,
    dist_train_stop, dist_beauty_shop, dist_bar, dist_bike_rent, dist_school_kindergarden, fast_foods_800,
    dist_bank, dist_attraction, dist_bike_parking, dist_road_primary, dist_road_tertiary, dist_renthouse, dist_office,
    dist_shopping_mall, dist_airport, public_institutions_800, dist_river, dist_tram_stop, dist_road_residential, dist_road_secondary,
    dist_road_track_grade, dist_temple_catholic, dist_pharmacy, dist_college_university, dist_public_institution, playgrounds_800,
    dist_park, dist_shop, dist_sport_object, dist_restaurant, beauty_shops_800, dist_temple_other, offices_800, bus_stops_800,
    dist_subway_entrance, dist_car_service, dist_road_trunk, dist_prison, dist_dormitory, dist_cbd, dist_bus_station, dist_construction,
    dist_public_service, is_closed_territory, is_heating_urban, dist_cultural, dist_healthcare_institution, dist_service
  )) %>% 
  mutate(rooms_num = ifelse(rooms_num %in% c("1", "2", "3"), "1-3", rooms_num))

SDEM <- errorsarlm(price_per_m ~ ., data = df_sdem, listw = W_list, etype = "emixed")
summary(SDEM)
coeffs <- summary(SDEM)$Coef[, 4] %>% .[names(.)!="(Intercept)"] %>% .[!str_detect(names(.), "lag\\.")]
which.max(coeffs)

res_sdem <- SDEM$residuals
moran.test(res_sdem, listw = W_list)
LR1.Sarlm(SDEM)
Wald1.Sarlm(SDEM)
show_residuals(rect_grid, res_sdem, "SDEM")
show_residuals_point(res_sdem)



# GNS ----
df_gns <- df %>% 
  select(-c(x, y, id)) %>% 
  select(-c(
    building_floors_num, floor_no, windows_type, is_first_time, is_phone, is_security_windows, is_market_primary, is_garage,
    is_domophone, is_media, is_ownership_full, is_utility_room, is_furniture, is_balcony,
    dist_water_object, school_kindergardens_800, industrials_800, dist_road_footway, dist_jeweller, dist_fast_food,
    dist_train_stop, dist_beauty_shop, dist_bar, dist_bike_rent, dist_school_kindergarden, fast_foods_800,
    dist_bank, dist_attraction, dist_bike_parking, dist_road_primary, dist_road_tertiary, dist_renthouse, dist_office,
    dist_shopping_mall, dist_airport, public_institutions_800, dist_river, dist_tram_stop, dist_road_residential, dist_road_secondary,
    dist_road_track_grade, dist_temple_catholic, dist_pharmacy, dist_college_university, dist_public_institution, playgrounds_800,
    dist_park, dist_shop, dist_sport_object, dist_restaurant, beauty_shops_800, dist_temple_other, offices_800, bus_stops_800,
    dist_subway_entrance, dist_car_service, dist_road_trunk, dist_prison, dist_dormitory, dist_cbd, dist_bus_station, dist_construction,
    dist_public_service, is_closed_territory, is_heating_urban, dist_cultural, dist_healthcare_institution, dist_service
  )) %>% 
  mutate(rooms_num = ifelse(rooms_num %in% c("1", "2", "3"), "1-3", rooms_num))

GNS <- sacsarlm(price_per_m ~ ., data = df_gns, listw = W_list, type = "sacmixed")
summary(GNS)
coeffs <- summary(GNS)$Coef[, 4] %>% .[names(.)!="(Intercept)"] %>% .[!str_detect(names(.), "lag\\.")]
which.max(coeffs)

res_gns <- GNS$residuals
moran.test(res_gns, listw = W_list)
LR1.Sarlm(GNS)
Wald1.Sarlm(GNS)
show_residuals(rect_grid, res_gns, "GNS")
show_residuals_point(res_gns)

# CV FUNCTIONS ----
isis_lst <- df %>% select(contains("is_")) %>% colnames()

create_data <- function(spatial_data, mtype) {
  if (mtype == "SAR0") {
    df <- spatial_data@data %>% select(-c(PolIDS))
  } else if(mtype == "SAR") {
    df <- spatial_data@data %>% 
      select(-c(PolIDS)) %>% 
      select(-c(
        building_floors_num, floor_no, windows_type, is_first_time, is_phone, is_market_primary, is_security_windows, is_garage,
        is_domophone, is_media, is_ownership_full, is_utility_room, is_furniture, is_balcony,
        school_kindergardens_800, dist_bar, industrials_800, fast_foods_800, dist_water_object, dist_road_footway, dist_bike_rent,
        dist_jeweller, dist_shopping_mall, dist_beauty_shop, dist_road_primary, dist_bike_parking, dist_school_kindergarden,
        dist_bank, dist_road_tertiary, dist_attraction, dist_fast_food, dist_airport, dist_road_secondary, public_institutions_800,
        dist_renthouse, offices_800, dist_train_stop, dist_road_residential, dist_road_track_grade, dist_pharmacy, dist_public_institution,
        dist_temple_catholic, dist_tram_stop, dist_college_university, dist_river, dist_park, dist_sport_object, dist_office, dist_shop,
        bus_stops_800, beauty_shops_800, dist_restaurant, dist_temple_other, dist_cbd
      )) %>% 
      mutate(rooms_num = ifelse(rooms_num %in% c("1", "2", "3"), "1-3", rooms_num))
  } else if (mtype == "SEM") {
    df <- spatial_data@data %>%
      select(-c(PolIDS)) %>% 
      select(-c(
        building_floors_num, floor_no, windows_type, is_first_time, is_phone, is_security_windows, is_market_primary, is_garage,
        is_domophone, is_media, is_ownership_full, is_utility_room, is_furniture, is_balcony,
        dist_water_object, school_kindergardens_800, industrials_800, dist_road_footway, dist_jeweller, dist_fast_food,
        dist_train_stop, dist_beauty_shop, dist_bar, dist_bike_rent, dist_school_kindergarden, fast_foods_800,
        dist_bank, dist_attraction, dist_bike_parking, dist_road_primary, dist_road_tertiary, dist_renthouse, dist_office,
        dist_shopping_mall, dist_airport, public_institutions_800, dist_river, dist_tram_stop, dist_road_residential, dist_road_secondary,
        dist_road_track_grade, dist_temple_catholic, dist_pharmacy, dist_college_university, dist_public_institution, playgrounds_800,
        dist_park, dist_shop, dist_sport_object, dist_restaurant, beauty_shops_800, dist_temple_other, offices_800, bus_stops_800
      )) %>% 
      mutate(rooms_num = ifelse(rooms_num %in% c("1", "2", "3"), "1-3", rooms_num))
  } else if (mtype == "SLX") {
    df <- spatial_data@data %>% 
      select(-c(PolIDS)) %>% 
      select(-c(
        building_floors_num, floor_no, windows_type, is_first_time, is_phone, is_security_windows, is_market_primary, is_garage,
        is_domophone, is_media, is_ownership_full, is_utility_room, is_furniture, is_balcony, is_kitchen_fursnished,
        dist_dormitory, dist_attraction, fast_foods_800, bus_stops_800, dist_park, dist_bike_rent, dist_bar, industrials_800,
        school_kindergardens_800, dist_restaurant, dist_bike_parking, dist_temple_other, dist_prison, dist_water_object,
        dist_train_stop, dist_bus_station, playgrounds_800, dist_school_kindergarden, dist_tram_stop, dist_bank, dist_jeweller,
        offices_800, dist_road_track_grade, dist_shopping_mall, dist_renthouse, dist_college_university, dist_car_service,
        dist_road_trunk, dist_road_trunk, dist_road_residential, dist_beauty_shop, dist_shop, beauty_shops_800, dist_subway_entrance,
        dist_fast_food, dist_pharmacy, dist_road_footway, dist_construction, dist_temple_catholic, dist_road_primary, dist_road_tertiary,
        dist_public_service, dist_office, dist_public_institution, public_institutions_800, dist_service, dist_healthcare_institution,
        dist_river, dist_cultural
      )) %>% 
      mutate(rooms_num = ifelse(rooms_num %in% c("1", "2", "3"), "1-3", rooms_num)) %>% 
      mutate(rooms_num = ifelse(rooms_num %in% c("4", "4+"), "more", rooms_num))
  } else if (mtype == "SARAR") {
    df <- spatial_data@data %>% 
      select(-c(PolIDS)) %>% 
      select(-c(
        building_floors_num, floor_no, windows_type, is_first_time, is_phone, is_security_windows, is_market_primary, is_garage,
        is_domophone, is_media, is_ownership_full, is_utility_room, is_furniture, is_balcony,
        school_kindergardens_800, dist_bar, industrials_800, dist_road_footway, dist_water_object, fast_foods_800, dist_bike_rent,
        dist_jeweller, dist_shopping_mall, dist_beauty_shop, dist_road_primary, dist_bike_parking, dist_school_kindergarden,
        dist_bank, dist_road_tertiary, dist_attraction, dist_fast_food, dist_airport, dist_road_secondary, public_institutions_800,
        dist_renthouse, offices_800, dist_train_stop, dist_road_residential, dist_road_track_grade, dist_pharmacy, dist_public_institution,
        dist_temple_catholic, dist_tram_stop, dist_college_university, dist_river, dist_park, dist_sport_object, dist_office, dist_shop,
        bus_stops_800, beauty_shops_800, dist_restaurant, dist_temple_other, dist_cbd, dist_construction, playgrounds_800
      )) %>% 
      mutate(rooms_num = ifelse(rooms_num %in% c("1", "2", "3"), "1-3", rooms_num))
  } else if (mtype == "SDM") {
    df <- spatial_data@data %>% 
      select(-c(PolIDS)) %>% 
      select(-c(
        building_floors_num, floor_no, windows_type, is_first_time, is_phone, is_security_windows, is_market_primary, is_garage,
        is_domophone, is_media, is_ownership_full, is_utility_room, is_furniture, is_balcony, is_kitchen_fursnished,
        dist_dormitory, dist_attraction, fast_foods_800, bus_stops_800, dist_park, dist_bike_rent, dist_bar, industrials_800,
        school_kindergardens_800, dist_restaurant, dist_bike_parking, dist_temple_other, dist_prison, dist_water_object,
        dist_train_stop, dist_bus_station, playgrounds_800, dist_school_kindergarden, dist_tram_stop, dist_bank, dist_jeweller,
        offices_800, dist_road_track_grade, dist_shopping_mall, dist_renthouse, dist_college_university, dist_car_service,
        dist_road_trunk, dist_road_trunk, dist_road_residential, dist_beauty_shop, dist_shop, beauty_shops_800, dist_subway_entrance,
        dist_fast_food, dist_pharmacy, dist_road_footway, dist_construction, dist_temple_catholic, dist_road_primary, dist_road_tertiary,
        dist_public_service, dist_office, dist_public_institution, public_institutions_800, dist_river, dist_service, dist_cultural,
        dist_healthcare_institution
      )) %>% 
      mutate(rooms_num = ifelse(rooms_num %in% c("1", "2", "3"), "1-3", rooms_num)) %>% 
      mutate(rooms_num = ifelse(rooms_num %in% c("4", "4+"), "more", rooms_num))
  } else if (mtype == "SDEM") {
    df <- spatial_data@data %>% 
      select(-c(PolIDS)) %>% 
      select(-c(
        building_floors_num, floor_no, windows_type, is_first_time, is_phone, is_security_windows, is_market_primary, is_garage,
        is_domophone, is_media, is_ownership_full, is_utility_room, is_furniture, is_balcony,
        dist_water_object, school_kindergardens_800, industrials_800, dist_road_footway, dist_jeweller, dist_fast_food,
        dist_train_stop, dist_beauty_shop, dist_bar, dist_bike_rent, dist_school_kindergarden, fast_foods_800,
        dist_bank, dist_attraction, dist_bike_parking, dist_road_primary, dist_road_tertiary, dist_renthouse, dist_office,
        dist_shopping_mall, dist_airport, public_institutions_800, dist_river, dist_tram_stop, dist_road_residential, dist_road_secondary,
        dist_road_track_grade, dist_temple_catholic, dist_pharmacy, dist_college_university, dist_public_institution, playgrounds_800,
        dist_park, dist_shop, dist_sport_object, dist_restaurant, beauty_shops_800, dist_temple_other, offices_800, bus_stops_800,
        dist_subway_entrance, dist_car_service, dist_road_trunk, dist_prison, dist_dormitory, dist_cbd, dist_bus_station, dist_construction,
        dist_public_service, is_closed_territory, is_heating_urban, dist_cultural, dist_healthcare_institution, dist_service
      )) %>% 
      mutate(rooms_num = ifelse(rooms_num %in% c("1", "2", "3"), "1-3", rooms_num))
  } else if (mtype == "GNS") {
    df <- spatial_data@data %>% 
      select(-c(PolIDS)) %>% 
      select(-c(
        building_floors_num, floor_no, windows_type, is_first_time, is_phone, is_security_windows, is_market_primary, is_garage,
        is_domophone, is_media, is_ownership_full, is_utility_room, is_furniture, is_balcony,
        dist_water_object, school_kindergardens_800, industrials_800, dist_road_footway, dist_jeweller, dist_fast_food,
        dist_train_stop, dist_beauty_shop, dist_bar, dist_bike_rent, dist_school_kindergarden, fast_foods_800,
        dist_bank, dist_attraction, dist_bike_parking, dist_road_primary, dist_road_tertiary, dist_renthouse, dist_office,
        dist_shopping_mall, dist_airport, public_institutions_800, dist_river, dist_tram_stop, dist_road_residential, dist_road_secondary,
        dist_road_track_grade, dist_temple_catholic, dist_pharmacy, dist_college_university, dist_public_institution, playgrounds_800,
        dist_park, dist_shop, dist_sport_object, dist_restaurant, beauty_shops_800, dist_temple_other, offices_800, bus_stops_800,
        dist_subway_entrance, dist_car_service, dist_road_trunk, dist_prison, dist_dormitory, dist_cbd, dist_bus_station, dist_construction,
        dist_public_service, is_closed_territory, is_heating_urban, dist_cultural, dist_healthcare_institution, dist_service
      )) %>% 
      mutate(rooms_num = ifelse(rooms_num %in% c("1", "2", "3"), "1-3", rooms_num))
  } else {
    stop("unknown model type")
  }
  df <- df %>% 
    mutate(across(any_of(isis_lst), function(x) {factor(x, levels = c("0", "1"))}))
  
  return(df)
}

create_model <- function(df, w_list, mtype) {
  if (mtype == "SAR0") {
    mdl <- spautolm(price_per_m ~ 1, data = df, listw = w_list)
  } else if(mtype == "SAR") {
    mdl <- lagsarlm(price_per_m ~ ., data = df, listw = w_list)
  } else if (mtype == "SEM") {
    mdl <- errorsarlm(price_per_m ~ ., data = df, listw = w_list)
  } else if (mtype == "SLX") {
    mdl <- lmSLX(price_per_m ~ ., data = df, listw = w_list)
  } else if (mtype == "SARAR") {
    mdl <- sacsarlm(price_per_m ~ ., data = df, listw = w_list)
  } else if (mtype == "SDM") {
    mdl <- lagsarlm(price_per_m ~ ., data = df, listw = w_list, type = "Durbin")
  } else if (mtype == "SDEM") {
    mdl <- errorsarlm(price_per_m ~ ., data = df, listw = w_list, etype = "emixed")
  } else if (mtype == "GNS") {
    mdl <- sacsarlm(price_per_m ~ ., data = df, listw = w_list, type = "sacmixed")
  } else {
    stop("unknown model type")
  }
  return(mdl)
}

cv <- function(spatial_data, k = 10, mtype = "SAR0", wtype = "dist", id_col = "PolIDS", yname = "price_per_m", knn = 5, d1 = 0.01, d2 = 2) {
  spatial_data <- spatial_data[sample(nrow(spatial_data)), ]
  folds <- cut(seq(1, nrow(spatial_data)), breaks=k, labels=FALSE)
  res_lst <- list()
  for(i in 1:k) {
    ind_sel <- which(folds==i, arr.ind=TRUE)
    spatial_test <- spatial_data[ind_sel, ]
    spatial_train <- spatial_data[-ind_sel, ]
    
    w_list_train <- create_w_lst(spatial_train, id_col = id_col, type = wtype, k = knn, d1=d1, d2=d2)
    w_list_test <- create_w_lst(spatial_test, id_col = id_col, type = wtype, k = knn, d1=d1, d2=d2)
    df_train <- create_data(spatial_train, mtype)
    df_test <- create_data(spatial_test, mtype) %>% as.data.frame()
    rownames(df_test) <- attr(w_list_test, "region.id")
    sp_model <- create_model(df_train, w_list_train, mtype)
    
    y_id <- spatial_test[[id_col]]
    y_actual <- df_test[[yname]]
    
    if (mtype == "SLX") {
      df_test <- df_test %>%
        rename_with(~str_c(., "1"), colnames(df_test)[str_detect(colnames(df_test), "is_")]) %>%
        mutate(rooms_nummore =ifelse(rooms_num == "1-3", "0", "1")) %>% 
        mutate(building_typeblock = ifelse(building_type == "block", "1", "0")) %>% 
        mutate(building_typeother = ifelse(building_type == "other", "1", "0")) %>% 
        select(-c(rooms_num, building_type))
      y_pred <- predict(sp_model, newdata = df_test, listw = w_list_test)
      names(y_pred) <- NULL
    } else if (mtype %in% c("SARAR")) {
      y_pred <- predict(sp_model, newdata = df_train, listw = w_list_train, pred.type="BP") %>% as.data.frame() %>% as_tibble() %>% pull(fit)
      y_id <- spatial_train[[id_col]]
      y_actual <- df_train[[yname]]
    } else if (mtype %in% c("SDM", "SDEM")) {
      y_pred <- predict(sp_model, newdata = df_train, listw = w_list_train) %>% as.data.frame() %>% as_tibble() %>% pull(fit)
      y_id <- spatial_train[[id_col]]
      y_actual <- df_train[[yname]]
    } else if(mtype %in% c("GNS")) {
      y_id <- spatial_train[[id_col]]
      y_actual <- df_train[[yname]]
      y_pred <- sp_model$residuals+y_actual
    } else if(mtype %in% c("SAR0")) {
      y_id <- spatial_train[[id_col]]
      y_actual <- df_train[[yname]]
      y_pred <- sp_model$fit$residuals+y_actual
    } else {
      y_pred <- predict(sp_model, newdata = df_test, listw = w_list_test) %>% as.data.frame() %>% as_tibble() %>% pull(fit)
    }
    res_lst[[i]] <- tibble(id = y_id, actual = y_actual, pred = y_pred)
  }
  return(res_lst)
}

cv_aggregate <- function(df) {
  df %>% 
    data.table::rbindlist(use.names = FALSE) %>% 
    as_tibble() %>% 
    group_by(id) %>%
    summarise(actual = median(actual), pred = median(pred)) %>% 
    arrange(id)
}

# CV CALCULATIONS ----
tic()
cv_sar0 <- cv(rect_grid, mtype = "SAR0") %>% cv_aggregate()
toc()
# 104.04

tic()
cv_sar <- cv(rect_grid, mtype = "SAR") %>% cv_aggregate()
toc()
# 104

tic()
cv_sem <- cv(rect_grid, mtype = "SEM") %>% cv_aggregate()
toc()
# 101.33

tic()
cv_slx <- cv(rect_grid, mtype = "SLX") %>% cv_aggregate()
toc()
# 77.48

tic()
cv_sarar <- cv(rect_grid, mtype = "SARAR") %>% cv_aggregate()
toc()
# 199

tic()
cv_sdm <- cv(rect_grid, mtype = "SDM") %>% cv_aggregate() #pred.type = "TS"
toc()
# 122

tic()
cv_sdem <- cv(rect_grid, mtype = "SDEM") %>% cv_aggregate()
toc()
# 111.7

tic()
cv_gns <- cv(rect_grid, mtype = "GNS") %>% cv_aggregate()
toc()
# 192


# CALCULATE CV RESIDUALS ----
calc_res_tb <- function(df, mtype){
  df[[paste0("res_", mtype)]] <- df$pred-df$actual
  df <- df %>% dplyr::select(-c(pred, actual))
  return(df)
}

res_cv_sar0 <- calc_res_tb(cv_sar0, "sar0")
res_cv_sar <- calc_res_tb(cv_sar, "sar")
res_cv_sem <- calc_res_tb(cv_sem, "sem")
res_cv_slx <- calc_res_tb(cv_slx, "slx")
res_cv_sarar <- calc_res_tb(cv_sarar, "sarar")
res_cv_sdm <- calc_res_tb(cv_sdm, "sdm")
res_cv_sdem <- calc_res_tb(cv_sdem, "sdem")
res_cv_gns <- calc_res_tb(cv_gns, "gns")

res_cv_all <- res_cv_sar0 %>% 
  left_join(res_cv_sar, by = "id") %>% 
  left_join(res_cv_sem, by = "id") %>% 
  left_join(res_cv_slx, by = "id") %>% 
  left_join(res_cv_sarar, by = "id") %>% 
  left_join(res_cv_sdm, by = "id") %>% 
  left_join(res_cv_sdem, by = "id") %>% 
  left_join(res_cv_gns, by = "id")

# CALCULATE METRICS ----
mse_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  mse_impl <- function(truth, estimate) {
    mean((truth - estimate) ^ 2)
  }
  yardstick::metric_vec_template(
    metric_impl = mse_impl,
    truth = truth, 
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

calc_metrics <- function(df, mtype){
  truth <- df$actual
  estimate <- df$pred
  tibble(
    model = mtype,
    MSE = mse_vec(truth, estimate),
    RMSE = yardstick::rmse_vec(truth, estimate),
    MPE = yardstick::mpe_vec(truth, estimate),
    MAPE = yardstick::mape_vec(truth, estimate),
    sMAPE = yardstick::smape_vec(truth, estimate)
  )
}

metrics_s <- bind_rows(
  calc_metrics(cv_sar0, "sar0"),
  calc_metrics(cv_sar, "sar"),
  calc_metrics(cv_sem, "sem"),
  calc_metrics(cv_slx, "slx"),
  calc_metrics(cv_sarar, "sarar"),
  calc_metrics(cv_sdm, "sdm"),
  calc_metrics(cv_sdem, "sdem"),
  calc_metrics(cv_gns, "gns")
)


# CALCULATE RESIDUALS & METRICS FOR ML MODELS FROM PYTHON ----
ml_pred <- data.table::fread("data/clean/ml_pred.csv") %>% as_tibble() %>% dplyr::select(-V1)
rect_id <- read_rds("data/clean/rect_id.rds")

ml_res <- ml_pred %>% 
  bind_cols(rect_id) %>% 
  mutate_if(is.double, function(x, y) {x-y}, y = rect_id$y) %>% 
  `colnames<-`(str_replace(colnames(.), "pred_", "res_")) %>% 
  dplyr::select(-id, -y) %>% 
  rename(id = PolIDS) %>% 
  group_by(id) %>% 
  summarise_all(median)


metrics_ml <- ml_pred %>% 
  bind_cols(rect_id) %>% 
  dplyr::select(-id) %>% 
  rename(id = PolIDS) %>% 
  `colnames<-`(str_replace(colnames(.), "pred_", "")) %>% 
  summarise_if(
    is.double,
    .funs = list(
      MSE = mse_vec, RMSE = yardstick::rmse_vec, MPE = yardstick::mpe_vec, 
      MAPE = yardstick::mape_vec, sMAPE = yardstick::smape_vec
    ),
    truth = .$y
  ) %>% 
  dplyr::select(-starts_with("y")) %>% 
  pivot_longer(cols = colnames(.)) %>% 
  mutate(metric = str_extract(name, "\\_.*")) %>% 
  mutate(metric = str_remove(metric, "\\_")) %>% 
  mutate(model = str_remove(name, "\\_.*")) %>% 
  dplyr::select(-name) %>% 
  pivot_wider(names_from = metric, values_from = value)
metrics_ml


# FITING BAYESIAN MODELS ----
# SAR 0
tic()
bsar0_fit <- bsar(
  price_per_m ~ .,
  data = df_sar_0 %>% select(price_per_m) %>% mutate(a = 1),
  W = W_matrix, 
  n_save = 5000L, 
  n_burn = 1000L,
  doptions = set_options(SAR = set_SAR())
)
toc() # 4.32

# SAR
tic()
bsar_fit <- bsar(
  price_per_m ~ .,
  data = df_sar,
  W = W_matrix, 
  n_save = 10000L, 
  n_burn = 1000L,
  doptions = set_options(SAR = set_SAR())
)
toc() # 6.25

res_bsar = bsar_fit$model$residuals %>% as.vector()
show_residuals(rect_grid, res_bsar, "bSAR")

# SEM
tic()
bsem_fit <- bsem(
  price_per_m ~ .,
  data = df_sem %>% select(price_per_m) %>% mutate(a = 1),
  W = W_matrix, 
  n_save = 5000L, 
  n_burn = 1000L,
  doptions = set_options(SEM = set_SEM())
)
toc() # 38.95

# SLX
tic()
bslx_fit <- bslx(
  price_per_m ~ .,
  data = df_sdem,
  W = W_matrix, 
  n_save = 5000L, 
  n_burn = 1000L,
  doptions = set_options(SLX = set_SLX())
)
toc()# 2.15

# SARAR
tic()
bsarar_fit <- bsem(
  price_per_m ~ .,
  data = df_sarar,
  W = W_matrix, 
  n_save = 5000L, 
  n_burn = 1000L,
  doptions = set_options(SEM = set_SEM())
)
toc() # 141.44

# SDM
tic()
bsdm_fit <- bsdm(
  price_per_m ~ .,
  data = df_sdem,
  W = W_matrix, 
  n_save = 3000L, 
  n_burn = 250L,
  doptions = set_options(SAR = set_SAR(), SLX = set_SLX())
)
toc()# 38.64

res_bsdm = bsdm_fit$model$residuals %>% as.vector()
show_residuals(rect_grid, res_bsdm, "bSDM")

# SDEM
tic()
bsdem_fit <- bsdem(
  price_per_m ~ .,
  data = df_sdem,
  W = W_matrix, 
  #n_save = 5000L, 
  n_burn = 250L,
  doptions = set_options(SEM = set_SEM(), SLX = set_SLX())
)
toc()

# FITTING BAYESIAN GNS MODEL ----
get_gns <- function(y, X, options = set_options(), X_SLX, Psi, Psi_SLX, ...) {
  
  class <- bsreg:::get_sem_class(
    parent = bsreg:::get_slx_class(
      parent = switch(
        options$type,
        Independent = bsreg:::NormalGamma,
        Conjugate = bsreg:::ConjugateNormalGamma,
        Shrinkage = bsreg:::ShrinkageNormalGamma,
        Horseshoe = bsreg:::Horseshoe
      )
    )
  )
  
  mdl <- class$new(priors = options$priors)
  mdl$setup(X = X, y = y, X_SLX = X_SLX, Psi_SEM = Psi, Psi_SAR = Psi, Psi_SLX = if(missing(Psi_SLX)) Psi else Psi_SLX, ...)
  
  return(mdl)
}

bm.formula <- function(x, data = NULL,
                       n_save = 1000L, n_burn = 500L,
                       options = set_options(), mh = set_mh(), verbose = TRUE,
                       W, X_SLX,
                       type = c("lm", "slx", "sar", "sem", "sdm", "sdem", "sv", "gns"),
                       ...) {
  
  # Check inputs ---
  call <- match.call()
  type <- match.arg(type)
  getter <- switch(type, lm = get_blm, slx = get_bslx, sar = get_bsar, sem = get_bsem, sdm = get_bsdm, sdem = get_bsdem, sv = get_bsv, gns = get_gns)
  
  # Prepare data ---
  mf <- model.frame(x, data = data)
  y <- model.response(mf, "numeric")
  X <- model.matrix(attr(mf, "terms"), mf, contrasts = NULL)
  
  if(all(X[, 1] == X[, 2])) {X <- X[, -1]} # Drop double intercept
  if(type %in% c("slx", "sdm", "sdem", "gns") && missing(X_SLX)) {X_SLX <- X[, -1]} # Use all regressors except the intercept
  
  # Get model and estimate ---
  mdl <- getter(y = y, X = X, options = options, Psi = W, X_SLX = X_SLX, ...)
  
  draws <- bsreg:::sample(mdl, n_save = n_save, n_burn = n_burn, mh = mh, verbose = verbose)
  
  # Done ---
  return(structure(list("draws" = draws, "model" = mdl, "call" = call), class = "bm"))
}

bgns <- function(...) {bm(..., type = "gns")}

tic()
bgns_fit <- bgns(
  price_per_m ~ .,
  data = df_sdem,
  W = W_matrix, 
  #n_save = 5000L, 
  n_burn = 250L,
  doptions = set_options(SEM = set_SEM(), SLX = set_SLX(), SAR = set_SAR())
)
toc()

# CALCULATE RESIDUALS AND METRICS FOR BAYESIAN MODELS ----
bs_res <- tibble(
  id = df$id,
  res_bsar0 = bsar0_fit$model$residuals %>% as.vector(),
  res_bsar = bsar_fit$model$residuals %>% as.vector(),
  res_bsem = bsem_fit$model$residuals %>% as.vector(),
  res_bslx = bslx_fit$model$residuals %>% as.vector(),
  res_bsarar = bsarar_fit$model$residuals %>% as.vector(),
  res_bsdm = bsdm_fit$model$residuals %>% as.vector(),
  res_bsdem = bsdem_fit$model$residuals %>% as.vector(),
  res_bgns = bgns_fit$model$residuals %>% as.vector()
)

metrics_bs <- bs_res %>% 
  mutate_if(is.double, function(x, y) {x+y}, y = df$price_per_m) %>% 
  mutate(y = df$price_per_m) %>% 
  `colnames<-`(str_replace(colnames(.), "res_", "")) %>% 
  dplyr::select(-id) %>% 
  summarise_if(
    is.double,
    .funs = list(
      MSE = mse_vec, RMSE = yardstick::rmse_vec, MPE = yardstick::mpe_vec, 
      MAPE = yardstick::mape_vec, sMAPE = yardstick::smape_vec
    ),
    truth = .$y
  ) %>% 
  dplyr::select(-starts_with("y")) %>% 
  pivot_longer(cols = colnames(.)) %>% 
  mutate(metric = str_extract(name, "\\_.*")) %>% 
  mutate(metric = str_remove(metric, "\\_")) %>% 
  mutate(model = str_remove(name, "\\_.*")) %>% 
  dplyr::select(-name) %>% 
  pivot_wider(names_from = metric, values_from = value)


# GWR ----
cv_gwr <- function(spatial_data, k=10) {
  spatial_data <- spatial_data[sample(nrow(spatial_data)), ]
  df <- spatial_data@data %>% 
    select(-c(PolIDS)) %>% 
    select(-c(
      building_floors_num, floor_no, windows_type, is_first_time, is_phone, is_market_primary, is_security_windows, is_garage,
      is_domophone, is_media, is_ownership_full, is_utility_room, is_furniture, is_balcony,
      school_kindergardens_800, dist_bar, industrials_800, fast_foods_800, dist_water_object, dist_road_footway, dist_bike_rent,
      dist_jeweller, dist_shopping_mall, dist_beauty_shop, dist_road_primary, dist_bike_parking, dist_school_kindergarden,
      dist_bank, dist_road_tertiary, dist_attraction, dist_fast_food, dist_airport, dist_road_secondary, public_institutions_800,
      dist_renthouse, offices_800, dist_train_stop, dist_road_residential, dist_road_track_grade, dist_pharmacy, dist_public_institution,
      dist_temple_catholic, dist_tram_stop, dist_college_university, dist_river, dist_park, dist_sport_object, dist_office, dist_shop,
      bus_stops_800, beauty_shops_800, dist_restaurant, dist_temple_other, dist_cbd
    )) %>% 
    mutate(rooms_num = ifelse(rooms_num %in% c("1", "2", "3"), "1-3", rooms_num))
  
  folds <- cut(seq(1, nrow(spatial_data)), breaks=k, labels=FALSE)
  res_lst <- list()
  for(i in 1:k) {
    ind_sel <- which(folds==i, arr.ind=TRUE)
    df_test <- df[ind_sel, ]
    df_train <- df[-ind_sel, ]
    spatial_test <- spatial_data[ind_sel, ]
    spatial_train <- spatial_data[-ind_sel, ]

    bandwidth_optimized <- gwr.sel(price_per_m ~ ., data = df_train, coords = coordinates(spatial_train), adapt = T, verbose = FALSE)
    gwr_fit <- gwr(price_per_m ~ ., data = df_test, coords = coordinates(spatial_test), adapt=bandwidth_optimized, hatmatrix=TRUE, se.fit=TRUE, predictions = TRUE)
    
    y_id <- spatial_test$PolIDS
    y_actual <- df_test$price_per_m
    y_pred <- gwr_fit$lm$fitted.values
    
    res_lst[[i]] <- tibble(id = y_id, actual = y_actual, pred = y_pred)
  }
  return(res_lst)
}

tic()
bandwidth_optimized <- gwr.sel(price_per_m ~ ., data = df_sar, coords = coordinates(rect_grid), adapt = T)
gwr_fit <- gwr(price_per_m ~ ., data = df_sar, coords = coordinates(rect_grid), adapt=bandwidth_optimized, hatmatrix=TRUE, se.fit=TRUE, predictions = TRUE)
toc()

tic()
cv_gwr_fit <- cv_gwr(rect_grid) %>% cv_aggregate()
toc()# 161.88
res_cv_gwr <- calc_res_tb(cv_gwr_fit, "gwr")
metrics_gwrs <- calc_metrics(cv_gwr_fit, "gwr")

# gwr_res <- tibble(
#   id = df$id,
#   res_gwr = gwr_fit$lm$residuals
# )
# metrics_gwr <- gwr_res %>% 
#   mutate_if(is.double, function(x, y) {x+y}, y = df$price_per_m) %>% 
#   mutate(y = df$price_per_m) %>% 
#   `colnames<-`(str_replace(colnames(.), "res_", "")) %>% 
#   dplyr::select(-id) %>% 
#   summarise_if(
#     is.double,
#     .funs = list(
#       MSE = mse_vec, RMSE = yardstick::rmse_vec, MPE = yardstick::mpe_vec, 
#       MAPE = yardstick::mape_vec, sMAPE = yardstick::smape_vec
#     ),
#     truth = .$y
#   ) %>% 
#   dplyr::select(-starts_with("y")) %>% 
#   pivot_longer(cols = colnames(.)) %>% 
#   mutate(metric = str_extract(name, "\\_.*")) %>% 
#   mutate(metric = str_remove(metric, "\\_")) %>% 
#   mutate(model = str_remove(name, "\\_.*")) %>% 
#   dplyr::select(-name) %>% 
#   pivot_wider(names_from = metric, values_from = value)

# GWANN ----
cv_gwann_fit <- read_rds("data/clean/gwann_pred.rds")
gwann_pred <- cv_gwann_fit %>% cv_aggregate()
rect_id <- read_rds("data/clean/rect_id.rds")

gwann_res <- gwann_pred %>% 
  dplyr::select(-id, -actual) %>% 
  rename(pred_gwann = pred) %>% 
  bind_cols(rect_id) %>% 
  mutate_if(is.double, function(x, y) {x-y}, y = rect_id$y) %>% 
  `colnames<-`(str_replace(colnames(.), "pred_", "res_")) %>% 
  dplyr::select(-id, -y) %>% 
  rename(id = PolIDS) %>% 
  group_by(id) %>% 
  summarise_all(median)


metrics_gwann <- gwann_pred %>% 
  dplyr::select(-id, -actual) %>% 
  rename(pred_gwann = pred) %>% 
  bind_cols(rect_id) %>% 
  dplyr::select(-id) %>% 
  rename(id = PolIDS) %>% 
  `colnames<-`(str_replace(colnames(.), "pred_", "")) %>% 
  summarise_if(
    is.double,
    .funs = list(
      MSE = mse_vec, RMSE = yardstick::rmse_vec, MPE = yardstick::mpe_vec, 
      MAPE = yardstick::mape_vec, sMAPE = yardstick::smape_vec
    ),
    truth = .$y
  ) %>% 
  dplyr::select(-starts_with("y")) %>% 
  pivot_longer(cols = colnames(.)) %>% 
  mutate(metric = str_extract(name, "\\_.*")) %>% 
  mutate(metric = str_remove(metric, "\\_")) %>% 
  mutate(model = str_remove(name, "\\_.*")) %>% 
  dplyr::select(-name) %>% 
  pivot_wider(names_from = metric, values_from = value)

# SIMPLE MODELS ----
cv_lm <- function(df, k=10) {
  df <- df[sample(nrow(df)), ]
  df <- df %>% 
    dplyr::select(-c(
      building_floors_num, windows_type, is_first_time, is_phone, is_market_primary, is_security_windows,
      is_garage, is_media, is_domophone, is_ownership_full, is_furniture,
      dist_jeweller, school_kindergardens_800, dist_tram_stop, dist_beauty_shop, dist_train_stop, dist_airport,
      public_institutions_800, dist_bike_parking, dist_water_object, dist_school_kindergarden, fast_foods_800,
      dist_bar, offices_800, dist_road_primary, dist_road_footway, industrials_800, dist_bike_rent, dist_fast_food, x,
      dist_road_tertiary, dist_renthouse, dist_shopping_mall, dist_office, dist_road_secondary, dist_bank, dist_attraction, dist_dormitory,
      dist_road_residential, dist_pharmacy, dist_temple_catholic, dist_road_track_grade, dist_river, dist_park, dist_sport_object,
      dist_public_institution, dist_shop, dist_restaurant, is_utility_room, dist_college_university, bus_stops_800, beauty_shops_800,
      playgrounds_800, is_closed_territory
    )) %>% 
    mutate(floor_no = ifelse(floor_no %in% c("1", "2"), "1-2", floor_no)) %>% 
    mutate(floor_no = ifelse(floor_no %in% c("4", "5"), "4-5", floor_no)) %>% 
    mutate(rooms_num = ifelse(rooms_num %in% c("1", "2"), "1-2", rooms_num)) %>% 
    select(-floor_no)
  
  
  folds <- cut(seq(1, nrow(df)), breaks=k, labels=FALSE)
  res_lst <- list()
  for(i in 1:k) {
    s_test = which(folds==i, arr.ind=TRUE)
    df_train = df[-s_test,] %>% dplyr::select(-id)
    df_test = df[s_test,] %>% dplyr::select(-id)
    test_id = df[s_test,]$id
    
    model_lm <- lm(price_per_m ~ ., data = df_train)
    y_pred <- predict(model_lm, df_test %>% select(-price_per_m))
    
    res_lst[[i]] <- tibble(id = test_id, actual = df_test$price_per_m, pred = y_pred)
  }
  return(res_lst)
}
cv_simple <- function(df, k=10, func_type = "mean") {
  df <- df[sample(nrow(df)), ] %>% select(price_per_m, id)
  folds <- cut(seq(1, nrow(df)), breaks=k, labels=FALSE)
  res_lst <- list()
  for(i in 1:k) {
    s_test = which(folds==i, arr.ind=TRUE)
    df_train = df[-s_test,]
    df_test = df[s_test,]
    test_id = df[s_test,]$id
    metric <- do.call(func_type, args = list(x = df_train$price_per_m, na.rm = TRUE))
    y_pred <- rep(metric, length(test_id))
    res_lst[[i]] <- tibble(id = test_id, actual = df_test$price_per_m, pred = y_pred)
  }
  return(res_lst)
}

tic()
cv_lm_fit <- cv_lm(df) %>% cv_aggregate()
toc()# 0.25sec
res_cv_lm <- calc_res_tb(cv_lm_fit, "lm")
metrics_lm <- calc_metrics(cv_lm_fit, "lm")

tic()
cv_mean_fit <- cv_simple(df, 10, "mean") %>% cv_aggregate()
toc() #0.06sec
res_cv_mean <- calc_res_tb(cv_mean_fit, "mean")
metrics_mean <- calc_metrics(cv_mean_fit, "mean")

tic()
cv_median_fit <- cv_simple(df, 10, "median") %>% cv_aggregate()
toc() #0.06sec
res_cv_median <- calc_res_tb(cv_median_fit, "median")
metrics_median <- calc_metrics(cv_median_fit, "median")

# Comparing all models ----
metrics_all <- bind_rows(
  metrics_lm,
  metrics_mean,
  metrics_median,
  metrics_s,
  metrics_bs,
  metrics_gwrs,
  metrics_ml,
  metrics_gwann,
) %>% arrange(RMSE)
metrics_all %>% mutate_if(is.numeric, round, 4) %>% DT::datatable(rownames = FALSE, options = list(pageLength = 40))

residuals_all <- res_cv_all %>% 
  left_join(res_cv_mean, by = "id") %>% 
  left_join(res_cv_median, by = "id") %>% 
  left_join(res_cv_lm, by = "id") %>% 
  left_join(bs_res, by = "id") %>% 
  left_join(res_cv_gwr, by = "id") %>% 
  left_join(ml_res, by = "id") %>% 
  left_join(gwann_res, by = "id")

show_residuals(rect_grid, residuals_all$res_sar0, "SAR (pure)")
show_residuals(rect_grid, residuals_all$res_sar, "SAR")
show_residuals(rect_grid, residuals_all$res_sem, "SEM")
show_residuals(rect_grid, residuals_all$res_slx, "SLX")
show_residuals(rect_grid, residuals_all$res_sarar, "SARAR") # 719x675
show_residuals(rect_grid, residuals_all$res_sdm, "SDM")
show_residuals(rect_grid, residuals_all$res_sdem, "SDEM")
show_residuals(rect_grid, residuals_all$res_gns, "GNS")
show_residuals(rect_grid, residuals_all$res_mean, "mean")
show_residuals(rect_grid, residuals_all$res_median, "median")
show_residuals(rect_grid, residuals_all$res_lm, "LM")
show_residuals(rect_grid, residuals_all$res_bsar0, "bSAR (pure)")
show_residuals(rect_grid, residuals_all$res_bsar, "bSAR")
show_residuals(rect_grid, residuals_all$res_bsem, "bSEM")
show_residuals(rect_grid, residuals_all$res_bslx, "bSLX")
show_residuals(rect_grid, residuals_all$res_bsarar, "bSARAR")
show_residuals(rect_grid, residuals_all$res_bsdm, "bSDM")
show_residuals(rect_grid, residuals_all$res_bsdem, "bSDEM")
show_residuals(rect_grid, residuals_all$res_bgns, "bGNS")
show_residuals(rect_grid, residuals_all$res_gwr, "GWR")
show_residuals(rect_grid, residuals_all$res_rf, "RF")
show_residuals(rect_grid, residuals_all$res_ef, "ExtraTrees")
show_residuals(rect_grid, residuals_all$res_ad, "adaboost")
show_residuals(rect_grid, residuals_all$res_gd, "gradient boosting")
show_residuals(rect_grid, residuals_all$res_xgb, "xgboost")
show_residuals(rect_grid, residuals_all$res_lgbm, "LGBM")
show_residuals(rect_grid, residuals_all$res_cat, "catboost")
show_residuals(rect_grid, residuals_all$res_mlp, "MLP")
show_residuals(rect_grid, residuals_all$res_gwann, "GWANN")


show_residuals_point(residuals_all$res_sar0, "SAR (pure)")
show_residuals_point(residuals_all$res_sar, "SAR")
show_residuals_point(residuals_all$res_sem, "SEM")
show_residuals_point(residuals_all$res_slx, "SLX")
show_residuals_point(residuals_all$res_sarar, "SARAR") # 719x675
show_residuals_point(residuals_all$res_sdm, "SDM")
show_residuals_point(residuals_all$res_sdem, "SDEM")
show_residuals_point(residuals_all$res_gns, "GNS")
show_residuals_point(residuals_all$res_mean, "mean")
show_residuals_point(residuals_all$res_median, "median")
show_residuals_point(residuals_all$res_lm, "LM")
show_residuals_point(residuals_all$res_bsar0, "bSAR (pure)")
show_residuals_point(residuals_all$res_bsar, "bSAR")
show_residuals_point(residuals_all$res_bsem, "bSEM")
show_residuals_point(residuals_all$res_bslx, "bSLX")
show_residuals_point(residuals_all$res_bsarar, "bSARAR")
show_residuals_point(residuals_all$res_bsdm, "bSDM")
show_residuals_point(residuals_all$res_bsdem, "bSDEM")
show_residuals_point(residuals_all$res_bgns, "bGNS")
show_residuals_point(residuals_all$res_gwr, "GWR")
show_residuals_point(residuals_all$res_rf, "RF")
show_residuals_point(residuals_all$res_ef, "ExtraTrees")
show_residuals_point(residuals_all$res_ad, "adaboost")
show_residuals_point(residuals_all$res_gd, "gradient boosting")
show_residuals_point(residuals_all$res_xgb, "xgboost")
show_residuals_point(residuals_all$res_lgbm, "LGBM")
show_residuals_point(residuals_all$res_cat, "catboost")
show_residuals_point(residuals_all$res_mlp, "MLP")
show_residuals_point(residuals_all$res_gwann, "GWANN")





# Feature importances ----

# ML:
importances_ml <- readxl::read_excel("data/clean/importances.xlsx")


imp_top_ml <- importances_ml %>% 
  group_by(variable) %>% 
  summarise(n = n()) %>% 
  mutate(variable = ifelse(variable == "y", "lattitude", variable)) %>% 
  mutate(variable = ifelse(variable == "x", "longitude", variable)) %>% 
  arrange(desc(n)) %>% 
  head(30)

imp_top_ml %>% 
  plot_ly(x =~ reorder(variable, -n), y =~ n, type = "bar") %>% 
  layout(
    yaxis = list(title = ""),
    xaxis = list(title = "Nazwa zmiennej")
  )

imp_m_ml <- importances_ml %>% 
  group_by(variable) %>% 
  summarise(n = mean(importance)) %>% 
  mutate(variable = ifelse(variable == "y", "lattitude", variable)) %>% 
  mutate(variable = ifelse(variable == "x", "longitude", variable)) %>% 
  mutate(n = round(n, 3)) %>% 
  arrange(desc(n)) %>% 
  head(30)

imp_m_ml %>% 
  plot_ly(x =~ reorder(variable, -n), y =~ n, type = "bar") %>% 
  layout(
    yaxis = list(title = ""),
    xaxis = list(title = "Nazwa zmiennej")
  )


# Spatial
extract_importances <- function(model, model_name) {
  summary(model)$Coef[, 4] %>% 
    as.data.frame() %>% 
    as_tibble(rownames = "variable") %>% 
    `colnames<-`(c("variable", "importance")) %>% 
    filter(variable != '(Intercept)') %>% 
    mutate(model = model_name)
}

importances_sp <- bind_rows(
  extract_importances(SAR, "SAR"),
  extract_importances(SEM, "SEM"),
  extract_importances(SLX, "SLX"),
  extract_importances(SARAR, "SARAR"),
  extract_importances(SDM, "SDM"),
  extract_importances(SDEM, "SDEM"),
  extract_importances(GNS, "GNS")
)

imp_m_sp <- importances_sp %>% 
  group_by(variable) %>% 
  summarise(n = mean(importance)) %>% 
  mutate(variable = ifelse(variable == "y", "lattitude", variable)) %>% 
  mutate(variable = ifelse(variable == "x", "longitude", variable)) %>% 
  filter(!str_detect(variable, "lag\\."), !variable %in% c("dist_cultural", "dist_public_service")) %>% 
  mutate(n = round(n, 3)) %>% 
  arrange(desc(n))

imp_m_sp %>% 
  plot_ly(x =~ reorder(variable, n), y =~ n, type = "bar") %>% 
  layout(
    yaxis = list(title = ""),
    xaxis = list(title = "Nazwa zmiennej")
  )

# BEST MODEL DESC ----
tmp_gwr <- gwr_fit$lm$coefficients %>% 
  as.data.frame() %>% 
  as_tibble(rownames = "variable") %>% 
  `colnames<-`(c("variable", "coefficient")) %>% 
  mutate(coefficient = round(coefficient, 4))
  
tmp_gwr %>% DT::datatable(rownames = FALSE, options = list(pageLength = 40))


library(GWmodel)

data_gwmdel <- rect_grid
data_gwmdel@data <- df_sar

gw_m_fit <- bw.gwr(formula = price_per_m ~ ., 
       approach = "AIC",
       dMat = W_matrix,
       adaptive = T, 
       data = data_gwmdel) 
summary(gw_m_fit)

gw_m_fit2 <- gwr.basic(formula = price_per_m ~ ., 
                                  adaptive = T,
                                  dMat = W_matrix,
                                  data = data_gwmdel, 
                                  bw = gw_m_fit) 


gw_m_fit2$Ftests
