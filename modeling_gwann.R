# PRESETS ----
options(java.parameters="-Xmx16g")
library(sp)
library(leaflet)
library(tidyverse)
library(raster)
library(rgeos)
library(dplyr)
library(tidyr)
library(sp)
library(gridExtra)
library(gwann)
library(tictoc)

setwd("C:/Projects/WarsawFlatPrices/")
df <- read_rds("data/clean/dataset_final.rds")

# DATA PREP ----
if (FALSE) {
  X <- df %>% 
    mutate(
      rooms_num_4 = ifelse(rooms_num == "4", 1L, 0L),
      rooms_num_more = ifelse(rooms_num == "4+", 1L, 0L),
      building_type_block = ifelse(building_type == "block", 1L, 0L),
      building_type_apartment = ifelse(building_type == "apartment", 1L, 0L),
      windows_type_plastic = ifelse(windows_type == "plastic", 1L, 0L),
      windows_type_wooden = ifelse(windows_type == "wooden", 1L, 0L),
      floor_no_45 = ifelse(floor_no %in% c("4", "5"), 1L, 0L),
      floor_no_67 = ifelse(floor_no %in% c("6", "7"), 1L, 0L),
      floor_no_89 = ifelse(floor_no %in% c("8", "9", "9+", "unknown"), 1L, 0L),
      building_floors_num_45 = ifelse(building_floors_num %in% c("4", "5"), 1L, 0L),
      building_floors_num_67 = ifelse(building_floors_num %in% c("6", "7"), 1L, 0L),
      building_floors_num_89 = ifelse(building_floors_num %in% c("8", "9", "9+", "unknown"), 1L, 0L)
    ) %>% 
    fastDummies::dummy_cols("region") %>% 
    dplyr::select(-c(id, price_per_m, rooms_num, building_type, windows_type, floor_no, building_floors_num, region, `region_Śródmieście`)) %>% 
    as.matrix()
  y <- as.numeric(df$price_per_m)
  dm <- as.matrix(dist(df %>% dplyr::select(x, y)))
  s_test <- sample(nrow(X), 0.3*nrow(X))
}

# MODELING ----
cv_gwann <- function(df, k=10) {
  df <- df[sample(nrow(df)), ]
  y <- as.numeric(df$price_per_m)
  df <- df %>% 
    mutate(
      rooms_num_4 = ifelse(rooms_num == "4", 1L, 0L),
      rooms_num_more = ifelse(rooms_num == "4+", 1L, 0L),
      building_type_block = ifelse(building_type == "block", 1L, 0L),
      building_type_apartment = ifelse(building_type == "apartment", 1L, 0L),
      windows_type_plastic = ifelse(windows_type == "plastic", 1L, 0L),
      windows_type_wooden = ifelse(windows_type == "wooden", 1L, 0L),
      floor_no_45 = ifelse(floor_no %in% c("4", "5"), 1L, 0L),
      floor_no_67 = ifelse(floor_no %in% c("6", "7"), 1L, 0L),
      floor_no_89 = ifelse(floor_no %in% c("8", "9", "9+", "unknown"), 1L, 0L),
      building_floors_num_45 = ifelse(building_floors_num %in% c("4", "5"), 1L, 0L),
      building_floors_num_67 = ifelse(building_floors_num %in% c("6", "7"), 1L, 0L),
      building_floors_num_89 = ifelse(building_floors_num %in% c("8", "9", "9+", "unknown"), 1L, 0L)
    ) %>% 
    fastDummies::dummy_cols("region") %>% 
    dplyr::select(-c(price_per_m, rooms_num, building_type, windows_type, floor_no, building_floors_num, region, `region_Śródmieście`))
  dm <- as.matrix(dist(df %>% dplyr::select(x, y)))
  X <- df %>% dplyr::select(-id) %>% as.matrix()
  
  folds <- cut(seq(1, nrow(df)), breaks=k, labels=FALSE)
  res_lst <- list()
  for(i in 1:k) {
    print(i)
    s_test = which(folds==i, arr.ind=TRUE)
    x_train = X[-s_test,]
    y_train = y[-s_test]
    w_train = dm[-s_test, -s_test]
    x_pred = X[s_test,]
    w_pred = dm[-s_test, s_test]
    test_id = df[s_test,]$id
    
    gwann_fit = gwann(
      x_train = x_train, y_train = y_train, w_train = w_train,
      x_pred = x_pred, w_pred = w_pred,
      nrHidden = 40, batchSize = 50, lr = 0.1, optimizer = "adam", adaptive = F,
      cv_patience = 9999, bwSearch = "goldenSection", bwMin = min(dm)/4, bwMax = max(dm)/4,
      threads = 10
    )
    res_lst[[i]] <- tibble(id = test_id, actual = y[s_test], pred = diag(gwann_fit$predictions))
    
    rm(s_test, x_train, y_train, w_train, x_pred, w_pred, test_id, gwann_fit)
    gc()
  }
  return(res_lst)
}

tic()
cv_gwann_fit <- cv_gwann(df, 10)
toc()

write_rds(cv_gwann_fit, "data/clean/gwann_pred.rds")
# tic()
# gwann_fit <- gwann(
#   x_train = X[-s_test,],
#   y_train = y[-s_test],
#   w_train = dm[-s_test, -s_test],
#   x_pred = X[s_test,],
#   w_pred = dm[-s_test, s_test],
#   nrHidden = 40, batchSize = 50, lr = 0.1, optimizer = "adam", adaptive = F,
#   cv_patience = 9999, bwSearch = "goldenSection", bwMin = min(dm)/4, bwMax = max(dm)/4,
#   threads = 10
# )
# toc()