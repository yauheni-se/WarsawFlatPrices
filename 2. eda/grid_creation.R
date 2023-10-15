# PRESETS ----
library(sp)
library(leaflet)
library(tidyverse)
library(raster)
library(rgeos)
library(dplyr)
library(tidyr)
library(sp)
library(gridExtra)

setwd("C:/Projects/WarsawFlatPrices/")
df <- read_rds("data/clean/dataset_final.rds") #SF.rest
coordinates(df) <- c("x", "y")

poland <- read_rds("data/poland.rds")
warsaw <- poland[poland$NAME_3 == "Warszawa",]

# CREATE RECTENGULAR GRID ----
rect_grid <- raster(df, ncol = 50, nrow = 50)
rect_grid <- as(rect_grid, "SpatialPolygonsDataFrame")
rect_grid <- SpatialPolygonsDataFrame(rect_grid, data.frame(row.names = 1:length(rect_grid), PolIDS = 1:length(rect_grid)), match.ID = FALSE)

rect_id_tbl <- over(df, rect_grid)

rect_id <- rect_id_tbl %>% as_tibble(rownames = "id") %>% mutate(y = df@data$price_per_m) %>% mutate(id = df@data$id)

write_rds(rect_id, "data/clean/rect_id.rds")

df@data <- data.frame(df@data, rect_id_tbl)


# AGGREGATE VARIABLES OVER GRID ----
get_mode <- function(x) {
  tt <- table(x)
  tt[which.max(tt)]%>% names()
}

by_medians <- df@data %>% 
  as_tibble() %>% 
  select_if(is.numeric) %>% 
  dplyr::select(-starts_with("is_")) %>% 
  group_by(PolIDS) %>% 
  summarise_all(mean, na.rm = TRUE)

vec_chr <- c(
  df@data %>% select_if(is.integer) %>% dplyr::select(!ends_with("s_800")) %>% colnames(),
  df@data %>% select_if(is.character) %>% colnames()
)

by_modes <- df@data %>% 
  as_tibble() %>% 
  dplyr::select(all_of(vec_chr)) %>% 
  dplyr::select(-id) %>% 
  group_by(PolIDS) %>% 
  summarise_all(get_mode)

by_stats <- by_medians %>% inner_join(by_modes, by = "PolIDS")

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

by_stats %>% 
  dplyr::select(any_of(vec_chr)) %>% 
  dplyr::select(-PolIDS) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = colnames(.)) %>% 
  group_by(name) %>% 
  summarise_all(.funs = list(count_categories)) %>% 
  DT::datatable(rownames = FALSE, options = list(pageLength = 50))
# => drop rare


by_stats <- by_stats %>% 
  dplyr::select(-c(is_air_cond, is_alarm, is_building_concrete, is_remote_service, is_to_renovation, region)) %>% 
  mutate(windows_type = ifelse(windows_type %in% c("unknown", "aluminium"), "other", windows_type))

# ASSIGN AGGREGATED VARS TO THE GRID ----
rect_grid@data <- rect_grid@data %>% left_join(by_stats, by = "PolIDS")
rect_grid <- rect_grid[!is.na(rect_grid$price_per_m), ]

rect_grid@data %>% 
  as_tibble()


# VISUALIZE NUM ----
create_color <- function(x) {
  case_when(
    x >= quantile(x, probs = 0.8, names = FALSE, na.rm = TRUE) ~ "#bd0026",   
    x >= quantile(x, probs = 0.6, names = FALSE, na.rm = TRUE) ~ "#f03b20", 
    x >= quantile(x, probs = 0.4, names = FALSE, na.rm = TRUE) ~ "#fd8d3c",
    x >= quantile(x, probs = 0.2, names = FALSE, na.rm = TRUE) ~ "#fecc5c",
    TRUE ~ "#ffffb2"
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

cols_num <- rect_grid@data %>% dplyr::select(contains("800"), contains("dist"), price_per_m) %>% colnames()
df_sel <- rect_grid@data
plt_lst <- list()
if (FALSE) {
  for (i in seq_along(cols_num)) {
    name_beautiful <- cols_num[i] %>% str_replace("s_800", " within 800 m")
    
    flats_vis <- df_sel# %>% mutate(color = )
    plt_lst[[cols_num[i]]] <- rect_grid %>% 
      leaflet(options = leafletOptions(attributionControl=FALSE)) %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(minZoom = 11, maxZoom = 11)) %>% 
      addPolygons(
        stroke = 0.5,
        opacity = 0.7,
        fillColor = "grey",
        weight = 0.2,
        color = "grey",
        smoothFactor = 0.8,
        data = warsaw
      ) %>% 
      addPolygons(
        stroke = 0.9,
        opacity = 1,
        fillColor = create_color(df_sel[[cols_num[i]]]),
        fillOpacity = 1,
        weight = 0.6,
        color = "black",
        smoothFactor = 0.8,
        highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = TRUE),
        data = rect_grid
      ) %>% 
      addLegend(
        position = "topright",
        colors = c("#bd0026", "#f03b20", "#fd8d3c", "#fecc5c", "#ffffb2"),
        opacity = 1,
        #labels = df_sel[[cols_num[i]]] %>% create_label(),
        labels = c("< P20", "P20-P40", "P40-60", "P60-P80", "> P80"),
        title = glue::glue("{cols_num[i]}")
      ) %>%
      setView(lng = 21.05, lat = 52.23, zoom = 11)
  }
}
plt_lst[[58]]


# VISUALIZE CAT (Y-N) ----
create_color <- function(x) {case_when(x == 0 ~ "#ec1c24", x == 1 ~ "#00a8f3")}
cols_cat <- rect_grid@data %>% dplyr::select(contains("is_")) %>% colnames()
df_sel <- rect_grid@data
plt_lst <- list()
if (FALSE) {
  for (i in seq_along(cols_cat)) {
    flats_vis <- df_sel
    plt_lst[[cols_cat[i]]] <- rect_grid %>% 
      leaflet(options = leafletOptions(attributionControl=FALSE)) %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(minZoom = 11, maxZoom = 11)) %>% 
      addPolygons(
        stroke = 0.5,
        opacity = 0.7,
        fillColor = "grey",
        weight = 0.2,
        color = "grey",
        smoothFactor = 0.8,
        data = warsaw
      ) %>% 
      addPolygons(
        stroke = 0.9,
        opacity = 1,
        fillColor = create_color(df_sel[[cols_cat[i]]]),
        weight = 0.6,
        color = "black",
        smoothFactor = 0.8,
        highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = TRUE),
        data = rect_grid
      ) %>% 
      addLegend(
        position = "topright",
        colors = c('#00a8f3', '#ec1c24'),
        labels = c('yes', 'no'),
        title = glue::glue("{cols_cat[i]}")
      ) %>%
      setView(lng = 21.05, lat = 52.23, zoom = 11)
  }
}
plt_lst[[20]]

# VISUALIZE CAT (2+) ----
create_color <- function(x) {
  x <- as.factor(x)
  lvls <- levels(x)
  case_when(
    x == lvls[1] ~ "#ec1c24",
    x == lvls[2] ~ "#00a8f3",
    x == lvls[3] ~ "#ffca18",
    x == lvls[4] ~ "#0ed145",
    TRUE ~ "#ff7f27"
  )
}
cols_cat <- c("building_type", "windows_type")
df_sel <- rect_grid@data
plt_lst <- list()
if (FALSE) {
  for (i in seq_along(cols_cat)) {
    flats_vis <- df_sel
    plt_lst[[cols_cat[i]]] <- rect_grid %>% 
      leaflet(options = leafletOptions(attributionControl=FALSE)) %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(minZoom = 11, maxZoom = 11)) %>% 
      addPolygons(
        stroke = 0.5,
        opacity = 0.7,
        fillColor = "grey",
        weight = 0.2,
        color = "grey",
        smoothFactor = 0.8,
        data = warsaw
      ) %>% 
      addPolygons(
        stroke = 0.9,
        opacity = 1,
        fillColor = create_color(df_sel[[cols_cat[i]]]),
        weight = 0.6,
        color = "black",
        smoothFactor = 0.8,
        highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = TRUE),
        data = rect_grid
      ) %>% 
      addLegend(
        position = "topright",
        colors = df_sel[[cols_cat[i]]] %>% create_color() %>% unique() %>% sort(),
        labels = df_sel[[cols_cat[i]]] %>% as.factor() %>% levels(),
        title = glue::glue("{cols_cat[i]}")
      ) %>%
      setView(lng = 21.05, lat = 52.23, zoom = 11)
  }
}
plt_lst[[2]]

# VISUALIZE CAT (10+) ----
f <- function(pal) RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[pal, "maxcolors"], pal)

create_color <- function(x) {
  case_when(
    x == "0" ~ "white",
    x == "1" ~ f("Reds")[2],
    x == "2" ~ f("Reds")[3],
    x == "3" ~ f("Reds")[4],
    x == "4" ~ f("Reds")[5],
    x == "4+" ~ f("Reds")[6],
    x == "5" ~ f("Reds")[7],
    x == "6" ~ f("Reds")[8],
    x == "7" ~ f("Reds")[9],
    x == "8" ~ f("Reds")[10],
    x == "9" ~ f("Reds")[11],
    x == "9+" ~ "black",
    x == "unknown" ~ "#c3c3c3",
  )
}

cols_cat <- c("building_floors_num", "floor_no", "rooms_num")
df_sel <- rect_grid@data
plt_lst <- list()
if (FALSE) {
  for (i in seq_along(cols_cat)) {
    flats_vis <- df_sel
    
    colors_sel <- if (!"0" %in% df_sel[[cols_cat[i]]] %>% unique() %>% sort()) {c(f("Reds"), "black", "#c3c3c3")} else {c("white", f("Reds"), "black", "#c3c3c3")}
    
    if (df_sel[[cols_cat[i]]] %>% n_distinct() == 5) {
      colors_sel <- f("Reds")[1:5]
    }
    
    plt_lst[[cols_cat[i]]] <- rect_grid %>% 
      leaflet(options = leafletOptions(attributionControl=FALSE)) %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(minZoom = 11, maxZoom = 11)) %>% 
      addPolygons(
        stroke = 0.5,
        opacity = 0.7,
        fillColor = "grey",
        weight = 0.2,
        color = "grey",
        smoothFactor = 0.8,
        data = warsaw
      ) %>% 
      addPolygons(
        stroke = 0.9,
        opacity = 1,
        fillColor = create_color(df_sel[[cols_cat[i]]]),
        weight = 0.6,
        color = "black",
        smoothFactor = 0.8,
        highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = TRUE),
        data = rect_grid
      ) %>% 
      addLegend(
        position = "topright",
        colors = colors_sel,
        labels = df_sel[[cols_cat[i]]] %>% unique() %>% sort(),
        title = glue::glue("{cols_cat[i]}")
      ) %>%
      setView(lng = 21.05, lat = 52.23, zoom = 11)
  }
}
plt_lst[[3]]
# SAVE ----
write_rds(rect_grid, "data/clean/rect_grid.rds")
