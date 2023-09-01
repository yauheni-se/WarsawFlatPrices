library(tidyverse)
library(plotly)
setwd('C:/Projects/WarsawFlatPrices/')
Sys.setlocale("LC_ALL", "Polish")

df <- data.table::fread("data/flats.csv") %>% as_tibble()

df <- df %>% 
  filter(!is.na(pprice)) %>% 
  select(-c(V1)) %>% 
  rename(
    is_exclusive_offer = isExclusiveOffer,
    is_private_owner = isPrivateOwner,
    is_promoted = isPromoted,
    hide_price = hidePrice,
    date_created = dateCreated,
    date_created_first = dateCreatedFirst,
    area = m
  )

df %>% colnames() %>% sort()

var_sel <- "remote_services"
plot_ly(x = df[[var_sel]], type = "histogram")
df$description[1]
# building_material - unite
# building_type - unite
# building_ownership - recode
# floor_no - recode
# remote_services - recode
# hide_price- drop
# areaInSquareMeters - drop
# owner_name - drop
# p_bheating - drop
# p_bmaterial - drop
# p_btype - drop
# p_bwindows - drop
# pprice - drop
# property - drop
# desc - drop
# title - drop
# description - extract info
# features - extract info

translation_map <- "ąćęłńóśźżĄĆĘŁŃÓŚŹŻ"
replacement_map <- "acelnoszzACELNOSZZ"
clean_letters <- function(x) {x %>% str_trim() %>% unique() %>% .[.!=""] %>% stringi::stri_enc_tonative() %>% chartr(translation_map, replacement_map, .)}

df <- df %>% 
  select(-c("hide_price", "areaInSquareMeters", "owner_name", "p_bheating", "p_bmaterial", "p_btype", "p_bwindows", "pprice", "property", "desc", "title"))%>% 
  mutate(features_lst = iconv(features, "UTF8") %>% str_split(","))

df <- df %>% 
  mutate(features_lst = lapply(features_lst, clean_letters))

unique_features <- df$features_lst %>% unlist() %>% unique()

for (i in unique_features) {
  df[[i]] = map_int(df$features_lst, function(x, y) {ifelse(y %in% x, 1L, 0L)}, y = i)
}

switch_btype <- function(x) {
  case_when(
    x == "" ~ NA_character_,
    x %in% c("house", "tenement", "ribbon", "infill", "loft") ~ "other",
    TRUE ~ x
  )
}

switch_bmat <- function(x) {
  case_when(
    x == "" ~ NA_character_,
    str_detect(x, "concrete") ~ "concrete",
    x == "brick" ~ "brick",
    TRUE ~ "other"
  )
}

switch_owner <- function(x) {
  case_when(
    x == "full_ownership" ~ "full",
    x == "share" ~ "share",
    x == "co_operative_ownership" ~ "cooperative",
    x == "co_operative_ownership_with_a_land_and_mortgage_registe" ~ "cooperative_register",
    TRUE ~ NA_character_
  )
}

df <- df %>% 
  select(-c(features_lst, features)) %>% 
  rename(
    is_closed_territory = `teren zamkniety`,
    is_internet = internet,
    is_domophone = `domofon / wideofon`,
    is_security = `monitoring / ochrona`,
    is_dishwasher = zmywarka,
    is_freezer = lodowka,
    is_furniture = meble,
    is_oven = piekarnik,
    is_stove = kuchenka,
    is_air_cond = klimatyzacja,
    is_balcony = balkon,
    is_basement = piwnica,
    is_garage = `garaz/miejsce parkingowe`,
    is_elevator = winda,
    is_separate_kitchen = `oddzielna kuchnia`,
    is_kable_tv = `telewizja kablowa`,
    is_phone = telefon,
    is_security_windows = `drzwi / okna antywlamaniowe`,
    is_wash_machine = pralka,
    is_terrace = taras,
    is_utility_room = `pom. uzytkowe`,
    is_alarm = `system alarmowy`,
    is_garden = ogrodek,
    is_two_level = dwupoziomowe,
    is_tv = telewizor,
    is_security_blinds = `rolety antywlamaniowe`
  ) %>% 
  mutate(
    date_created = as.Date(date_created),
    date_created_first = as.Date(date_created_first),
    loc = loc %>% iconv("UTF8") %>% trimws(whitespace = ", "),
    loc_label = loc_label %>% iconv("UTF8") %>% trimws(whitespace = ", "),
    is_promoted = as.integer(is_promoted),
    is_exclusive_offer = as.integer(is_exclusive_offer),
    is_private_owner = as.integer(is_private_owner),
    building_type = switch_btype(building_type),
    building_material = switch_bmat(building_material),
    building_ownership = switch_owner(building_ownership),
    floor_no = str_replace(floor_no, "floor_", ""),
    is_remote_service = ifelse(!is.na(remote_services), 1L, 0L),
    object_name = ifelse(object_name == "", NA_character_, object_name) %>% iconv("UTF8"),
    agency_name = ifelse(agency_name == "", NA_character_, agency_name) %>% iconv("UTF8"),
    district = ifelse(district == "", NA_character_, district) %>% iconv("UTF8")
  ) %>% 
  select(-remote_services)

df %>% View()

write_rds(df, "data/clean/flats.rds")