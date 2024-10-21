rm(list = ls())

library(tidyverse)
library(sf)

ward.polygons <- read_rds("ward-data/wards-with-polling-places_munged.rds") |>
  select(polling_place_no = polling_place_num, ward_no = WARD,
         geometry = ward_geometry) |>
  st_as_sf()

trim_isochrone <- function(polling_place, distance){
  iso.sf <- read_rds(paste0("isochrones/polygons/", distance, polling_place, ".rds"))
  iso.sf.trimmed <- suppressWarnings(st_intersection(x = iso.sf,
                                                     y = ward.polygons |>
                                                       filter(polling_place_no == polling_place)))
  
  iso.sf.trimmed |>
    #rmapshaper::ms_simplify() |>
    saveRDS(paste0("isochrones/polygons-trimmed/", distance, polling_place, ".rds"))
}

map(.x = unique(ward.polygons$polling_place_no),
    .f = ~trim_isochrone(.x, distance = "walk_time5_"),
    .progress = T)
map(.x = unique(ward.polygons$polling_place_no),
    .f = ~trim_isochrone(.x, distance = "walk_time4cat_"),
    .progress = T)
