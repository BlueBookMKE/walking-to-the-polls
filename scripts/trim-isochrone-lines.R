rm(list = ls())

library(tidyverse)
library(sf)

ward.polygons <- read_rds("ward-data/wards-with-polling-places_munged.rds") |>
  select(polling_place_no = polling_place_num, ward_no = WARD,
         geometry = ward_geometry) |>
  st_as_sf()

trim_isochrone <- function(polling_place){
  iso.sf <- read_rds(paste0("isochrones/lines/", polling_place, ".rds"))
  suppressWarnings(st_intersection(x = iso.sf,
                                   y = ward.polygons |>
                                     filter(polling_place_no == polling_place))) |>
    saveRDS(paste0("isochrones/lines-trimmed/", polling_place, ".rds"))
}

map(.x = unique(ward.polygons$polling_place_no),
    .f = ~trim_isochrone(.x),
    .progress = T)
