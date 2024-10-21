rm(list = ls())

library(tidyverse)
library(sf)

polygons.walk_time4cat <- map(.x = list.files("isochrones/polygons-trimmed", full.names = T,
                                              pattern = "walk_time4cat_"),
                              .f = ~read_rds(.x),
                              .progress = T) |>
  list_rbind() |>
  st_as_sf() |>
  select(polling_place_no, distance, distance_n, ward_no)

mprop <- st_read("mprop-data/parcelpolygontax2023/ParcelPolygonTax.shp")
parcels.to.walkpolygons <- mprop |>
  select(Taxkey, resi_units = Residentia, geometry) |>
  filter(resi_units > 0) |>
  mutate(geometry = st_centroid(geometry)) |>
  st_join(polygons.walk_time4cat)

ward.stats <- parcels.to.walkpolygons |>
  st_drop_geometry() |>
  filter(!is.na(distance)) |>
  group_by(ward_no, distance) |>
  summarise(units = sum(resi_units)) |>
  mutate(pct = units/sum(units)*100) |>
  ungroup()

ward.stats |>
  group_by(distance) |>
  summarise(units = sum(units)) |>
  mutate(pct = units/sum(units)*100)

saveRDS(ward.stats, "distance-data/ward-units-by-distance.rds")
