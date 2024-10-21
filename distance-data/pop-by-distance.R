rm(list = ls())

library(tidyverse)
library(sf)

block.demog <- read_csv("/Users/johnsonjoh/Dropbox/Projects/2024/January/wi-legis-map-proposals-2024/census-blocks/wi-blocks-simple.csv") |>
  mutate(GEOID = as.character(GEOID))

block.shp <- st_read("/Users/johnsonjoh/Dropbox/Projects/2024/January/wi-legis-map-proposals-2024/census-blocks/WI_BLOCKS_2020_TIGER_PL94171.geojson")
mke.block.shp <- block.shp |>
  filter(str_detect(GEOID, "^55079"))

polygons.walk_time4cat <- map(.x = list.files("isochrones/polygons-trimmed", full.names = T,
                                              pattern = "walk_time4cat_"),
                              .f = ~read_rds(.x),
                              .progress = T) |>
  list_rbind() |>
  st_as_sf() |>
  select(polling_place_no, distance, distance_n, ward_no) |>
  st_transform(crs = st_crs(mke.block.shp))

mprop <- st_read("mprop-data/parcelpolygontax2023/ParcelPolygonTax.shp")
parcels.to.walkpolygons <- mprop |>
  st_transform(crs = st_crs(polygons.walk_time4cat)) |>
  select(Taxkey, resi_units = Residentia, geometry) |>
  filter(resi_units > 0) |>
  mutate(geometry = st_centroid(geometry)) |>
  st_join(polygons.walk_time4cat) |>
  st_join(mke.block.shp)

blocks.to.distance <- parcels.to.walkpolygons |>
  st_drop_geometry() |>
  tibble() |>
  filter(!is.na(distance),
         !is.na(GEOID)) |>
  group_by(GEOID, distance) |>
  summarise(resi_units = sum(resi_units)) |>
  mutate(total_units = sum(resi_units),
         prop_of_block = resi_units/total_units) |>
  ungroup()

distance.demogs <- blocks.to.distance |>
  select(GEOID, distance, prop_of_block) |>
  inner_join(block.demog |>
               select(GEOID, starts_with("pop")) |>
               pivot_longer(cols = -GEOID, names_to = "race", values_to = "pop"),
             relationship = "many-to-many") |>
  mutate(pop = pop*prop_of_block) |>
  group_by(race, distance) |>
  summarise(pop = sum(pop)) |>
  mutate(pct_of_race = pop/sum(pop)*100) |>
  ungroup()
distance.demogs |>
  select(-pop) |>
  pivot_wider(names_from = race, values_from = pct_of_race) |>
  select(distance, pop, pop_black, pop_white, pop_hisp, pop_asian)
saveRDS(distance.demogs, "distance-data/pop-race-by-distance.rds")
