rm(list = ls())

library(sf)
library(tidyverse)
library(leaflet)
library(tmap)
library(tidygraph)
library(spNetwork)

# this function returns sfc coordinates as lon/lat columns
#   https://github.com/r-spatial/sf/issues/231
mke.osm.pedestrian <- read_rds("osm-data/mke-osm-pedestrian-lines-with-components.rds") |>
  st_transform(crs = 32054)

ward.polygons <- read_rds("ward-data/wards-with-polling-places_munged.rds") |>
  select(polling_place_no = polling_place_num, ward_no = WARD,
         geometry = ward_geometry) |>
  st_as_sf()
polling.places <- read_rds("ward-data/wards-with-polling-places_munged.rds") |>
  select(polling_place_no = polling_place_num, polling_place_name = polling_place_name,
         geometry = polling_place_geometry) |>
  st_as_sf() |>
  group_by(polling_place_no) |>
  filter(row_number() == 1) |>
  ungroup()

#polling_place = 137

calculate_polling_place_isochrone <- function(polling_place){
  print(paste("starting polling place", polling_place))
  # wards at this polling place
  target.wards <- ward.polygons |>
    filter(polling_place_no == polling_place)
  
  # coordinate point of this polling place
  polling.place.coords <- polling.places |> filter(polling_place_no == polling_place)
  
  ##############################################################################
  # create combined ward polygon
  combined.wards <- target.wards |> summarise(geometry = st_combine(geometry)) |> st_make_valid()
  # compute distances 
  max.dist <- combined.wards %>% 
    st_cast("POINT") %>% 
    st_distance(st_centroid(combined.wards)) |>
    max()
  buffer.circle <- st_buffer(st_centroid(combined.wards), dist = max.dist*1.5)
  target.wards.osm <- st_intersection(mke.osm.pedestrian, buffer.circle) |>
    st_make_valid() |>
    st_cast("LINESTRING")
  main.component <- target.wards.osm |> st_drop_geometry() |> group_by(component_id) |> summarise(count = n()) |> filter(count == max(count))
  target.wards.osm <- target.wards.osm |> filter(component_id == main.component$component_id)
  ##############################################################################
  
  iso_results <- calc_isochrones(lines = target.wards.osm,
                                 start_points = polling.place.coords,
                                 donught = T,
                                 dists = seq(262, 15720, 262), # 80 meters, or 1-minute walking distances
                                 weight = "length"
  ) |>
    st_make_valid() |>
    #st_intersection(combined.wards) |>
    st_collection_extract("LINESTRING")
  iso_results |>
    mutate(distance_min = distance/262,
           walk_time = paste0((distance_min-1), "-", distance_min, " min."),
           walk_time = fct_reorder(walk_time, distance),
           walk_time5 = case_when(
             distance_min < 5 ~ "0-5 min.",
             distance_min < 10 ~ "5-10 min.",
             distance_min < 15 ~ "10-15 min.",
             distance_min < 20 ~ "15-20 min.",
             distance_min < 25 ~ "20-25 min.",
             distance_min < 30 ~ "25-30 min.",
             distance_min < 35 ~ "30-35 min.",
             distance_min < 40 ~ "35-40 min.",
             distance_min < 45 ~ "40-45 min.",
             distance_min < 50 ~ "45-50 min.",
             TRUE ~ "50+ min."
           ),
           walk_time5 = fct_reorder(walk_time5, distance_min),
           polling_place_no = polling_place)
}

iso.137 <- calculate_polling_place_isochrone(137)

tm_shape(iso.137) +
  tm_lines(col = "walk_time5", palette = "viridis") +
  tm_shape(polling.places[polling.places$polling_place_no == 137,]) +
  tm_bubbles(col = "red") +
  tm_layout(bg.color = "aliceblue", frame = FALSE)

#saveRDS(iso.137, "isochrones/lines/137.rds")
# build and save all isochrones
map(.x = unique(polling.places$polling_place_no),
    .f = function(.x){
      calculate_polling_place_isochrone(.x) |>
        saveRDS(file = paste0("isochrones/lines/", .x, ".rds"))
    },
    .progress = T)
