rm(list = ls())

library(tidyverse)
library(sf)
library(tmap)
library(concaveman)
options(scipen = 999)

# turn some lines into polygons
polygonize_lines <- function(lines){
  coords <- st_coordinates(lines)
  poly_coords <- concaveman(points = coords, concavity = 3)
  poly <- st_polygon(list(poly_coords[,1:2]))
  poly
}

# remove any polygons that are entirely contained by 1 or more nearer polygons
remove_contained <- function(iso_polygons){
  polygon.levels <- iso_polygons$distance_n[iso_polygons$distance_n > 1]
  list.contained <- map(.x = polygon.levels,
                        .f = ~st_within(x = filter(iso_polygons, distance_n == .x),
                                        y = filter(iso_polygons, distance_n < .x)) |> 
                          as.list() |> first())
  
  iso_polygons |>
    filter(! distance_n %in% polygon.levels[which(lengths(list.contained) > 0)])
}

# remove the area of the nearer polygons from each overlapping polygon
erase_overlaps <- function(iso_polygons){
  polygon.levels <- iso_polygons$distance_n[iso_polygons$distance_n > 1]
  map(.x = polygon.levels,
      .f = ~rmapshaper::ms_erase(target = filter(iso_polygons, distance_n == .x),
                                 erase = filter(iso_polygons, distance_n < .x))) |>
    list_rbind() |>
    rbind(iso_polygons[iso_polygons$distance_n == 1,]) |>
    filter(!st_is_empty(geometry)) |>
    arrange(distance) |>
    st_as_sf()
}

# creating the polygons for each isochrone
polygonize_iso <- function(iso, distvar){
  distvar.levels <- levels(iso[[distvar]])
  
  # create the iso polygons
  polygons <- map(.x = distvar.levels,
                  .f = ~polygonize_lines(iso[iso[[distvar]] == .x,]))
  
  # create sf object from polygons
  polygons.sf <- st_sf(
    polling_place_no = unique(iso$polling_place_no),
    distance = factor(distvar.levels, levels = distvar.levels),
    distance_n = 1:length(polygons),
    geometry = polygons,
    crs = st_crs(iso)
  )
  
  polygons.sf <- remove_contained(polygons.sf)
  
  erase_overlaps(polygons.sf)
}


# demo
# iso137 <- read_rds("isochrones/lines/137.rds")
# polygonize_iso(iso137, "walk_time5") |>
#   tm_shape() +
#   tm_polygons() +
#   tm_facets(by = "distance")

# create and save all polygons
map(.x = list.files("isochrones/lines", full.names = T),
    .f = function(.x){
      polygonize_iso(iso = read_rds(.x) |>
                       mutate(
                         walk_time4cat = case_when(
                           distance_min < 5 ~ "0-5 min.",
                           distance_min < 10 ~ "5-10 min.",
                           distance_min < 15 ~ "10-15 min.",
                           TRUE ~ "15+ min."),
                         walk_time4cat = fct_reorder(walk_time4cat, distance_min)),
                     distvar = "walk_time4cat") |>
        saveRDS(file = paste0("isochrones/polygons/walk_time4cat_", word(.x, -1, sep = "/")))
    },
    .progress = TRUE)
