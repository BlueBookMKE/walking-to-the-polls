rm(list = ls())

library(tidyverse)
library(sf)
library(tmap)
tmap_options(show.messages = F)

all.wards <- read_rds("ward-data/wards-with-polling-places_munged.rds") |>
  rename(polling_place_no = polling_place_num, ward_no = WARD,
         geometry = ward_geometry) |>
  st_as_sf()

# polling_place = 137

map_polling_place <- function(polling_place, distance, pal = "viridis", droplevels = T){
  polling.place.wards <- all.wards |>
    filter(polling_place_no == polling_place) |>
    st_set_geometry("geometry")
  polling.place.coords <- polling.place.wards[1,] |> st_set_geometry("polling_place_geometry")
  lines <- read_rds(paste0("isochrones/lines/", polling_place, ".rds"))
  polygons <- read_rds(paste0("isochrones/polygons-trimmed/", distance, polling_place, ".rds"))
  if(droplevels == T){
    polygons <- polygons |> mutate(distance = fct_drop(distance))
  }
  
  tm_shape(polygons, bbox = st_bbox(lines)) +
    tm_polygons(col = "distance", title.col = "distance (min.)",
                palette = pal, border.col = "white") +
    tm_shape(polling.place.wards) +
    tm_text("ward_no", fontface = "bold", size = 2.5,
            col = "white") +
    tm_shape(lines) +
    tm_lines(lwd = 0.75) +
    tm_shape(polling.place.wards) +
    tm_borders(col = "seashell", lwd = 4, alpha = 0.75) +
    tm_shape(polling.place.coords) +
    tm_bubbles(size = 0.75, col = "red", alpha = 0.75) +
    tm_layout(bg.color = "aliceblue", outer.bg.color = "aliceblue",
              frame = FALSE, main.title.fontface = "bold",
              main.title = str_wrap(paste("Walking time to", 
                                          polling.place.coords$polling_place_name,
                                          "from ward(s)", 
                                          knitr::combine_words(polling.place.wards$ward_no)),
                                    39),
              legend.position = c("left","bottom"))
}

# test
# tm1 <- map_polling_place(1, "walk_time4cat_")
# tmap_save(tm1, "~/downloads/tm1.png", width = 7, height = 7, units = "in")

map(.x = unique(all.wards$polling_place_no),
    .f = ~map_polling_place(.x, distance = "walk_time4cat_", droplevels = F,
                            pal = c("#90789f","#87a6be","#8ad1b2","#f7ed8a")) |>
      tmap_save(filename = paste0("graphics/indiv-polling-place/walk_time4cat_", .x, ".png"),
                width = 7, height = 7, units = "in"),
    .progress = TRUE)

map(.x = unique(all.wards$polling_place_no),
    .f = ~map_polling_place(.x, distance = "walk_time5_") |>
      tmap_save(filename = paste0("graphics/indiv-polling-place/walk_time5_", .x, ".png"),
                width = 7, height = 7, units = "in"),
    .progress = TRUE)
