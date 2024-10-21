rm(list = ls())

library(tidyverse)
library(sf)
library(tmap)
library(leaflet)

polygons.walk_time4cat <- map(.x = list.files("isochrones/polygons-trimmed", full.names = T,
                                              pattern = "walk_time4cat_"),
                              .f = ~read_rds(.x),
                              .progress = T) |>
  list_rbind() |>
  st_as_sf() |>
  st_transform(crs = 4326)

all.wards <- read_rds("ward-data/wards-with-polling-places_munged.rds") |>
  rename(polling_place_no = polling_place_num, ward_no = WARD,
         geometry = ward_geometry)

all.ward.stats <- read_csv("distance-data/ward-summary-stats-long.csv")
all.ward.labels <- all.ward.stats |>
  inner_join(all.wards |> select(ward_no, polling_place_no)) |>
  group_by(ward_no) |>
  filter(!is.na(units)) |>
  summarise(ward_label = paste0("Ward ", unique(ward_no), " votes at ",
                                "<a href='https://github.com/BlueBookMKE/walking-to-the-polls/blob/main/graphics/indiv-polling-place/walk_time4cat_", unique(polling_place_no), ".png'>",
                                unique(polling_place_name), "</a>.<br>",
                               "It has ", prettyNum(unique(voter_count), ","), " registered voters.<br>",
                               round(pct[distance == "0-5 min."]), "% live 0-5 min. away.<br>",
                               round(pct[distance == "5-10 min."]), "% live 5-10 min. away.<br>",
                               round(pct[distance == "10-15 min."]), "% live 10-15 min. away.<br>",
                               round(pct[distance == "15+ min."]), "% live 15+ min. away.<br>"))
all.wards.2 <- all.wards |>
  st_set_geometry("geometry") |>
  st_transform(crs = 4326) |>
  left_join(all.ward.labels)

polling.place.coords <- all.wards.2 |>
  group_by(polling_place_no) |>
  filter(row_number() == 1) |>
  ungroup() |>
  st_set_geometry("polling_place_geometry") |>
  st_transform(crs = 4326)

distpal <- colorFactor(palette = c("#90789f","#87a6be","#8ad1b2","#f7ed8a"),
            domain = polygons.walk_time4cat$distance)

leaflet.walk_time4cat <- polygons.walk_time4cat |>
  leaflet() |>
  addMapPane("background_map", zIndex = 410) %>%  # Level 1: bottom
  addMapPane("polygons", zIndex = 420) %>%        # Level 2: middle
  addMapPane("wards", zIndex = 430) %>%        # Level 3: middle
  addMapPane("polling_places", zIndex = 440) %>%        # Level 4: middle
  addMapPane("labels", zIndex = 450) %>%          # Level 5: top
  addProviderTiles(provider = providers$CartoDB.PositronNoLabels,
                   options = pathOptions(pane = "background_map")) |>
  addPolygons(fillColor = ~distpal(distance), fillOpacity = 0.75,
              stroke = F,
              options = pathOptions(pane = "polygons")) |>
  addCircleMarkers(data = polling.place.coords,
                   label = ~polling_place_name, radius = 1, color = "red",
                   options = pathOptions(pane = "polling_places")) |>
  addLegend(position = "topright", pal = distpal, values = ~distance,
            opacity = 1) |>
  addProviderTiles(provider = providers$CartoDB.DarkMatterOnlyLabels,
                   options = pathOptions(pane = "labels")) |>
  addPolygons(data = all.wards.2, fill = T, fillOpacity = 0,
              weight = 1, color = "black",
              options = pathOptions(pane = "wards"),
              label = ~lapply(ward_label, htmltools::HTML),
              popup = ~lapply(ward_label, htmltools::HTML))
htmlwidgets::saveWidget(leaflet.walk_time4cat, "graphics/all-ward-iso-polygons_walktime4cat.html",
                        selfcontained = T)
