rm(list = ls())

library(tidyverse)
library(sf)
library(leaflet)

ward.distances <- read_rds("distance-data/ward-units-by-distance.rds") |>
  complete(ward_no, distance) |>
  mutate(units = if_else(is.na(units), 0, units),
         pct = if_else(is.na(pct), 0, pct))
rv.wi <- readxl::read_excel("ward-data/VoterCountsByWard_12.xlsx")
rv.mke <- rv.wi |>
  janitor::clean_names() |>
  filter(str_detect(muni, "^CITY OF MILWAUKEE")) |>
  mutate(ward_no = as.numeric(word(ward_code, -1, sep = "-"))) |>
  select(ward_no, voter_count)
polling.place <- read_rds("ward-data/wards-with-polling-places_munged.rds") |>
  rename(ward_no = WARD)

polling.place
all.ward.stats <- ward.distances |>
  full_join(polling.place |> select(ward_no, polling_place_name)) |>
  left_join(rv.mke) |>
  mutate(voter_count = if_else(is.na(voter_count), 0, voter_count))

all.ward.stats |>
  filter(is.na(distance)) |>
  inner_join(polling.place) |>
  st_set_geometry("ward_geometry") |>
  st_transform(crs = 4326) |>
  leaflet() |>
  addTiles() |>
  addPolygons(label = ~ward_no)

df <- all.ward.stats |>
  mutate(pct = paste0(round(pct), "%")) |>
  group_by(ward_no) |>
  summarise(wardstring = if_else(
    max(voter_count) == 0,
    true = paste("Ward", unique(ward_no), "/", "Unpopulated"),
    false = paste(
      "Ward", unique(ward_no), "/",
      pct[distance == "0-5 min."], "/",
      pct[distance == "5-10 min."], "/",
      pct[distance == "10-15 min."], "/",
      pct[distance == "15+ min."], "/",
      unique(polling_place_name))
  ))

write_csv(df, "distance-data/ward-summary-stats.csv")
write_csv(all.ward.stats, "distance-data/ward-summary-stats-long.csv")

all.pollingplace.stats <- all.ward.stats |>
  filter(!is.na(units)) |>
  group_by(polling_place_name, distance) |>
  summarise(units = sum(units, na.rm = T),
            voter_count = sum(voter_count, na.rm = T),
            wards = paste(ward_no, collapse = ", ")) |>
  mutate(pct = units/sum(units)*100,
         pct = paste0(round(pct), "%")) |>
  ungroup()
df.place <- all.pollingplace.stats |>
  filter(!is.na(voter_count)) |>
  group_by(polling_place_name) |>
  summarise(placestring = paste(
      unique(polling_place_name), "/",
      pct[distance == "0-5 min."], "/",
      pct[distance == "5-10 min."], "/",
      pct[distance == "10-15 min."], "/",
      pct[distance == "15+ min."], "/",
      "Ward(s):", paste(unique(wards), collapse = ", "))
  )
