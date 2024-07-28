# query season hitter stats over time range and player details (to calculate age)
# and save data frames to disk
# Arthur Andrews
# July 2024

library(tidyverse)
library(baseballr)
library(magrittr, include.only = "divide_by")


# query hitter stats
hitter <- tibble(season = 2005:2023) |> 
  pmap(
    partial(mlb_stats, stat_type = "season", stat_group = "hitting", player_pool = "All")
  ) |> 
  bind_rows() |> 
  filter(at_bats > 200)


# get unique ids
ids <- hitter |> 
  drop_na(player_id) |> 
  pull(player_id) |> 
  unique()

# query player data.  need this to calculate age.
# this fails.  i think the vector of ids is too long and the API complains.
# player <- ids |> 
#   mlb_people()


# query in chunks instead
player <- hitter |> 
  drop_na(player_id) |> 
  distinct(player_id) |> 
  mutate(q = ntile(player_id, 10)) |> 
  nest(id = player_id) |> 
  mutate(
    data = id |> 
      map(pull, player_id) |> 
      map(mlb_people)
  ) |> 
  select(-id) |> 
  unnest(data)
  
# join birth date and calculate age at mid-season
hitter <- hitter |> 
  select(-any_of(c("birth_date", "age"))) |> 
  left_join(
    player |> 
      select(id, birth_date),
    by = join_by(player_id == id)
  ) |> 
  mutate(
    across(birth_date, as_date),
    mid_season = str_glue("{season}-07-1") |> 
      as_date(),
     age = (mid_season - birth_date) |> 
      as.numeric(units = "days") |>
      divide_by(365.25)
  )

# smaller set of columns
hit <- hitter |> 
  select(
    id = player_id, name = player_full_name, season, team_name, 
    ab = at_bats, pa = plate_appearances, age, avg, obp, slg, ops
  ) |> 
  arrange(id, season) |> 
  group_by(id) |> 
  mutate(n_seasons = n_distinct(season)) |> 
  ungroup() |> 
  mutate(
    across(c(avg:ops), as.numeric),
    centered_age = age - mean(age)
  )

# save
save(hitter, player, hit, file = "data/hitting_stats.RData")
