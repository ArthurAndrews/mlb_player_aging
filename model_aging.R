library(tidyverse)
library(lme4, exclude = c("expand", "pack", "unpack"))
library(broom.mixed)
library(janitor)
library(splines)
library(factoextra)
library(magrittr, include.only = "set_rownames")

load("../data/hitting_stats.RData")





hit <- hit |> 
  filter(n_seasons >= 5) 

df <- 3
  
age_range <- seq(
  hit$centered_age |> min(),
  hit$centered_age |> max(),
  length.out = 500
)

# fit the spline model
spline_model <- age_range |> 
  # ns(df = df) 
  bs(df = df)

# add splines to data
hit <- hit |> 
  add_splines(spline_model)

# plot splines
hit |> 
  select(age, contains("spline")) |> 
  pivot_longer(-age) |> 
  ggplot() + 
  aes(age, value, color = name) + 
  geom_line()

form1 <- ops ~ (spline1 + spline2 + spline3 | name) + spline1 + spline2 + spline3
form2 <- ops ~ (centered_age | name) + spline1 + spline2 + spline3

model1 <- lmer(form1, hit)
model2 <- lmer(form1, hit)

mean_player <- tibble(age = 19:40, centered_age = age - mean(hit$age)) |> 
  add_splines(spline_model) |> 
  add_prediction(model, re.form = ~0)

mean_player |> 
  ggplot() + 
  aes(age, pred_ops) + 
  geom_line()

model |> 
  tidy("ran_pars")

model |> 
  tidy("ran_vals")

cf <- model |> 
  tidy("ran_vals") |>
  pivot_wider(id_cols = level, names_from = term, values_from = estimate) |> 
  clean_names()

cf |> 
  select(spline1:spline3) |> 
  as.matrix() |> 
  set_rownames(cf$name) |> 
  prcomp() |> 
  fviz_pca_ind()

cf |> 
  select(intercept, spline1:spline3) |> 
  as.matrix() |> 
  set_rownames(cf$name) |> 
  prcomp() |> 
  fviz_pca_ind()

broom.mixed::tidy

grouped_hit <- hit |> 
  group_by(id) |> 
  mutate(
    diff_28 = abs(age - 28),
    age_28_season = diff_28 == min(diff_28),
    age_28_ops = ops[age_28_season],
    age_28_pa = pa[age_28_season]
  ) |> 
  ungroup() |> 
  group_by(age = floor(age)) |> 
  mutate(player_contribution = pa / sum(pa)) 

aggregate_player <- grouped_hit |> 
  summarize(
    across(c(avg, obp, slg, ops), \(x) weighted.mean(x, pa)),
    age_28_ops = weighted.mean(age_28_ops, age_28_pa),
    .groups = "drop"
  )

aging <- mean_player |> 
  left_join(aggregate_player, by = "age") 

aging |> 
  select(age, modeled = pred_ops, aggregate = ops) |> 
  pivot_longer(-age) |> 
  ggplot() + aes(age, value, color = name) + geom_line()

aging |> 
  ggplot() + aes(age, age_28_ops) + geom_line(color = "purple")

grouped_hit |> 
  slice_max(player_contribution, n = 5) |> 
  ungroup() |> 
  select(age, name, age_28_ops, player_contribution) |> 
  filter()

m |> 
  left_join(mean_aging) |> 
  select(age, pred_ops, ops) |> 
  pivot_longer(-age) |> 
  ggplot() + aes(age, value, color = name) + geom_line()
