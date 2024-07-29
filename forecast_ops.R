
library(tidymodels)

# if multiple teams in season?

load("data/hitting_stats.RData")

hit <- hit |> 
  filter(n_seasons >= 5) |> 
  complete(nesting(id, name), season) |> #nesting(name, id)) |> 
  arrange(id, name, season) |> 
  group_by(id, name) |> 
  mutate(
    across(
      c(avg, obp, slg, ops), 
      list(lag1 = \(x) lag(x, 1), lag2 = \(x) lag(x, 2))
    )
  ) |> 
  ungroup() |> 
  drop_na(ops, ops_lag1, ops_lag2)

model <- workflow() |> 
  add_recipe(
    recipe(
      ops ~ age + ops_lag1 + ops_lag2, # wt, position
      data = hit
    )
  ) |> 
  add_model(
    boost_tree(mtry = tune(), tree_depth = tune(), learn_rate = tune()) |> 
      set_mode("regression") |> 
      set_engine("xgboost")
  ) |> 
  tune_grid(
    resamples = group_vfold_cv(hit, group = name),
    grid = 3,
    control = control_grid(save_workflow = TRUE, save_pred = TRUE)
  )

model |> 
  fit_best()
