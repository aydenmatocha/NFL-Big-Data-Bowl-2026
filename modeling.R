library(tidyverse)
library(xgboost)
library(tidymodels)

plays <- combined_summary %>%
  initial_split(prop=0.8, strata = actual_closing_sep)
set.seed(8)

plays_train <- training(plays)
plays_test <- testing(plays)
plays_folds <- vfold_cv(plays_train, v = 10)

xgboost_spec <- 
  boost_tree(trees = 200,
             min_n = tune(), tree_depth = tune(), 
             learn_rate = tune(), loss_reduction = tune(), 
             sample_size = tune()) %>%
  set_mode("regression") %>%
  set_engine("xgboost") 
xgboost_spec

xgboost_recipe <- 
  recipe(formula = actual_closing_sep ~  . , data = plays_train) %>%
  step_rm(game_id, play_id, def_nfl_id, off_nfl_id,
          team_coverage_man_zone,
          play_direction) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors())  %>%
  step_corr(all_numeric_predictors(), threshold = 0.9)

xgboost_workflow <- 
  workflow() %>%
  add_recipe(xgboost_recipe) %>%
  add_model(xgboost_spec) 
xgboost_workflow

set.seed(49)
xgboost_tune <- tune_grid(
  xgboost_workflow,
  resamples = plays_folds,
  grid = 3
)
show_best(xgboost_tune, metric = "rmse")

xgboost_best <- select_best(xgboost_tune, metric = "rsq")

final_xgboost_wkfl <- xgboost_workflow %>%
  finalize_workflow(xgboost_best)

# final_xgboost_fit <- final_xgboost_wkfl %>%
#   fit(plays_train)
# 
# xgboost_predictions <- augment(final_xgboost_fit, new_data = plays_test)
# 
# xgboost_table <- xgboost_predictions %>%
#   metrics(truth = actual_closing_sep, estimate = .pred)
# 
# xgboost_table %>%
#   knitr::kable()


set.seed(49)
full_folds <- vfold_cv(combined_summary, v = 10, strata = actual_closing_sep)

oof_res <- fit_resamples(
  final_xgboost_wkfl,
  resamples = full_folds,
  control = control_resamples(save_pred = TRUE)
)

oof_predictions <- collect_predictions(oof_res) %>%
  # .row lets you join back to combined_summary's row order
  arrange(.row) %>%
  bind_cols(combined_summary %>% select(def_nfl_id, game_id, play_id))

xgboost_table <- oof_predictions %>%
  metrics(truth = actual_closing_sep, estimate = .pred)
 
xgboost_table %>%
  knitr::kable()

