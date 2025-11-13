library(tidymodels)
library(future)
library(tictoc)
library(pins)
library(vetiver)

data_board <- board_folder(path = 'data/final', versioned = TRUE)
top_1000_books_new_features <- data_board |> pin_read(name = 'dataset', version = "20250924T150811Z-9067c")

#using 90% of the data as training, 10% testing 
#This is ordered by year_month, so the last 10% of the dataset is used as testing set
#This ends up being Dec 2023 - July 2025
books_split <- initial_time_split(top_1000_books_new_features|> arrange(year_month), prop = .9)
training <- training(books_split)
testing <- testing(books_split)

#xgboost_recipe
xgboost_recipe <- recipe(
  checkouts ~ checkout_year + checkout_month + is_new +
    lag_1 + lag_2 + lag_3 + lag_6 + lag_12+ 
    rolling_avg_2_month + rolling_avg_3_month + rolling_avg_4_month + 
    rolling_avg_5_month + rolling_avg_6_month +
    avg_book_checkouts + is_summer + is_december +
    month_sd + months_old + median_book_checkouts+
    is_childrens_book+ checkout_book_sd,
  data = training) |> 
  step_dummy(all_nominal_predictors()) 

# specify spec
xgboost_spec <- 
  # standard set of tree parameters to tune
  boost_tree(trees = tune(),
             learn_rate = tune(),
             tree_depth = tune(),
             min_n = tune()) |> 
  set_mode("regression") |> 
  set_engine("xgboost")

#build tuning grid
grid <- grid_space_filling(
  tree_depth(),
  min_n(),
  learn_rate(),
  trees(),
  size = 10
)

# xgboost workflow
xgboost_workflow <-
  workflow() |> 
  add_recipe(xgboost_recipe) |> 
  add_model(xgboost_spec)

# generate folds for cross validation
folds <- vfold_cv(training)

#set of metrics to be used for tuning and evaluation
my_metrics <- metric_set(rmse, mape, mae)

#fit model and tune grid parameters
#using tic/toc to see how long this takes; using future::plan to fit in parallel
tic()
plan(multisession, workers = 14)
xgboost_tune <- 
  tune_grid(xgboost_workflow, resamples = folds, grid = grid, 
            control = control_grid(verbose = TRUE),#, save_pred = TRUE, save_workflow = TRUE),
            metrics = my_metrics)
toc()

#select the best tuning parameters
best_fit <- select_best(xgboost_tune)

#feed the optimal tuning parameters into the workflow
xgb_fit <- finalize_workflow(xgboost_workflow, best_fit)

#with the best model in hand, fit the final model on the training set; then evaluate on the test set
final <- last_fit(xgb_fit, books_split)

# save pin of vetiver_model- this should be ready to deploy as an api
deployable_model <- xgb_fit |>  fit(training)

# v <- vetiver_model(deployable_model, 'final_model_large')
# 
# modeling_board <- board_folder('data/final', versioned = TRUE)
# modeling_board |> vetiver_pin_write(v)

data_board |>  pin_write(deployable_model, type = 'rds')