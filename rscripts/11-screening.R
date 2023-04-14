## Setup ---------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(scales)

## Cambia el default del tamaño de fuente 
theme_set(theme_linedraw(base_size = 25))

## Cambia el número de decimales para mostrar
options(digits = 4)
## Problemas con mi consola en Emacs
options(pillar.subtle = FALSE)
options(rlang_backtrace_on_error = "none")
options(crayon.enabled = FALSE)

## Para el tema de ggplot
sin_lineas <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)

sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), axis.text = element_blank())

library(tidymodels)
data(concrete, package = "modeldata")
concrete |> print(n = 3, width = 70)

concrete <- 
  concrete |> 
  group_by(across(-compressive_strength)) |> 
  summarize(compressive_strength = mean(compressive_strength),
            .groups = "drop")

set.seed(1501)
concrete_split <- initial_split(concrete, strata = compressive_strength)
concrete_train <- training(concrete_split)
concrete_test  <- testing(concrete_split)

set.seed(1502)
concrete_folds <- 
  vfold_cv(concrete_train, strata = compressive_strength, repeats = 5)

normalized_rec <- 
  recipe(compressive_strength ~ ., data = concrete_train) |> 
  step_normalize(all_predictors()) 

poly_recipe <- 
  normalized_rec |> 
  step_poly(all_predictors()) |> 
  step_interact(~ all_predictors():all_predictors())

knn_spec <- 
  nearest_neighbor(neighbors = tune(),
                   dist_power = tune(),
                   weight_func = tune()) |> 
  set_engine("kknn") |> 
  set_mode("regression")

linear_reg_spec <- 
  linear_reg(penalty = tune(), mixture = tune()) |> 
  set_engine("glmnet")

library(baguette)
cart_spec <- 
  decision_tree(cost_complexity = tune(), min_n = tune()) |> 
  set_engine("rpart") |> 
  set_mode("regression")

bag_cart_spec <- 
  bag_tree() |> 
  set_engine("rpart", times = 50L) |> 
  set_mode("regression")

rf_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) |> 
  set_engine("ranger") |> 
  set_mode("regression")

xgb_spec <- 
  boost_tree(tree_depth = tune(), learn_rate = tune(),
             loss_reduction = tune(), 
             min_n = tune(), sample_size = tune(),
             trees = tune()) |> 
  set_engine("xgboost") |> 
  set_mode("regression")

normalized <- 
  workflow_set(
    preproc = list(normalized = normalized_rec), 
    models = list(KNN = knn_spec)
  )
normalized

normalized |> extract_workflow(id = "normalized_KNN")

model_vars <- workflow_variables(
  outcomes = compressive_strength, 
  predictors = everything()
)

no_pre_proc <- workflow_set(
  preproc = list(simple = model_vars), 
  models = list(CART = cart_spec,
                CART_bagged = bag_cart_spec,
                RF = rf_spec,
                boosting = xgb_spec)
)
no_pre_proc

with_features <- 
  workflow_set(
    preproc = list(fullquad = poly_recipe), 
    models = list(linear_reg = linear_reg_spec, KNN = knn_spec)
  )

all_workflows <- 
  bind_rows(no_pre_proc, normalized, with_features) |> 
  ## Make the workflow ID's a little more simple: 
  mutate(wflow_id = gsub("(simple_)|(normalized_)", "", wflow_id))
all_workflows

grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

all_cores <- parallel::detectCores(logical = TRUE) - 3
library(doParallel)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

system.time(
  grid_results <- all_workflows |>
    workflow_map(
      seed = 1503,
      resamples = concrete_folds,
      grid = 25,
      control = grid_ctrl
    )
)

grid_results

grid_results |> 
 rank_results(select_best = TRUE) |> 
 filter(.metric == "rmse") |> 
 select(model, .config, rmse = mean, rank)

autoplot(
  grid_results,
  rank_metric = "rmse",  # <- how to order models
  metric = "rmse",       # <- which metric to visualize
  select_best = TRUE     # <- one point per workflow
) +
  geom_text(aes(y = mean - 1/2, label = wflow_id), angle =45, hjust = 1, size = 7) +
  theme(legend.position = "none") + sin_lineas +
  coord_cartesian(ylim = c(2.5, 9.5))

library(finetune)

race_ctrl <-
  control_race(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

system.time(
  race_results <-
    all_workflows |>
    workflow_map(
      "tune_race_anova",
      seed = 1503,
      resamples = concrete_folds,
      grid = 25,
      control = race_ctrl
    ))

race_results

autoplot(
  race_results,
  rank_metric = "rmse",  
  metric = "rmse",       
  select_best = TRUE    
) +
  geom_text(aes(y = mean - 1/2, label = wflow_id), angle = 45, hjust = 1, size = 7) +
  theme(legend.position = "none") + sin_lineas + 
  coord_cartesian(ylim = c(2.5, 9.5))

matched_results <- 
  rank_results(race_results, select_best = TRUE) |> 
  select(wflow_id, .metric, race = mean, config_race = .config) |> 
  inner_join(
    rank_results(grid_results, select_best = TRUE) |> 
    select(wflow_id, .metric, complete = mean, 
           config_complete = .config, model),
    by = c("wflow_id", ".metric"),
    ) |>  
  filter(.metric == "rmse")

library(ggrepel)

matched_results |> 
  ggplot(aes(x = complete, y = race)) + 
  geom_abline(lty = 3) + 
  geom_point() + 
  geom_text_repel(aes(label = model)) +
  coord_obs_pred() + 
  labs(x = "Complete Grid RMSE", y = "Racing RMSE")  +
  sin_lineas

best_results <- 
  race_results |> 
  extract_workflow_set_result("boosting") |> 
  select_best(metric = "rmse")
best_results

boosting_test_results <- 
   race_results |> 
   extract_workflow("boosting") |> 
   finalize_workflow(best_results) |> 
   last_fit(split = concrete_split)

collect_metrics(boosting_test_results)

boosting_test_results |> 
  collect_predictions() |> 
  ggplot(aes(x = compressive_strength, y = .pred)) + 
  geom_abline(color = "gray50", lty = 2) + 
  geom_point(alpha = 0.5) + 
  coord_obs_pred() + 
  labs(x = "observed", y = "predicted") +
  sin_lineas

library(stacks)

concrete_stack <- 
  stacks() |> 
  add_candidates(race_results)

concrete_stack

set.seed(2001)
ens <- blend_predictions(concrete_stack)

autoplot(ens) + sin_lineas

set.seed(2002)
ens <- blend_predictions(concrete_stack, penalty = 10^seq(-2, -0.5, length = 20))

autoplot(ens) + sin_lineas

ens

autoplot(ens, "weights") +
  geom_text(aes(x = weight + 0.01, label = model), hjust = 0, size = 5) + 
  theme(legend.position = "none") +
  lims(x = c(-0.01, 1)) + sin_lineas

ens <- fit_members(ens)

reg_metrics <- metric_set(rmse, rsq)
ens_test_pred <- 
  predict(ens, concrete_test) |> 
  bind_cols(concrete_test)

ens_test_pred |> 
  reg_metrics(compressive_strength, .pred)
