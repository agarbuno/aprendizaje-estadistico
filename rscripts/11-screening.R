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
  concrete %>% 
  group_by(across(-compressive_strength)) %>% 
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
  recipe(compressive_strength ~ ., data = concrete_train) %>% 
  step_normalize(all_predictors()) 

poly_recipe <- 
  normalized_rec %>% 
  step_poly(all_predictors()) %>% 
  step_interact(~ all_predictors():all_predictors())

library(rules)
library(baguette)

linear_reg_spec <- 
  linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

mars_spec <- 
  mars(prod_degree = tune()) %>%  #<- use GCV to choose terms
  set_engine("earth") %>% 
  set_mode("regression")

cart_spec <- 
  decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

bag_cart_spec <- 
  bag_tree() %>% 
  set_engine("rpart", times = 50L) %>% 
  set_mode("regression")

knn_spec <- 
  nearest_neighbor(neighbors = tune(),
                   dist_power = tune(),
                   weight_func = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

rf_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

xgb_spec <- 
  boost_tree(tree_depth = tune(), learn_rate = tune(),
             loss_reduction = tune(), 
             min_n = tune(), sample_size = tune(),
             trees = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

cubist_spec <- 
  cubist_rules(committees = tune(), neighbors = tune()) %>% 
  set_engine("Cubist")

normalized <- 
  workflow_set(
    preproc = list(normalized = normalized_rec), 
    models = list(KNN = knn_spec)
  )
normalized

normalized %>% extract_workflow(id = "normalized_KNN")

model_vars <- 
  workflow_variables(outcomes = compressive_strength, 
                     predictors = everything())
no_pre_proc <- 
  workflow_set(
    preproc = list(simple = model_vars), 
    models = list(MARS = mars_spec, CART = cart_spec,
                  CART_bagged = bag_cart_spec,
                  RF = rf_spec, boosting = xgb_spec,
                  Cubist = cubist_spec)
  )
no_pre_proc

with_features <- 
  workflow_set(
    preproc = list(full_quad = poly_recipe), 
    models = list(linear_reg = linear_reg_spec, KNN = knn_spec)
  )

all_workflows <- 
  bind_rows(no_pre_proc, normalized, with_features) %>% 
  ## Make the workflow ID's a little more simple: 
  mutate(wflow_id = gsub("(simple_)|(normalized_)", "", wflow_id))
all_workflows

grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

system.time(
  grid_results <-
    all_workflows %>%
    workflow_map(
      seed = 1503,
      resamples = concrete_folds,
      grid = 25,
      control = grid_ctrl
    )
)
