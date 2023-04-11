## Setup ---------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(scales)

## Cambia el default del tamaño de fuente 
theme_set(theme_linedraw(base_size = 20))

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

## Ejemplo suavizadores ------------------------------------------------------
library(ISLR)
set.seed(108727)
## Cargamos datos
data <- tibble(Wage) |> select(year, age, wage, education) |>
  mutate(hi.income = ifelse(wage > 250, 1, 0),
         age = as.numeric(age)) |>
  sample_frac(.05)

data |> print(n = 3)

library(ggformula)
g1.ssplines <- data |>
  ggplot(aes(age, wage)) +
  geom_point(color = "gray") +
  geom_spline(aes(age, wage, color = "Suavizamiento"),
            df = 15, 
            color = 'red',
            lty = 1,
            show.legend = TRUE) + 
  sin_lineas +
  ## scale_x_continuous(limits = c(10, 80), expand = c(0,0)) +
  xlab("Edad") + ylab("Ingreso") + ggtitle("df = 15")
  coord_cartesian(ylim = c(0, 300))
g1.ssplines

library("rsample")
ajusta_boot <- function(id){
  ## Creo remuestra
  data.boot <- data |>
    slice_sample(prop = 1, replace = TRUE)
  ## Ajusto modelo 
  model <- smooth.spline(y = data.boot$wage, x = data.boot$age, df = 15)
  ## Hago predicciones y las regreso (ojo no extrapola)
  predict(model, newdata = tibble(age = seq(20, 80))) |>
    as_tibble()
}

boot.fit <- tibble(id = 1:100) |>
  mutate(resultados = map(id, ajusta_boot))

g1.ssplines + 
  geom_line(data = unnest(boot.fit, resultados),
            aes(x, y, group = id),
            color = 'lightblue', alpha = .2) +
  geom_spline(aes(age, wage, color = "Suavizamiento"),
              df = 15, 
              color = 'red',
              lty = 1,
              show.legend = TRUE)

boot.fit <- tibble(id = 1:100) |>
  mutate(resultados = map(id, ajusta_boot))

boot.fit |>
  unnest(resultados) |>
  group_by(x) |>
  summarise(pred.lo = quantile(y, prob = .025),
            pred    = mean(y),
            pred.hi = quantile(y, prob = .975)) |>
  ggplot(aes(x, pred)) +
  geom_ribbon(aes(ymin = pred.lo,
                  ymax = pred.hi),
              fill = "lightblue", alpha = .5) +
  geom_line(color = 'red') +
  geom_point(data = data, aes(age, wage), color = "gray") +
  sin_lineas +
  xlab("Edad") + ylab("Ingreso") + ggtitle("df = 15")

## Aplicación: Misiones de astronautas ---------------------------------------
astronauts <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv")

astronauts |>
  count(in_orbit, sort = TRUE) |>
  print(n = 3)

astronauts |>
  mutate(
    year_of_mission = 10 * (year_of_mission %/% 10),
    year_of_mission = factor(year_of_mission)
  ) |>
  ggplot(aes(year_of_mission, hours_mission,
             fill = year_of_mission, color = year_of_mission
             )) +
  geom_boxplot(alpha = 0.2, size = 1.5, show.legend = FALSE) +
  scale_y_log10() + sin_lineas + 
  labs(x = NULL, y = "Duration of mission in hours")

astronauts_df <- astronauts |>
  select(
    name, mission_title, hours_mission,
    military_civilian, occupation, year_of_mission, in_orbit
  ) |>
  mutate(
    in_orbit = case_when(
      str_detect(in_orbit, "^Salyut") ~ "Salyut",
      str_detect(in_orbit, "^STS") ~ "STS",
      TRUE ~ in_orbit
    ),
    occupation = str_to_lower(occupation)
  ) |>
  filter(hours_mission > 0) |>
  mutate(hours_mission = log(hours_mission)) |>
  na.omit() |>
  select(c(-mission_title, -name))

astronauts_df |>
  print(n = 5, width = 75)

### Preprocesamiento -------------------------------------------------------

set.seed(123)
astro_split <- initial_split(astronauts_df, strata = hours_mission)
astro_train <- training(astro_split)
astro_test <- testing(astro_split)

astro_recipe <- recipe(hours_mission ~ ., data = astro_train) |>
  step_other(occupation, in_orbit,
             threshold = 0.005, other = "Other"
             ) |>
  step_dummy(all_nominal_predictors())

astro_wf <- workflow() |>
  add_recipe(astro_recipe)

astro_wf

### Ajuste de modelo ---------------------------------------------------------

library(baguette)

tree_spec <- bag_tree() |>
  set_engine("rpart", times = 100) |>
  set_mode("regression")

tree_spec

tree_rs <- astro_wf |>
  add_model(tree_spec) |>
  fit(astro_train)

tree_rs

test_rs <- astro_test |>
  bind_cols(predict(tree_rs, astro_test)) |>
  rename(.pred_tree = .pred)

test_rs |> print(n = 5, width = 73)

test_rs |>
  metrics(hours_mission, .pred_tree)

new_astronauts <- crossing(
  in_orbit = fct_inorder(c("ISS", "STS", "Mir", "Other")),
  military_civilian = "civilian",
  occupation = "Other",
  year_of_mission = seq(1960, 2020, by = 10)
) |>
  filter(
    !(in_orbit == "ISS" & year_of_mission < 2000),
    !(in_orbit == "Mir" & year_of_mission < 1990),
    !(in_orbit == "STS" & year_of_mission > 2010),
    !(in_orbit == "STS" & year_of_mission < 1980)
  )

new_astronauts

new_astronauts |>
  bind_cols(predict(tree_rs, new_astronauts)) |>
  ggplot(aes(year_of_mission, .pred, color = in_orbit)) +
  geom_line(size = 1.5, alpha = 0.7) +
  geom_point(size = 2) +
  labs(
    x = NULL, y = "Duration of mission in hours",
    color = NULL, title = "How did the duration of astronauts' missions change over time?",
    subtitle = "Predicted using bagged decision tree model"
  ) + sin_lineas

tree_grid <- grid_random(mtry(c(1,35)),
                         min_n(),
                         size = 20)
tree_grid |> print(n = 5)

regular_res |>
  collect_metrics() |>
  filter(.metric == "roc_auc") |>
  mutate(min_n = factor(min_n)) |>
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, linewidth = 1.5) +
  geom_point() +
  labs(y = "AUC") + sin_lineas

library(vip)

final_res |>
  extract_fit_engine() |>
  vip() + sin_lineas

## Aplicacion: Precios de IKEA ---------------------------------------------
ikea <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv")

ikea |>
  select(`...1`, price, depth:width) |>
  pivot_longer(depth:width, names_to = "dim") |>
  ggplot(aes(value, price, color = dim)) +
  geom_point(alpha = 0.4, show.legend = FALSE) +
  scale_y_log10() +
  facet_wrap(~dim, scales = "free_x") +
  labs(x = NULL) + sin_lineas

ikea_df <- ikea |>
  select(price, name, category, depth, height, width) |>
  mutate(price = log10(price)) |>
  mutate_if(is.character, factor)

ikea_df |> print(n = 5)

### Preporocesamiento --------------------------------------------------------

set.seed(123)
ikea_split <- initial_split(ikea_df, strata = price)
ikea_train <- training(ikea_split)
ikea_test <- testing(ikea_split)

set.seed(234)
ikea_folds <- vfold_cv(ikea_train, strata = price)

library(textrecipes)
ranger_recipe <-
  recipe(formula = price ~ ., data = ikea_train) |>
  step_other(name, category, threshold = 0.01) |>
  step_clean_levels(name, category) |>
  step_impute_knn(depth, height, width)

### Especificación modelo ----------------------------------------------------

ranger_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) |>
  set_mode("regression") |>
  set_engine("ranger")

ranger_workflow <-
  workflow() |>
  add_recipe(ranger_recipe) |>
  add_model(ranger_spec)

all_cores <- parallel::detectCores(logical = TRUE) - 1
library(doParallel)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

set.seed(8577)
system.time(
ranger_tune <-
  tune_grid(ranger_workflow,
            resamples = ikea_folds,
            grid = 11,
            control = control_grid(
              parallel_over = "resamples",
              verbose = TRUE))
)

show_best(ranger_tune, metric = "rmse")

show_best(ranger_tune, metric = "rsq")

final_rf <- ranger_workflow |>
  finalize_workflow(select_best(ranger_tune))

final_rf

ikea_fit <- last_fit(final_rf, ikea_split)
ikea_fit |> select(c(-id, -.notes)) |> print(width = 75)

collect_metrics(ikea_fit)

collect_predictions(ikea_fit) |>
  ggplot(aes(price, .pred)) +
  geom_abline(lty = 2, color = "gray50") +
  geom_point(alpha = 0.5, color = "midnightblue") +
  coord_fixed() + sin_lineas

library(vip)

imp_spec <- ranger_spec |>
  finalize_model(select_best(ranger_tune)) |>
  set_engine("ranger", importance = "permutation")

workflow() |>
  add_recipe(ranger_recipe) |>
  add_model(imp_spec) |>
  fit(ikea_train) |>
  pull_workflow_fit() |>
  vip(aesthetics = list(alpha = 0.8, fill = "midnightblue")) + sin_lineas

### Postprocesamiento --------------------------------------------------------

ranger_prep <- prep(ranger_recipe, training = ikea_train)
rf_model <- randomForest::randomForest(
                            price ~., bake(ranger_prep, ikea_train),
                            mtry = 2, ntree = 1000, nodesize = 4)

collect_forest_predictions <- function(model, data){
  ## Obten predicciones por arbol
  predictions <- predict(model, bake(ranger_prep, data), predict.all = TRUE)
  predictions$individual |>
    ## obten tabla nrows x ncols = nobs x ntrees
    as_tibble() |>
    mutate(observation = 1:nrow(data),
           ## agrega response
           truth = data$price) |>
    ## obten tabla de nrows = nobs x ntrees
    pivot_longer(cols = 1:1000,
                 values_to = ".prediction", names_to = ".tree")  |>
    group_by(observation) |>
    ## reordena los arboles de manera aleatoria
    sample_frac(1, replace = FALSE) |>
    ## calcula prediccion del bosque
    mutate(.estimate = cummean(.prediction),
           .order = 1:n()) |>
    ungroup() |> select(c(-.prediction)) |>
    nest(data = c(observation, truth, .estimate, .tree)) |>
    ## calcula metrica de error
    mutate(results = map(data, function(x) { x |> rmse(truth, .estimate) })) |>
    select(-data)
}

nexp <- 100; set.seed(108)
predictions_train <- tibble(.expid = 1:nexp) |>
  mutate(.performance = map(## realiza remuestreo de orden de arboles
           .expid,
           ~collect_forest_predictions(rf_model, ikea_train)),
         .type = "training")
predictions_test <- tibble(.expid = 1:nexp) |>
  mutate(.performance = map(## realiza remuestreo de orden de arboles
           .expid,
           ~collect_forest_predictions(rf_model, ikea_test)),
         .type = "testing")

original_results <- predictions_train |>
  rbind(predictions_test) |>
  unnest(.performance) |> unnest(results) |>
  group_by(.type, .order) |>
  summarise(.error = mean(.estimate),
            .inf = quantile(.estimate, .05),
            .sup = quantile(.estimate, .95),
            .groups = "drop")

original_results |>
  ggplot(aes(.order, .error, fill = .type, group = .type, color = .type)) +
  geom_ribbon(aes(ymin = .inf, ymax = .sup), alpha = .2, color = "white") + 
  geom_line() + sin_lineas +  scale_x_log10()

### Quema de bosque ----------------------------------------------------------

predictions <- predict(rf_model, bake(ranger_prep, ikea_train), predict.all = TRUE)

trees_train  <- predictions$individual |>
  as_tibble() |>
  mutate(price = ikea_train$price)

predictions <- predict(rf_model, bake(ranger_prep, ikea_test), predict.all = TRUE)
trees_test  <- predictions$individual |>
  as_tibble() |>
  mutate(price = ikea_test$price)

trees_train <- trees_test

trees_train |> print(n = 3, width = 75)

lasso_spec <- linear_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet") |>
  set_mode("regression")

lasso_rec <- recipe(price ~ ., data = trees_train)

set.seed(108727)
forest_boot <- vfold_cv(trees_train, v = 10)

lasso_wf <- workflow() |>
  add_recipe(lasso_rec) |> 
  add_model(lasso_spec)

lasso_grid <- lasso_wf |>
  tune_grid(
    resamples = forest_boot,
    grid = 50,
    control = control_grid(verbose = FALSE)
  )

lasso_grid |>
  collect_metrics() |>
  print(n = 3, width = 75)

lowest_rmse <- lasso_grid |>
  select_best("rmse")

final_lasso <- finalize_workflow(
  lasso_wf,
  lowest_rmse
)

active_trees <- final_lasso |>
  fit(trees_train) |>
  broom::tidy() |>
  filter(estimate != 0) |>
  mutate(.tree = term, beta = estimate) |>
  select(c(.tree, beta)) 

active_trees |> print(n = 5)

collect_dforest_predictions <- function(model, data, active_trees){
  intercept <- active_trees$beta[1]
  predictions <- predict(model, bake(ranger_prep, data), predict.all = TRUE)
  predictions$individual |>
    as_tibble() |>
    mutate(observation = 1:nrow(data),
           truth = data$price) |>
    pivot_longer(cols = 1:1000,
                 values_to = ".prediction", names_to = ".tree") |>
    right_join(active_trees |> filter(.tree != "(Intercept)"), by = ".tree") |>
    filter(complete.cases(beta)) |>
    group_by(observation) |>
    sample_frac(1, replace = FALSE) |>
    mutate(.estimate = intercept + cumsum(beta * .prediction),
           .order = 1:n()) |>
    ungroup() |> select(c(-.prediction, -beta)) |>
    nest(data = c(observation, truth, .estimate, .tree)) |>
    mutate(results = map(data, function(x) { x |> rmse(truth, .estimate) })) |>
    select(-data)
}

nexp <- 100; set.seed(108)
dpredictions_train <- tibble(.expid = 1:nexp) |>
  mutate(.performance = map(## realiza remuestreo de orden de arboles
           .expid,
           ~collect_dforest_predictions(rf_model, ikea_train, active_trees)),
         .type = "dtraining")
dpredictions_test <- tibble(.expid = 1:nexp) |>
  mutate(.performance = map(## realiza remuestreo de orden de arboles
           .expid,
           ~collect_dforest_predictions(rf_model, ikea_test, active_trees)),
         .type = "dtesting")

deforest_results <- dpredictions_train |>
  rbind(dpredictions_test) |>
  unnest(.performance) |> unnest(results) |>
  group_by(.type, .order) |>
  summarise(.error = mean(.estimate),
            .inf = quantile(.estimate, .05),
            .sup = quantile(.estimate, .95),
            .groups = "drop")

deforest_results |>
  ggplot(aes(.order, .error, fill = .type, group = .type, color = .type)) +
  geom_ribbon(aes(ymin = .inf, ymax = .sup), alpha = .2, color = "white") + 
  geom_line() + sin_lineas +  scale_x_log10()

original_results |>
  rbind(deforest_results) |>
  ggplot(aes(.order, .error, fill = .type, group = .type, color = .type)) +
  geom_ribbon(aes(ymin = .inf, ymax = .sup), alpha = .2, color = "white") + 
  geom_line() + sin_lineas +  scale_x_log10() + scale_y_log10() 
  ## coord_cartesian(ylim = c(0.1, 1))
