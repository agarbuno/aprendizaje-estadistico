## Setup ---------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(scales)

## Cambia el default del tamaño de fuente 
theme_set(theme_linedraw(base_size = 25))

## Cambia el número de decimales para mostrar
options(digits = 4)
## Problemas con mi consola en Emacs
options(pillar.subtle = FALSE, pillar.width = 75)
options(rlang_backtrace_on_error = "none")
options(crayon.enabled = FALSE)

## Para el tema de ggplot
sin_lineas <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)

sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), axis.text = element_blank())

## Paquetes de arboles
library(tidymodels)
library(rpart.plot)
library(vip)

data(spam, package = "kernlab")
spam <- spam |> as_tibble() |> mutate(type = relevel(type, ref = "spam"))
spam |> print(n = 3, width = 75)

spam |>
  group_by(type) |>
  tally()

set.seed(108727)
spam_split <- initial_split(spam, strata = type)
spam_train <- training(spam_split)
spam_test <- testing(spam_split)

logistic_spec <- logistic_reg(penalty = .0003, mixture = 1) |>
  set_engine("glmnet")

logistic_recipe <- recipe(type ~ ., spam_train)

logistic_wf <- workflow() |>
  add_recipe(logistic_recipe) |>
  add_model(logistic_spec)

fit(logistic_wf, spam_train) |>
  augment(new_data = spam_test) |>
  conf_mat(type, .pred_class)

fit(logistic_wf, spam_train) |>
pull_workflow_fit() |>
  vi(lambda = 0.0003) |>
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) |> head(20) |> 
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL) + sin_lineas

tree_spec <- decision_tree(tree_depth = 2) |>
  set_engine("rpart")

class_tree_spec <- tree_spec |>
  set_mode("classification")

class_tree_fit <- class_tree_spec |>
  fit(type ~ ., data = spam_train)

class_tree_fit$fit |>
  rpart.plot(tweak = 2, gap = 0, shadow.col = "gray", branch.lty = 2)

class_tree_fit |>
  augment(new_data = spam_test) |>
  conf_mat(type, .pred_class)

library(ISLR2)
hitters <- as_tibble(Hitters) |>
  select(Hits, Years, Salary) |>
  filter(complete.cases(Salary))
hitters |> print(n = 5)

hitters |>
  ggplot(aes(Years, Hits)) +
  geom_point(aes(color = Salary), size = 4) +
  ## scale_color_gradient2(midpoint = 536, low = "blue", mid = "white", high = "red") +
  scale_color_viridis_c(option = "plasma") +
  sin_leyenda + sin_lineas

tree_spec <- decision_tree(tree_depth = 2) |>
  set_engine("rpart")

reg_tree_spec <- tree_spec |>
  set_mode("regression")

reg_tree_fit <- reg_tree_spec |>
  fit(Salary ~ ., data = hitters)

reg_tree_fit |>
  extract_fit_engine() |>
  rpart.rules(roundint = FALSE)

reg_tree_fit |>
  extract_fit_engine() |>
  rpart.plot(tweak = 2, gap = 0, shadow.col = "gray", branch.lty = 2)

hitters |>
  ggplot(aes(Years, Hits)) +
  geom_point(aes(color = Salary), size = 4) +
  scale_color_viridis_c(option = "plasma") +
  annotate("rect",
           xmin = -Inf, xmax = 4.5, ymin = -Inf, ymax = Inf,
           alpha = 0, color = "darkred", lty = 2) +
  annotate("rect",
           xmin = 4.5, xmax = Inf, ymin = 118, ymax = Inf,
           alpha = 0, color = "darkred", lty = 2) + 
  sin_leyenda + sin_lineas

tree_spec <- decision_tree(cost_complexity = 1e-6) |>
  set_engine("rpart")

reg_tree_spec <- tree_spec |>
  set_mode("regression")

reg_tree_fit <- reg_tree_spec |>
  fit(Salary ~ ., data = hitters)

reg_tree_fit |>
  extract_fit_engine() |>
  rpart.plot(tweak = 1.2, gap = 0, shadow.col = "gray", branch.lty = 2)

tree_spec <- decision_tree(cost_complexity = 1e-3) |>
  set_engine("rpart")

reg_tree_spec <- tree_spec |>
  set_mode("regression")

reg_tree_fit <- reg_tree_spec |>
  fit(Salary ~ ., data = hitters)

reg_tree_fit |>
  extract_fit_engine() |>
  rpart.plot(tweak = 1.2, gap = 0, shadow.col = "gray", branch.lty = 2)

tree_spec <- decision_tree(cost_complexity = 1e-2) |>
  set_engine("rpart")

reg_tree_spec <- tree_spec |>
  set_mode("regression")

reg_tree_fit <- reg_tree_spec |>
  fit(Salary ~ ., data = hitters)

reg_tree_fit |>
  extract_fit_engine() |>
  rpart.plot(tweak = 1.2, gap = 0, shadow.col = "gray", branch.lty = 2)

tree_spec <- decision_tree(cost_complexity = 1.5e-2) |>
  set_engine("rpart")

reg_tree_spec <- tree_spec |>
  set_mode("regression")

reg_tree_fit <- reg_tree_spec |>
  fit(Salary ~ ., data = hitters)

reg_tree_fit |>
  extract_fit_engine() |>
  rpart.plot(tweak = 1.2, gap = 0, shadow.col = "gray", branch.lty = 2)

## Turbinas de viento ------------------------------------
 turbines <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv")

turbines_df <- turbines |>
 transmute(
   turbine_capacity = turbine_rated_capacity_k_w,
   rotor_diameter_m,
   hub_height_m,
   commissioning_date = parse_number(commissioning_date),
   province_territory = fct_lump_n(province_territory, 10),
   model = fct_lump_n(model, 10)
 ) |>
 filter(!is.na(turbine_capacity)) |>
 mutate_if(is.character, factor)

turbines_df |>
  select(turbine_capacity:commissioning_date) |>
  pivot_longer(rotor_diameter_m:commissioning_date) |>
  ggplot(aes(turbine_capacity, value)) +
  geom_hex(bins = 15, alpha = 0.8) +
  geom_smooth(method = "lm") +
  facet_wrap(~name, scales = "free_y") +
  labs(y = NULL) +
  scale_fill_gradient(high = "cyan3") + sin_lineas

set.seed(123)
wind_split <- initial_split(turbines_df, strata = turbine_capacity, prop = .5)
wind_train <- training(wind_split)
wind_test <- testing(wind_split)

wind_folds <- vfold_cv(wind_train, strata = turbine_capacity)

tree_spec <- decision_tree(
  cost_complexity = tune(),
) |>
  set_engine("rpart") |>
  set_mode("regression")

tree_spec

tree_grid <- grid_regular(cost_complexity(), levels = 10)
tree_grid

doParallel::registerDoParallel()
set.seed(345)
tree_rs <- tune_grid(
  tree_spec,
  turbine_capacity ~ .,
  resamples = wind_folds,
  grid = tree_grid,
  metrics = metric_set(rmse)
)
tree_rs

autoplot(tree_rs) + sin_lineas

final_tree <- finalize_model(tree_spec, select_best(tree_rs, "rmse"))
final_tree

final_fit <- fit(final_tree, turbine_capacity ~ ., wind_train)
final_rs <- last_fit(final_tree, turbine_capacity ~ ., wind_split)

library(parttree)

ex_fit <- fit(
  final_tree,
  turbine_capacity ~ rotor_diameter_m + commissioning_date,
  wind_train
)

wind_train |>
  ggplot(aes(rotor_diameter_m, commissioning_date)) +
  geom_parttree(data = ex_fit, aes(fill = turbine_capacity), alpha = 0.3) +
  geom_jitter(alpha = 0.7, width = 1, height = 0.5, aes(color = turbine_capacity)) +
  scale_colour_viridis_c(aesthetics = c("color", "fill")) + sin_lineas

final_fit |>
  extract_fit_engine() |>
  rpart.plot(tweak = 2, gap = 0, shadow.col = "gray", branch.lty = 2)

tibble(prob.pred = seq(0.001, 1, length.out = 1000)) |>
  mutate(gini = 2 * prob.pred * (1 - prob.pred),
         entropy = -prob.pred * log(prob.pred) - (1-prob.pred) * log(1 - prob.pred),
         error.rate = ifelse(prob.pred <= .5, prob.pred, 1 - prob.pred)) |>
 pivot_longer(2:4, names_to = ".metric", values_to = "impurity") |>
 ggplot(aes(prob.pred, impurity, group = .metric)) +
  geom_line(aes(colour = .metric), linewidth = 2) + sin_lineas

## Clasificacion: Scooby doo -------------------------
scooby_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv", progress = FALSE, show_col_types = FALSE)

scooby_raw |>
  filter(monster_amount > 0) |>
  count(monster_real)

set.seed(123)
scooby_split <- scooby_raw |>
  mutate(
    imdb = parse_number(imdb),
    year_aired = lubridate::year(date_aired)
  ) |>
  filter(monster_amount > 0, !is.na(imdb)) |>
  mutate(
    monster_real = case_when(
      monster_real == "FALSE" ~ "fake",
      TRUE ~ "real"
    ),
    monster_real = factor(monster_real)
  ) |>
  select(year_aired, imdb, monster_real, title) |>
  initial_split(strata = monster_real)
scooby_train <- training(scooby_split)
scooby_test <- testing(scooby_split)

set.seed(234)
scooby_folds <- vfold_cv(scooby_train, strata = monster_real)

tree_spec <-
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune(),
    min_n = tune()
  ) |>
  set_mode("classification") |>
  set_engine("rpart")

tree_grid <- grid_regular(cost_complexity(), tree_depth(), min_n(), levels = 4)
tree_grid |> print(n = 5)

set.seed(345)
tree_rs <-
  tune_grid(
    tree_spec,
    monster_real ~ year_aired + imdb,
    resamples = scooby_folds,
    grid = tree_grid,
    metrics = metric_set(accuracy, recall, precision, roc_auc)
  )

autoplot(tree_rs) + sin_lineas

simpler_tree <- select_by_one_std_err(tree_rs,
                                      -cost_complexity,
                                      metric = "roc_auc"
                                      )
final_tree <- finalize_model(tree_spec, simpler_tree)
final_fit <- fit(final_tree, monster_real ~ year_aired + imdb, scooby_train)

scooby_train |>
  ggplot(aes(imdb, year_aired)) +
  geom_parttree(data = final_fit, aes(fill = monster_real), alpha = 0.2) +
  geom_jitter(alpha = 0.7, width = 0.05, height = 0.2, aes(color = monster_real))  + sin_lineas

final_fit |>
  extract_fit_engine() |>
  rpart.plot(tweak = 1.5, gap = 0, shadow.col = "gray", branch.lty = 2)
