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

## Aplicación: Volleyball de playa -------------------------------------------

library(tidymodels)
vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', progress = FALSE, show_col_types = FALSE)
vb_matches |> print(n = 3, width = 75)

vb_parsed <- vb_matches |>
  transmute(circuit, gender, year,
            w_attacks = w_p1_tot_attacks + w_p2_tot_attacks,
            w_kills = w_p1_tot_kills + w_p2_tot_kills,
            w_errors = w_p1_tot_errors + w_p2_tot_errors,
            w_aces = w_p1_tot_aces + w_p2_tot_aces,
            w_serve_errors = w_p1_tot_serve_errors + w_p2_tot_serve_errors,
            w_blocks = w_p1_tot_blocks + w_p2_tot_blocks,
            w_digs = w_p1_tot_digs + w_p2_tot_digs,
            l_attacks = l_p1_tot_attacks + l_p2_tot_attacks,
            l_kills = l_p1_tot_kills + l_p2_tot_kills,
            l_errors = l_p1_tot_errors + l_p2_tot_errors,
            l_aces = l_p1_tot_aces + l_p2_tot_aces,
            l_serve_errors = l_p1_tot_serve_errors + l_p2_tot_serve_errors,
            l_blocks = l_p1_tot_blocks + l_p2_tot_blocks,
            l_digs = l_p1_tot_digs + l_p2_tot_digs
            ) |>
  na.omit()
vb_parsed |> print(n = 3, width = 75)

winners <- vb_parsed |>
  select(circuit, gender, year,
         w_attacks:w_digs) |>
  rename_with(~ str_remove_all(., "w_"), w_attacks:w_digs) |>
  mutate(win = "win")

losers <- vb_parsed |>
  select(circuit, gender, year,
         l_attacks:l_digs) |>
  rename_with(~ str_remove_all(., "l_"), l_attacks:l_digs) |>
  mutate(win = "lose")

vb_df <- bind_rows(winners, losers) |>
  mutate_if(is.character, factor) |>
  mutate(win = factor(win, levels = c("win", "lose")))

vb_df |>
  pivot_longer(attacks:digs, names_to = "stat", values_to = "value") |>
  ggplot(aes(gender, value, fill = win, color = win)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~stat, scales = "free_y", nrow = 2) +
  labs(y = NULL, color = NULL, fill = NULL) + sin_lineas

set.seed(123)
vb_split <- initial_split(vb_df, strata = win)
vb_train <- training(vb_split)
vb_test <- testing(vb_split)

xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(), ## complexity
  mtry = tune(),         ## randomness
  learn_rate = tune()    ## step size
) |>
  set_engine("xgboost") |>
  set_mode("classification")

xgb_spec

xgb_rec <- recipe(win ~ ., vb_train) |>
  step_dummy(all_nominal_predictors())

xgb_rec

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  finalize(mtry(), vb_train),
  learn_rate(),
  size = 30
)

xgb_grid |> print(n = 3)

xgb_wf <- workflow() |>
  add_recipe(xgb_rec) |>
  add_model(xgb_spec)

xgb_wf

set.seed(123)
vb_folds <- vfold_cv(vb_train, strata = win)
vb_folds |> print(n = 5)

all_cores <- parallel::detectCores(logical = TRUE) - 1
library(doParallel)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

set.seed(234)
system.time(
xgb_res <- tune_grid(
  xgb_wf,
  resamples = vb_folds,
  grid = xgb_grid,
  control = control_grid(
    save_pred = TRUE, parallel = "everything")
))

set.seed(234)
system.time(
xgb_res <- tune_grid(
  xgb_wf,
  resamples = vb_folds,
  grid = xgb_grid,
  control = control_grid(
    save_pred = TRUE, parallel = "resamples")
))

xgb_res |> print(n = 3, width = 70)

collect_metrics(xgb_res) |> print(n = 5, width = 70)

xgb_res |>
  collect_metrics() |>
  filter(.metric == "roc_auc") |>
  select(mean, mtry:learn_rate) |>
  pivot_longer(mtry:learn_rate,
               values_to = "value",
               names_to = "parameter"
               ) |>
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC") + sin_lineas

show_best(xgb_res, "roc_auc") |>
  print(width = 70)

best_auc <- select_best(xgb_res, "roc_auc")
best_auc

final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc
)
final_xgb

final_res <- last_fit(final_xgb, vb_split)
collect_metrics(final_res)

final_res |>
  collect_predictions() |>
  roc_curve(win, .pred_win) |>
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "midnightblue") +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  ) + sin_lineas +
  coord_equal()

extract_boosted_prediction <- function(dt){
  final_res |>
    extract_fit_parsnip() |>
    multi_predict(new_data = prep(xgb_rec) |> bake(dt) |> select(-win),
                  trees = seq(1,1000, by = 5)) |>
    mutate(truth = dt$win,
           id = 1:n()) |>
    unnest(.pred) |>
    group_by(trees) |>
    nest(data = c(.pred_class, truth, id)) |>
    mutate(results = map(data, function(x) {
      accuracy(x, .pred_class, truth)})) |>
    ungroup()
}

preds_train <- extract_boosted_prediction(vb_train) |> mutate(type = "train")
preds_test <- extract_boosted_prediction(vb_test) |> mutate(type = "test")

preds_train |> select(-data) |>
  unnest(results) |>
  rbind(preds_test |> select(-data) |>
        unnest(results)) |>
  mutate(.estimate = 1 - .estimate) |>
  ggplot(aes(trees, .estimate, group = type, color = type)) +
  geom_line() + sin_lineas +
  ylab("error rate")

extract_boosted_roc <- function(dt){
  final_res |>
    extract_fit_parsnip() |>
    multi_predict(new_data = prep(xgb_rec) |> bake(dt) |> select(-win),
                  trees = seq(1,1000, by = 1),
                  type  = "prob") |>
    mutate(truth = dt$win,
           id = 1:n()) |>
    unnest(.pred) |>
    group_by(trees) |>
    nest(data = c(.pred_lose, .pred_win, truth, id)) |>
    mutate(results = map(data, function(x) {
      roc_curve(x, truth, .pred_win)})) |>
    ungroup()
}

roc_test <- extract_boosted_roc(vb_test) |> mutate(type = "test")

roc_test |>
  unnest(results) |>
  ggplot(aes( 1 - specificity, sensitivity, group = trees, color = trees)) +
  geom_line() + sin_lineas

library(finetune)
system.time(
  xgb_res <- tune_race_anova(
    xgb_wf,
    resamples = vb_folds,
    grid = xgb_grid,
    control = control_race(
      alpha = .05,
      verbose_elim = TRUE, 
      save_pred = TRUE,
      parallel = "everything"))
)

plot_race(xgb_res) + sin_lineas

extract_race_step <- function(xgb_race, ind = 3, alpha = 0.05){
  xgb_race |>
    select(id, .order, .metrics) %>%
    unnest(cols = .metrics) %>%
    filter(.metric == "roc_auc") |>
    filter(.order <= ind) |>
    mutate(model_id = str_remove_all(.config, "Preprocessor1_Model")) |>
    group_by(model_id) %>%
    summarize(
      mean = mean(.estimate, na.rm = TRUE),
      err  = sd(.estimate, na.rm = TRUE),
      n = sum(!is.na(.estimate)),
      .groups = "drop"
    ) %>%
    mutate(stage = ind) %>%
    ungroup() %>%
    filter(n == ind) |>
    arrange(desc(mean)) |>
    mutate(.inf = mean - qnorm(1-alpha/2) * err,
           .sup = mean + qnorm(1-alpha/2) * err,
           .order = model_id) 
}

race_initial <- extract_race_step(xgb_res)
race_initial |>
  ggplot(aes(.order, mean)) +
  geom_linerange(aes(ymin = .inf, max = .sup)) +
  geom_point() + sin_lineas +
  xlab("Configuración de modelo") +
  ylab("Métrica de desempeño") + 
  coord_cartesian(xlim = c(1, 30), ylim = c(.65, .95))

extract_race_step(xgb_res, ind = 4) %>%
  ggplot(aes(.order, mean)) +
  geom_point(data = race_initial, aes(.order, mean), color = "lightgray") + 
  geom_linerange(data = race_initial, aes(ymin = .inf, max = .sup), color = "lightgray") +
  geom_linerange(aes(ymin = .inf, max = .sup)) +
  geom_point() + sin_lineas +
  xlab("Configuración de modelo") +
  ylab("Métrica de desempeño") + 
  coord_cartesian(xlim = c(1, 30), ylim = c(.65, .95))

extract_race_step(xgb_res, ind = 5) |>
  ggplot(aes(.order, mean)) +
  geom_point(data = race_initial, aes(.order, mean), color = "lightgray") + 
  geom_linerange(data = race_initial, aes(ymin = .inf, max = .sup), color = "lightgray") +
  geom_linerange(aes(ymin = .inf, max = .sup)) +
  geom_point() + sin_lineas +
  xlab("Configuración de modelo") +
  ylab("Métrica de desempeño") + 
  coord_cartesian(xlim = c(1, 30), ylim = c(.65, .95))

extract_race_step(xgb_res, ind = 10) |>
  ggplot(aes(.order, mean)) +
  geom_point(data = race_initial, aes(.order, mean), color = "lightgray") + 
  geom_linerange(data = race_initial, aes(ymin = .inf, max = .sup), color = "lightgray") +
  geom_linerange(aes(ymin = .inf, max = .sup)) +
  geom_point() + sin_lineas +
  xlab("Configuración de modelo") +
  ylab("Métrica de desempeño") + 
  coord_cartesian(xlim = c(1, 30), ylim = c(.65, .95))
