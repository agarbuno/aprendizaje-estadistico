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

library(ISLR)
set.seed(108727)
## Cargamos datos
data <- tibble(Wage) |> select(year, age, wage, education) |>
  mutate(hi.income = ifelse(wage > 250, 1, 0),
         age = as.numeric(age)) |>
  sample_frac(.05)

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

sf_trees <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv", show_col_types = FALSE, progress = FALSE)

trees_df <- sf_trees |>
  mutate(
    legal_status = case_when(
      legal_status == "DPW Maintained" ~ legal_status,
      TRUE ~ "Other"
    ),
    plot_size = parse_number(plot_size)
  ) |>
  select(-address)|>
  na.omit() |>
  mutate_if(is.character, factor)

trees_df |>
  ggplot(aes(longitude, latitude, color = legal_status)) +
  geom_point(size = 0.5, alpha = 0.4) +
  labs(color = NULL) +
sin_lineas + coord_equal()

trees_df |>
  count(legal_status, caretaker) |>
  add_count(caretaker, wt = n, name = "caretaker_count") |>
  filter(caretaker_count > 50) |>
  group_by(legal_status) |>
  mutate(percent_legal = n / sum(n)) |>
  ggplot(aes(percent_legal, caretaker, fill = legal_status)) +
  geom_col(position = "dodge") +
  labs(
    fill = NULL,
    x = "% of trees in each category"
  ) + sin_lineas

set.seed(108727)
trees_split <- initial_split(trees_df, strata = legal_status, prop = 1/8)
trees_train <- training(trees_split)
trees_test <- testing(trees_split)

tree_rec <- recipe(legal_status ~ ., data = trees_train) |>
  update_role(tree_id, new_role = "ID") |>
  step_other(species, caretaker, threshold = 0.01) |>
  step_other(site_info, threshold = 0.005) |>
  step_dummy(all_nominal(), -all_outcomes()) |>
  step_date(date, features = c("year")) |>
  step_rm(date)
  ## step_downsample(legal_status)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) |>
set_mode("classification") |>
set_engine("ranger", importance = "permutation")

tune_wf <- workflow() |>
  add_recipe(tree_rec) |>
  add_model(tune_spec)

set.seed(108727)
trees_folds <- vfold_cv(trees_train, 5)

tree_grid <- grid_random(mtry(c(1,30)),
                         min_n(),
                         size = 20)
tree_grid |> print(n = 5)

doParallel::registerDoParallel()

set.seed(108727)
tune_res <- tune_grid(
  tune_wf,
  grid = tree_grid, 
  resamples = trees_folds,
  control = control_grid(parallel_over = "resamples")
)

tune_res |>
  collect_metrics() |>
  filter(.metric == "roc_auc") |>
  select(mean, min_n, mtry) |>
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
               ) |>
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC") + sin_lineas

rf_grid <- grid_regular(
  mtry(range = c(10, 30)),
  min_n(range = c(2, 8)),
  levels = 5
)

set.seed(108727)
regular_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = rf_grid,
  control = control_grid(parallel_over = "resamples")
)

regular_res |>
  collect_metrics() |>
  filter(.metric == "roc_auc") |>
  mutate(min_n = factor(min_n)) |>
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, linewidth = 1.5) +
  geom_point() +
  labs(y = "AUC") + sin_lineas

best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf

final_wf <- workflow() |>
  add_recipe(tree_rec) |>
  add_model(final_rf)

final_res <- final_wf |>
  last_fit(trees_split)

final_res |>
  collect_metrics()

library(vip)

final_res |>
  extract_fit_engine() |>
  vip() + sin_lineas

final_res |>
  collect_predictions() |>
  conf_mat(legal_status, .pred_class)

final_res |>
  collect_predictions() |>
  recall(legal_status, .pred_class)
