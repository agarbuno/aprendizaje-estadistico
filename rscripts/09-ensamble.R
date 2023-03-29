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

ranger_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) |>
  set_mode("regression") |>
  set_engine("ranger")

ranger_workflow <-
  workflow() |>
  add_recipe(ranger_recipe) |>
  add_model(ranger_spec)

set.seed(8577)
## Create a cluster object and then register: 
cl <- makePSOCKcluster(6)
doParallel::registerDoParallel(cl)

ranger_tune <-
  tune_grid(ranger_workflow,
            resamples = ikea_folds,
            grid = 11,
            control = control_grid(parallel_over = "resamples", verbose = TRUE)              
            )

show_best(ranger_tune, metric = "rmse")

show_best(ranger_tune, metric = "rsq")

final_rf <- ranger_workflow |>
  finalize_workflow(select_best(ranger_tune))

final_rf

ikea_fit <- last_fit(final_rf, ikea_split)
ikea_fit

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
