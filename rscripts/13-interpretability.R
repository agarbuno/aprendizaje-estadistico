## Setup ---------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(scales)
library(tidymodels)

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

## Aplicacion: Precios de IKEA ---------------------------------------------
ikea <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv")

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

linear_recipe <-
  recipe(formula = price ~ ., data = ikea_train) |>
  step_other(name, category, threshold = 0.01) |>
  step_clean_levels(name, category) |>
  step_impute_knn(depth, height, width) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_predictors())

### Especificación modelo ----------------------------------------------------

linear_spec <-
  linear_reg(penalty = 1e-3) |>
  set_mode("regression") |>
  set_engine("glmnet")

linear_workflow <-
  workflow() |>
  add_recipe(linear_recipe) |>
  add_model(linear_spec)

ranger_spec <-
  rand_forest(trees = 1000) |>
  set_mode("regression") |>
  set_engine("ranger", importance = "impurity")

ranger_workflow <-
  workflow() |>
  add_recipe(ranger_recipe) |>
  add_model(ranger_spec)

all_cores <- parallel::detectCores(logical = TRUE) - 1
library(doParallel)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

ikea_lm <- linear_workflow |> fit(data = ikea_train)
ikea_rf <- ranger_workflow |> fit(data = ikea_train)

augment(ikea_lm, ikea_test) |>
  mutate(.linear = .pred,
         .ranger = predict(ikea_rf, ikea_test) |> pull(.pred)) |>
  select(c(price, 8:9)) |>
  pivot_longer(cols = 2:3, names_to = "model", values_to = "predictions") |>
ggplot(aes(price, predictions)) +
geom_point(alpha = .4) +
facet_wrap(~model) +
geom_abline(intercept = 0, slope = 1, lty = 2) +
sin_lineas +
coord_equal()

## (cambio a slides)

library(DALEXtra)

explainer_lm <- 
  explain_tidymodels(
    ikea_lm, 
    data = ikea_train |> select(-price), 
    y    = ikea_train |> pull(price),
    label = "linear model",
    verbose = FALSE
  )

explainer_rf <- 
  explain_tidymodels(
    ikea_rf, 
    data = ikea_train |> select(-price), 
    y    = ikea_train |> pull(price),
    label = "random forest",
    verbose = FALSE
  )

set.seed(123)
mueble <- ikea_test |> sample_n(1)
mueble

ikea_lm |> extract_fit_parsnip() |>
  tidy() |>
  print(n = 5)

lm_breakdown <- predict_parts(
  explainer = explainer_lm,
  new_observation = mueble
)
lm_breakdown

rf_breakdown <- predict_parts(
  explainer = explainer_rf,
  new_observation = mueble
)
rf_breakdown

rfor_breakdown <- predict_parts(
  explainer = explainer_rf,
  new_observation = mueble,
  order = lm_breakdown$variable_name
)
rfor_breakdown

g2 <- rf_breakdown |> plot() + sin_lineas
g1 <- lm_breakdown |> plot() + sin_lineas
g3 <- rfor_breakdown |> plot() + sin_lineas
g1 + g2 + g3

rfin_breakdown <- predict_parts(
  explainer = explainer_rf,
  new_observation = mueble,
  type = "break_down_interactions"
)
rfin_breakdown

set.seed(1801)
shap_mueble_rf <- 
  predict_parts(
    explainer = explainer_rf, 
    new_observation = mueble, 
    type = "shap",
    B = 20
  )

shap_mueble_rf |>
plot()

set.seed(1801)
shap_mueble_lm <- 
  predict_parts(
    explainer = explainer_lm, 
    new_observation = mueble, 
    type = "shap",
    B = 20
  )

g.lm <- shap_mueble_lm |>
  plot()
g.rf <- shap_mueble_rf |>
  plot()

g.lm + g.rf

xgb_spec <-
  boost_tree(trees = 1000) |>
  set_mode("regression") |>
  set_engine("xgboost")

xgb_workflow <-
  workflow() |>
  add_recipe(ranger_recipe |> step_dummy(all_nominal_predictors())) |>
  add_model(xgb_spec)

ikea_xgb <- xgb_workflow |> fit(data = ikea_train)

explainer_xgb <- 
  DALEX::explain(
    ikea_xgb, 
    data = ikea_train |> select(-price), 
    y    = ikea_train |> pull(price),
    label = "boosted trees",
    verbose = FALSE
  )

library(lime)
set.seed(108)
model_type.dalex_explainer <- DALEXtra::model_type.dalex_explainer
predict_model.dalex_explainer <- DALEXtra::predict_model.dalex_explainer

lime_mueble <- predict_surrogate(
  explainer = explainer_xgb,
  new_observation = mueble,
  n_features = 3,
  n_permutations = 500,
  type = "lime"
)

lime_mueble |> print(width = 85)

lime_mueble |> plot()

set.seed(1804)
vip_rf <- model_parts(explainer_rf, loss_function = loss_root_mean_square)
plot(vip_rf)

set.seed(1805)
pdp_width <- model_profile(explainer_rf, N = 500, variables = "width")

ggplot_pdp <- function(obj, x) {

  p <- 
    as_tibble(obj$agr_profiles) %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "^[^_]*_")) %>%
    ggplot(aes(`_x_`, `_yhat_`)) +
    geom_line(data = as_tibble(obj$cp_profiles),
              aes(x = {{ x }}, group = `_ids_`),
              linewidth = 0.5, alpha = 0.05, color = "gray50")

  num_colors <- n_distinct(obj$agr_profiles$`_label_`)

  if (num_colors > 1) {
    p <- p + geom_line(aes(color = `_label_`), linewidth = 1.2, alpha = 0.8)
  } else {
    p <- p + geom_line(color = "midnightblue", linewidth = 1.2, alpha = 0.8)
  }

  p
}

pdp_width |> ggplot_pdp(width) +
labs(x = "Width", 
     y = "Price", 
     color = NULL) + sin_lineas

set.seed(1806)
pdp_wcat <- model_profile(explainer_rf, N = 1000, 
                         variables = "width", 
                         groups = "category")

as_tibble(pdp_wcat$agr_profiles) %>%
  mutate(category = stringr::str_remove(`_label_`, "random forest_")) %>%
  ggplot(aes(`_x_`, `_yhat_`, color = category)) +
  geom_line(data = as_tibble(pdp_wcat$cp_profiles),
            aes(x = width, group = `_ids_`),
            linewidth = 0.5, alpha = 0.1, color = "gray50") +
  geom_line(linewidth = 1.2, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  facet_wrap(~category, ncol = 4) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "width", 
       y = "price", 
       color = NULL) + sin_lineas
