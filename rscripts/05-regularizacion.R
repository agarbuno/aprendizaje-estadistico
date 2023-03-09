## Setup --------------------------------------------
library(tidyverse)
library(tidymodels)
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

## Seleccion iterativa -------------------------------------
library(ISLR)
library(rsample)
data <- as_tibble(Credit) |>
  select(-ID, -Ethnicity) |>
  mutate(Gender = factor(ifelse(Gender == "Female", "Female", "Male"),
                         levels = c("Male", "Female")))

data |> print(n = 5)

tibble( estrategia = c("subconjunto", "adelante"),
       modelo = list(lm(Balance ~ Cards + Income + Student + Limit, data),
                     lm(Balance ~ Rating + Income + Student + Limit, data))) |>
  mutate(resumen = map(modelo, broom::glance)) |>
  unnest(resumen) |>
  select(estrategia, sigma, r.squared, adj.r.squared, AIC, deviance)

ajusta_adelante <- function(split){
  ## Separa en entrenamiento / validacion
  train <- analysis(split)
  valid <- assessment(split)
  ## Entrena y evalua
  modelo.nulo     <- lm(Balance ~ 1, train)
  modelo.completo <- lm(Balance ~ ., train)
  adelante <- step(modelo.nulo, direction='forward', scope=formula(modelo.completo), trace=0)
  predictores <- attributes(adelante$terms)$term.labels
  ## Itero sobre los predictores
  tibble(predictors = 1:length(predictores)) |>
    mutate(model = map(predictors, function(k){
      ## Filtro predictores (1:k) para entrenar y ajusto modelo
      train.d <- train |> select(predictores[1:k], Balance)
      model.d <- lm(Balance ~ ., train.d)
      model.d
    }), error = map_dbl(model, function(m){
      ## Uso modelo entrenado para evaluar error de prueba
      residuales <- predict(m, newdata = valid) - valid$Balance
      mean(residuales**2)
    })
    )
}

set.seed(108727)
data |>
  ## Separo en bloques y realizo procedimiento en cada bloque
  vfold_cv(10, strat = Student) |>
  mutate(results = map(splits, ajusta_adelante)) |>
  unnest(results) |>
  ## Tengo resultados para cada eleccion de k en cada bloque
  group_by(predictors) |>
  summarise(cv.error = mean(error),
            se.error = sd(error)) |>
  ## grafico 
  ggplot(aes(predictors, cv.error)) +
  geom_line(color = 'gray') + 
  geom_linerange(aes(ymin = cv.error - se.error,
                     ymax = cv.error + se.error), size = 2) +
  geom_linerange(aes(ymin = cv.error - 2 * se.error,
                     ymax = cv.error + 2 * se.error)) +
  geom_point(color = 'red', size = 4) + sin_lineas +
  xlab("Numero de predictores") +
  ylab("Error Validación Cruzada")

## Ridge -------------------------------------
library(glmnet)
library(recipes)

atributos <- model.matrix(Balance ~ . - 1, data)
respuesta <- data |> pull(Balance)

separa_procesa <- function(split){
  ## Separa datos
  train <- analysis(split)
  valid <- assessment(split)
  ## Preparo el objetivo del modelo 
  rec <- recipe(respuesta ~ .,  data = train)
  ## Defino procesamiento de datos
  estandarizador <- rec |>
    step_normalize(Income, Limit, Rating, Cards, Age, Education, respuesta)
  ## Calculo medias y desviaciones estandar en entrenamiento
  estandarizador.ajustado <- prep(estandarizador, train)
  ## Normalizo ambos conjuntos
  valid.std <- bake(estandarizador.ajustado, valid)
  train.std <- bake(estandarizador.ajustado, train)
  list(train = train.std, valid = valid.std)
}

ajusta_ridge <- function(split){
  ## Separo en entrenamiento / validacion
  split <- separa_procesa(split)
  ## Extraigo atributos / respuesta 
  xtrain <- split$train |> select(-respuesta) |> as.matrix()
  ytrain <- split$train |> pull(respuesta) |> as.matrix()
  xvalid <- split$valid |> select(-respuesta) |> as.matrix()
  yvalid <- split$valid |> pull(respuesta) |> as.matrix()

  ## Ajusta modelos para trayectoria de lambda
  tibble(lambda = 10**seq(-4, 2, length.out = 50)) |>
    mutate(modelo = map(lambda, function(lam){
      ## Ajusto modelo con lambda fija
      glmnet(y = ytrain, x = xtrain, lambda = lam, alpha = 0)
    }), residuales = map(modelo, function(mod){
      ## Calculo residuales 
      predict(mod, newx = xvalid) - yvalid
    }))
}

cv.results <- cbind(atributos, respuesta) |>
  as_tibble() |>
  vfold_cv(10) |>
  mutate(results = map(splits, ajusta_ridge))

## Muestro la trayectoria para una bloque 
g1 <- cv.results |>
  filter(id == "Fold01") |>
  unnest(results) |>
  mutate(estimates = map(modelo, tidy)) |>
  select(-lambda) |>
  unnest(estimates) |>
  filter(term != "(Intercept)") |>
  complete(term, lambda, fill = list(estimate = 0)) |>
  ggplot(aes(lambda, estimate, color= term)) + sin_lineas +
  geom_line() + scale_x_log10() + xlab(expression(lambda))
g1

## Cuantifico el error de validacion
g2 <- cv.results |>
  unnest(results) |>
  mutate(valid.error = map_dbl(residuales, function(x){mean(x**2)})) |>
  group_by(lambda) |>
  summarise(cv.error = mean(valid.error),
            se.error = sd(valid.error)) |> 
  ggplot(aes(1/lambda, cv.error)) +
  geom_line(color = 'gray') + 
  geom_linerange(aes(ymin = cv.error - se.error,
                     ymax = cv.error + se.error), size = 2) +
  geom_linerange(aes(ymin = cv.error - 2 * se.error,
                     ymax = cv.error + 2 * se.error)) +
  geom_point(color = 'red', size = 2) + sin_lineas + 
  xlab(paste(expression(1/lambda), " (Complejidad)")) +
  ylab("Error Validación Cruzada") +
  scale_x_log10()
g2

## Implementación tidymodels -------------------------------------------------

data_split <- initial_split(data)
data_train <- training(data_split)
data_test <- testing(data_split)

ridge_spec <- linear_reg(penalty = tune(), mixture = 0) |>
    set_engine(engine = "glmnet")

ridge_spec

## Defino el pre-procesamiento
ridge_recipe <- recipe(Balance ~ .,  data = data_train) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_predictors()) 

  ridge_recipe

ridge_workflow <- workflow() |>
  add_recipe(ridge_recipe) |> 
  add_model(ridge_spec)

ridge_workflow

data_folds <- vfold_cv(data_train, v = 10)
data_folds |> print(n = 5)

penalty_grid <- grid_regular(penalty(range = c(-3, 5)), levels = 50)
penalty_grid |> print(n = 5)

tune_res <- tune_grid(
  ridge_workflow,
  resamples = data_folds,
  metrics = metric_set(rmse),
  grid = penalty_grid
)
tune_res |> print(n = 5)

tune_res |> unnest(.metrics) |> print(n = 5)

collect_metrics(tune_res) |> print(n = 5)

autoplot(tune_res) + sin_lineas

best_penalty <- select_best(tune_res, metric = "rmse")
best_penalty

ridge_final <- finalize_workflow(ridge_workflow, best_penalty)
ridge_final_fit <- fit(ridge_final, data = data_train)

augment(ridge_final_fit, new_data = data_test) |>
  rmse(truth = Balance, estimate = .pred)

## Aplicación: The Office ----------------------------------------------------

ratings_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv", show_col_types = FALSE)

remove_regex <- "[:punct:]|[:digit:]|parts |part |the |and"

office_ratings <- ratings_raw |>
  transmute(
    episode_name = str_to_lower(title),
    episode_name = str_remove_all(episode_name, remove_regex),
    episode_name = str_trim(episode_name),
    imdb_rating
  )

office_info <- schrute::theoffice |>
  mutate(
    season = as.numeric(season),
    episode = as.numeric(episode),
    episode_name = str_to_lower(episode_name),
    episode_name = str_remove_all(episode_name, remove_regex),
    episode_name = str_trim(episode_name)
  ) |>
  select(season, episode, episode_name, director, writer, character)

characters <- office_info %>%
  count(episode_name, character) %>%
  add_count(character, wt = n, name = "character_count") %>%
  filter(character_count > 800) %>%
  select(-character_count) %>%
  pivot_wider(
    names_from = character,
    values_from = n,
    values_fill = list(n = 0)
  )

characters |> print(n = 5)

creators <- office_info %>%
  distinct(episode_name, director, writer) %>%
  pivot_longer(director:writer, names_to = "role", values_to = "person") %>%
  separate_rows(person, sep = ";") %>%
  add_count(person) %>%
  filter(n > 10) %>%
  distinct(episode_name, person) %>%
  mutate(person_value = 1) %>%
  pivot_wider(
    names_from = person,
    values_from = person_value,
    values_fill = list(person_value = 0)
  )

creators |> print(n = 5)

office <- office_info %>%
  distinct(season, episode, episode_name) %>%
  inner_join(characters) %>%
  inner_join(creators) %>%
  inner_join(office_ratings %>%
             select(episode_name, imdb_rating)) %>%
  janitor::clean_names()

office |> print(n = 5)

office %>%
  ggplot(aes(episode, imdb_rating, fill = as.factor(episode))) +
  geom_boxplot(show.legend = FALSE) + sin_lineas

office_split <- initial_split(office, strata = season)
office_train <- training(office_split)
office_test <- testing(office_split)

## Con mixture = 1, pedimos lasso
tune_spec <- linear_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet") |>
  set_mode("regression")

office_rec <- recipe(imdb_rating ~ ., data = office_train) |>
  update_role(episode_name, new_role = "ID") |>
  step_zv(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors())

office_prep <- office_rec |>
  prep(strings_as_factors = FALSE)

set.seed(108727)
office_boot <- vfold_cv(office_train, v = 10, strata = season)

## All operating systems
library(doParallel)

## Create a cluster object and then register: 
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

set.seed(2020)
wf <- workflow() |>
  add_recipe(office_rec) |>
  add_model(tune_spec)

lasso_grid <- wf |>
  tune_grid(
    resamples = office_boot,
    grid = lambda_grid,
    control = control_grid(verbose = FALSE)
  )

lasso_grid |>
  collect_metrics()

lasso_grid %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_linerange(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) + geom_point() + 
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none") + sin_lineas

lowest_rmse <- lasso_grid |>
  select_best("rmse")

final_lasso <- finalize_workflow(
  wf |> add_model(tune_spec),
  lowest_rmse
)

library(vip)

final_lasso %>%
  fit(office_train) %>%
  pull_workflow_fit() %>%
  vi(lambda = lowest_rmse$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL) + sin_lineas

final_lasso |>
  fit(office_train) |>
  augment(new_data = office_test) |>
  rmse(truth = imdb_rating, estimate = .pred)

tune_spec <- linear_reg(penalty = tune(), mixture = tune()) |> 
  set_engine("glmnet") |>
  set_mode("regression")

hyper_params <- parameters(penalty(range = c(-5, 3)), mixture())
hparams_grid <- grid_regular(hyper_params, levels = 10)

set.seed(2020)
wf <- workflow() |>
  add_recipe(office_rec) |>
  add_model(tune_spec)

lasso_grid <- wf |>
  tune_grid(
    resamples = office_boot,
    grid = hparams_grid,
    control = control_grid(verbose = FALSE)
  )

grid_max_entropy(hyper.params, size = 50)
grid_random(hyper.params, size = 50)
