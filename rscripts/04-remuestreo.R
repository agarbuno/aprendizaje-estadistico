## Setup --------------------------------------------
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

## Separación entrenamiento - prueba ---------------------------
library(rsample)

data <- read.csv("https://www.statlearning.com/s/Auto.csv") |>
  as_tibble() |>
  mutate(horsepower = as.numeric(horsepower)) |>
  select(-name) |> 
  filter(!is.na(horsepower))

data |> print(n = 5)

set.seed(108790)
sample_rows <- sample(1:nrow(data), nrow(data)/2)

data_train <- data[sample_rows,]
data_test <- data[-sample_rows,]

fit_model <- function(power, data){
  lm(mpg ~ poly(horsepower, power), data)
}

eval_error <- function(model, data){
  (mean((data$mpg - predict(model, newdata = data))**2))
}

g1 <- tibble(degree = 1:10) |>
  mutate(model = map(degree, fit_model, data_train),
         error = map_dbl(model, eval_error, data_test)) |>
  ggplot(aes(degree, error)) + 
  geom_point() + geom_line() +  sin_lineas +
  coord_cartesian(ylim = c(16, 26) ) + 
  ggtitle("Error en test")

eval_resample <- function(id){
  ## Hace splits
  sample_rows <- sample(1:nrow(data), nrow(data)/2)
  data_train <- data[sample_rows,]
  data_test <- data[-sample_rows,]
  ## Entrena y evalua
  tibble(degree = 1:10) |>
    mutate(model = map(degree, fit_model, data_train),
           error = map_dbl(model, eval_error, data_test))
}

g2 <- tibble(id = factor(1:10)) |>
  mutate(resultados = map(id, eval_resample)) |>
  unnest(resultados) |>
  ggplot(aes(degree, error, color = id)) +
  geom_line() + geom_point() + sin_leyenda +
  sin_lineas +
    coord_cartesian(ylim = c(16, 26) ) + 
  ggtitle("Error en test \n(multiples particiones)")

g1 + g2

set.seed(1087)
data_split <- rsample::initial_split(data, prop = .5)
data_train <- training(data_split)
data_test <- testing(data_split)

## Validación cruzada -----------------------------------
ajusta_modelo <- function(split){
    ## Separa en entrenamiento / validacion
    train <-  analysis(split)
    valid <- assessment(split)
    ## Entrena y evalua
    tibble(degree = 1:10) |>
      mutate(model = map(degree, fit_model, train),
             error = map_dbl(model, eval_error, valid))
  }

data_train |> vfold_cv(5)
data_train |> loo_cv()

train.loo <- data_train |>
  rsample::loo_cv() |>
  mutate(results = map(splits, ajusta_modelo)) |>
  unnest(results) |>
  group_by(degree) |>
  summarise(error.loo = mean(error))

g.loo <- train.loo |>
  ggplot(aes(degree, error.loo)) +
  geom_line() + geom_point() +
  ggtitle("Leave-one out") +  sin_lineas +
  coord_cartesian(ylim = c(18, 27) ) 

train.kcv <- data_train |>
  vfold_cv(10, repeats = 10) |>
  mutate(results = map(splits, ajusta_modelo)) |>
  unnest(results) |>
  group_by(id, degree) |>
  summarise(error.cv = mean(error))

g.cv <- train.kcv |>
  ggplot(aes(degree, error.cv, color = id)) +
  geom_line() + geom_point() + sin_leyenda +
  ggtitle("Validación cruzada K=10") +  sin_lineas +
  coord_cartesian(ylim = c(18, 27) ) 

g.loo + g.cv

library(tidymodels)

evalua_ajuste <- function(modelo, datos){
  eval_metrics <- metric_set(rmse)
  predict(modelo, datos) |>
    as_tibble() |>
    bind_cols(datos |> select(mpg)) |>
    eval_metrics(mpg, value) |>
    mutate(mse = .estimate**2)
}

test.kcv <- data_train |>
  vfold_cv(K = 10) |>
  mutate(results = map(splits, ajusta_modelo)) |>
  unnest(results) |>
  mutate(tests = map(model, evalua_ajuste, data_test)) |>
  unnest(tests)

test.loo <- data_train |>
  loo_cv() |>
  mutate(results = map(splits, ajusta_modelo)) |>
  unnest(results) |>
  mutate(tests = map(model, evalua_ajuste, data_test)) |>
  unnest(tests)

train.kcv.summary <- train.kcv |>
  group_by(degree) |>
  summarise(train.error = mean(error.cv),
            inf.error = quantile(error.cv, 0.05),
            sup.error = quantile(error.cv, 0.95))

train.loo.summary <- train.loo |>
  mutate(train.error = error.loo,
         sup.error = train.error,
         inf.error = train.error) |>
  select(-error.loo)

g.kcv <-  test.kcv |>
  group_by(degree) |>
  summarise(train.error = mean(error),
            test.error  = mean(mse),
            inf.error = quantile(mse, 0.05),
            sup.error = quantile(mse, 0.95)) |>
  ggplot(aes(degree, test.error)) +
  geom_ribbon(aes(ymin = inf.error, ymax = sup.error), alpha = .3) + 
  geom_line() + geom_point() + 
  geom_line(data = train.kcv.summary, aes(degree, train.error), lty = 2, color = "salmon") +
  geom_ribbon(data = train.kcv.summary, aes(x = degree, y = train.error,
                                            ymin = inf.error, ymax = sup.error),
              alpha = .3, fill = "salmon") +
  ggtitle("Validación cruzada K=10") +
  coord_cartesian(ylim = c(18, 27) ) +
  sin_lineas

g.loo <- test.loo |>
  group_by(degree) |>
  summarise(train.error = mean(error),
            test.error  = mean(mse),
            inf.error = quantile(mse, 0.05),
            sup.error = quantile(mse, 0.95)) |>
  ggplot(aes(degree, test.error)) +
  geom_ribbon(aes(ymin = inf.error, ymax = sup.error), alpha = .3) + 
  geom_line() + geom_point() + 
  geom_line(data = train.loo.summary, aes(degree, train.error), lty = 2, color = "salmon") +
  ggtitle("Leave-one-out") +
  coord_cartesian(ylim = c(18, 27) ) +
  sin_lineas

g.loo + g.kcv
