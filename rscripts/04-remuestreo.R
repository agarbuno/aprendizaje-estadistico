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

data |> head()

set.seed(108790)
sample_rows <- sample(1:nrow(data), nrow(data)/2)

data_train <- data[sample_rows,]
data_test <- data[-sample_rows,]

fit_model <- function(power, data){
  lm(mpg ~ poly(horsepower, power), data)
}

eval_error <- function(model, data){
  mean((data$mpg - predict(model, newdata = data))**2)
}

g1 <- tibble(degree = 1:10) |>
  mutate(model = map(degree, fit_model, data_train),
         error = map_dbl(model, eval_error, data_test)) |>
  ggplot(aes(degree, error)) + 
  geom_point() + geom_line() + ylim(16, 26)+ sin_lineas

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
  ylim(16,26) + sin_lineas

g1 + g2

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

data |> vfold_cv(5)
data |> loo_cv()

g.loo <- data |>
    rsample::loo_cv() |>
    mutate(results = map(splits, ajusta_modelo)) |>
    unnest(results) |>
    group_by(degree) |>
    summarise(error.loo = mean(error)) |>
    ggplot(aes(degree, error.loo)) +
    geom_line() + geom_point() +
    ggtitle("Leave-one out") +
    ylim(16, 26)+ sin_lineas

g.cv <- data |>
    vfold_cv(10, repeats = 10) |>
    mutate(results = map(splits, ajusta_modelo)) |>
    unnest(results) |>
    group_by(id, degree) |>
    summarise(error.cv = mean(error)) |>
    ggplot(aes(degree, error.cv, color = id)) +
    geom_line() + geom_point() + sin_leyenda +
    ggtitle("Validación cruzada K=10") +
    ylim(16, 26)+ sin_lineas

  g.loo + g.cv
