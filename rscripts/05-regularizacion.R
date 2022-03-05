## Setup --------------------------------------------
library(tidyverse)
library(patchwork)
library(scales)
## Cambia el default del tamaño de fuente 
theme_set(theme_linedraw(base_size = 25))

## Cambia el número de decimales para mostrar
options(digits = 2)

sin_lineas <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)

sin_lineas <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), axis.text = element_blank())

## Seleccion iterativa -------------------------------------
library(ISLR)
library(rsample)
data <- as_tibble(Credit) |>
  select(-ID, -Ethnicity) |>
  mutate(Gender = factor(ifelse(Gender == "Female", "Female", "Male"),
                         levels = c("Male", "Female")))

data |> sample_n(5) |> as.data.frame()

tibble( estrategia = c("subconjunto", "adelante"),
       modelo = list(lm(Balance ~ Cards + Income + Student + Limit, data),
                     lm(Balance ~ Rating + Income + Student + Limit, data))) |>
  mutate(resumen = map(modelo, broom::glance)) |>
  unnest(resumen) |>
  select(estrategia, sigma, r.squared, adj.r.squared, AIC, deviance) |>
  as.data.frame()

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
  xlab(expression(1/lambda)) +
  ylab("Error Validación Cruzada") +
  scale_x_log10()
g2

## LASSO -------------------------------------
library(glmnet)
library(recipes)

atributos <- model.matrix(Balance ~ . - 1, data)
respuesta <- data |> pull(Balance)

separa_procesa <- function(split){
  ## Separa datos
  train <- analysis(split)
  valid <- assessment(split)
  ## Preparo objetivo 
  rec <- recipe(respuesta ~ .,  data = train)
  ## Defino procesamiento de datos
  estandarizador <- rec |>
    step_normalize(Income, Limit, Rating, Cards, Age, Education, respuesta)
  ## Calculo medias y desviaciones estandar en entrenamiento
  estandarizador.ajustado <- prep(estandarizador, train)
  ##
  valid.std <- bake(estandarizador.ajustado, valid)
  train.std <- bake(estandarizador.ajustado, train)
  list(train = train.std, valid = valid.std)
}

ajusta_lasso <- function(split){
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
    glmnet(y = ytrain, x = xtrain, lambda = lam, alpha = 1)
  }), residuales = map(modelo, function(mod){
    ## Calculo residuales 
    predict(mod, newx = xvalid) - yvalid
  }))
}

cv.results <- cbind(atributos, respuesta) |>
  as_tibble() |>
  vfold_cv(10) |>
  mutate(results = map(splits, ajusta_lasso))

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
  xlab(expression(1/lambda)) +
  ylab("Error Validación Cruzada") +
  scale_x_log10()
g2
