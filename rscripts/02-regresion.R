## Setup ----------------------------------------------
library(tidyverse)
library(patchwork)
set.seed(108727)
## Cambia el default del tamaño de fuente 
theme_set(theme_grey(base_size = 18))

## Cambia el número de decimales para mostrar
options(digits = 2)


## Datos de marketing ---------------------------------
data <- read_csv("https://www.statlearning.com/s/Advertising.csv", col_select = 2:5)
data |> colnames()
data |> head()

g1 <- ggplot(data, aes(TV, sales)) + geom_point(color = 'red') + geom_smooth(method = "lm", se = FALSE) 
g2 <- ggplot(data, aes(radio, sales)) + geom_point(color = 'red') + geom_smooth(method = "lm", se = FALSE) 
g3 <- ggplot(data, aes(newspaper, sales)) + geom_point(color = 'red') + geom_smooth(method = "lm", se = FALSE) 

g1 + g2 + g3

## Modelo lineal simple --------------------------------

model <- lm(sales ~ TV, data)

data |>
  mutate(fitted = fitted(model)) |>
  ggplot(aes(TV, sales)) + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_errorbar(aes(ymin = fitted, ymax = sales),
                lty = 1, color = "gray") +
  geom_point(color = 'red')

### Resumenes de modelos --------------------------

model |> 
      summary()

model |>
  broom::tidy() |>
  as.data.frame()

### Simulación de variabilidad ----------------------------

genera_datos <- function(id){
  a <- 1; b <- 0; n <- 100
  tibble(x = runif(n, -1, 1),
         y = a * x + b + rnorm(n, sd = 1))
}
ajusta_modelo <- function(datos){
  modelo <- lm(y ~ x, datos)
  modelo
}

simulacion <-  tibble(id = seq(1, 10)) |>
    mutate(datos  = map(id, genera_datos),
           modelo = map(datos, ajusta_modelo),
           ajuste = map(modelo, broom::tidy))

params <- simulacion |>
  select(id, ajuste) |>
  unnest(ajuste) |>
  group_by(term) |>
  summarise(estimate = mean(estimate)) |>
  pull(estimate)

simulacion |>
  select(id, ajuste) |>
  unnest(ajuste) |>
  pivot_wider(names_from = term, values_from = estimate, id_cols = id) |>
  ggplot() +
  geom_abline(aes(intercept = `(Intercept)`,
                  slope = x), alpha = .7) +
  geom_abline(intercept = 0, slope = 1, color = 'red', size = 3) + 
  geom_abline(intercept = params[1], slope = params[2], color = 'blue', size = 2, lty = 2)

## Modelo lineal multiple --------------------------------

model <- lm(sales ~ ., data)

model |>
  broom::tidy() |>
  as.data.frame()

### Resumenes globales --------------------------------

model |>
  broom::glance() |>
  select(statistic, p.value, df, df.residual) |>
  as.data.frame()

model.1 <- lm(sales ~ TV + radio, data)
model.2 <- lm(sales ~ TV + radio + TV:radio, data)

tibble(modelo = list(model.1, model.2),
       tipo   = c("lineal", "interaccion")) |>
  mutate(resultados = map(modelo, broom::tidy)) |>
  select(-modelo) |>
  unnest(resultados) |>
  select(tipo, term, estimate, p.value) |>
  as.data.frame()

tibble(modelo = list(model.1, model.2)) |>
  mutate(resultados = map(modelo, broom::glance)) |>
  select(-modelo)|>
  unnest(resultados) |>
  select(r.squared, sigma, AIC, deviance) |>
  as.data.frame()

data <- ISLR::Credit
data |>
  ggplot(aes(Income, Balance, group = Student, color = Student)) +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_point()
