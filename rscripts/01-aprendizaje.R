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

## Introducción ------------------------------------
library(tidyverse)
library(patchwork)
## Cambia el default del tamaño de fuente 
theme_set(theme_grey(base_size = 18))

data <- read_csv("https://www.statlearning.com/s/Advertising.csv", col_select = 2:5)
data |> colnames()
data |> head()

g1 <- ggplot(data, aes(TV, sales)) + geom_point(color = 'red') + geom_smooth(method = "lm", se = FALSE) 
g2 <- ggplot(data, aes(radio, sales)) + geom_point(color = 'red') + geom_smooth(method = "lm", se = FALSE) 
g3 <- ggplot(data, aes(newspaper, sales)) + geom_point(color = 'red') + geom_smooth(method = "lm", se = FALSE) 

g1 + g2 + g3

ggplot(data, aes(TV, sales)) +
  geom_point(color = 'red') +
  geom_smooth(method = "loess", span = .1, se = FALSE)

ggplot(data, aes(TV, sales)) +
  geom_point(color = 'red') +
  geom_smooth(method = "lm", se = FALSE)

ggplot(data, aes(TV, sales)) +
  geom_point(color = 'red') +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, size = 1)

ggplot(data, aes(TV, sales)) +
  geom_point(color = 'red') +
  geom_smooth(method = "lm", formula = y ~ poly(x, 10), se = FALSE)

## Ejemplo de regresión ----------------------------

library(dplyr)
library(tidyr)

# Definimos la funcion
f <- function(x){
  sin(2*pi*x) + cos(2*pi*x)
}

# Procedimiento de simulacion
simular  <- function(n_muestra, sigma){
  x <- runif(n_muestra, 0, 1) 
  y <- f(x) + rnorm(length(x), mean = 0, sd = sigma)
  data.frame(x, y)
}

# Semilla para resultados reproducibles
set.seed(108727) 

# Simulamos
sd_mod <- 0.5
datos <- simular(20, sd_mod)

# Grafica la función latente y observaciones 
x_plot <- seq(0,1,0.01)
y_plot <- f(x_plot)
ggplot(datos, aes(x=x, y=y), colour='red')+
  geom_point() +
  annotate("line", x=x_plot, y=y_plot, linetype="dotted")

ajuste_mod <- function(m){
  lm(y ~ poly(x, degree=m, raw = TRUE), data = datos) 
}

results <- tibble(grado = seq(1,9)) |>
    mutate(modelos    = map(grado, ajuste_mod),
           prediccion = map(modelos, predict,
                            newdata = data.frame(x = x_plot)))

results |>
  unnest(prediccion) |>
  mutate(x = rep(x_plot, 9),
         verdadero = rep(y_plot, 9)) |>
  pivot_longer(cols = c(prediccion, verdadero)) |>
  ggplot(aes(x, value, linetype = name)) +
  geom_line() +
  facet_wrap(~grado) +
  ylim(c(-3,3)) + 
  annotate("point", x=datos$x, y=datos$y, colour="black")

datos_prueba <- simular(1000, sd_mod)

errores <- results |>
  mutate(prueba = map(modelos, function(modelo) {
    predicciones <- predict(modelo, newdata = data.frame(x = datos_prueba$x))
    predicciones - datos_prueba$y}),
    entrenamiento = map(modelos, residuals)) |>
  pivot_longer(cols = prueba:entrenamiento,
               names_to = "tipo", values_to = "residuales") |>
  unnest(residuales) |>
  group_by(grado, tipo) |>
    summarise(error = mean((residuales)**2), .groups = "drop")

errores |>
  ggplot(aes(grado, error, linetype = tipo)) +
  geom_line() + geom_point()
