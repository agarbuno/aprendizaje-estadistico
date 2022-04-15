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
