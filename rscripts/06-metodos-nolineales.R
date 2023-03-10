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

## Regresión polinomial ------------------------------------------------------
library(ISLR)
set.seed(108727)
## Cargamos datos
data <- tibble(Wage) |> select(year, age, wage, education) |>
  mutate(hi.income = ifelse(wage > 250, 1, 0),
         age = as.numeric(age))
data |> 
  print(n = 5)

g.reg <- data |>
  ggplot(aes(age, wage)) +
  geom_point(color = "gray") +
  geom_smooth(formula = y ~ poly(x, 4),
              method = "lm", se = TRUE,
              fill = "salmon") + sin_lineas +
  xlab("Edad") + ylab("Ingreso")

g.log <- data |>
  mutate(wage.plt = ifelse(hi.income == 1, .20, 0 )) |>
  ggplot(aes(age, wage.plt)) +
  geom_point(color = "gray") +
  geom_smooth(aes(age, hi.income),
              formula = y ~ poly(x, 4),
              method = "glm",
              method.args = list(family = "binomial"),
              se = 2, fill = "salmon") + sin_lineas +
  xlab("Edad") + ylab(expression(paste(P,"( Ingreso >",250,"|Edad)", sep = ""))) +
  coord_cartesian(ylim = c(0, 0.20))

g.reg + g.log

y ~ poly(x, degree = 4)

## Regesion constante por regiones ----------------------------
g.reg <- data |>
  mutate(age.group = cut(age, breaks = c(-Inf, 35, 50, 65, Inf), right = FALSE)) |>
  ggplot(aes(age, wage, group = age.group)) +
  geom_point(color = "gray") + 
  geom_smooth(method = "lm",
              formula = y ~ 1,
              se = TRUE,
              fill = "salmon") + sin_lineas +
  xlab("Edad") + ylab("Ingreso")

g.log <- data |>
  mutate(wage.plt = ifelse(hi.income == 1, .20, 0 )) |>
  mutate(age.group = cut(age, breaks = c(-Inf, 35, 50, 65, Inf), right = FALSE)) |>
  ggplot(aes(age, wage.plt, group = age.group)) +
  geom_point(color = "gray") + # geom_jitter(color = "gray", width = 1, height = .01) + 
  geom_smooth(aes(age, hi.income),
              formula = y ~ 1,
              method = "glm",
              method.args = list(family = "binomial"),
              se = TRUE,
              fill = "salmon") + sin_lineas +
              xlab("Edad") + ylab(expression(paste(P,"( Ingreso >",250,"|Edad)", sep = "")))

g.reg + g.log

g.reg <- data |>
  mutate(age.group = cut(age, breaks = c(-Inf, 35, 50, 65, Inf), right = FALSE)) |>
  ggplot(aes(age, wage, group = age.group)) +
  geom_point(color = "gray") + 
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = TRUE,
              fill = "salmon") + sin_lineas +
  xlab("Edad") + ylab("Ingreso")

g.log <- data |>
  mutate(wage.plt = ifelse(hi.income == 1, .20, 0 )) |>
  mutate(age.group = cut(age, breaks = c(-Inf, 35, 50, 65, Inf), right = FALSE)) |>
  ggplot(aes(age, wage.plt, group = age.group)) +
  geom_point(color = "gray") + # geom_jitter(color = "gray", width = 1, height = .01) + 
  geom_smooth(aes(age, hi.income),
              formula = y ~ x,
              method = "glm",
              method.args = list(family = "binomial"),
              se = TRUE,
              fill = "salmon") + sin_lineas +
  xlab("Edad") + ylab(expression(paste(P,"( Ingreso >",250,"|Edad)", sep = ""))) +
  coord_cartesian(ylim = c(0, 0.20))

g.reg + g.log

## Modelos por segmentos -----------------------------
library(splines)
g.reg <- data |>
  mutate(age.group = cut(age, breaks = c(-Inf, 50, Inf), right = FALSE)) |>
  ggplot(aes(age, wage, group = age.group)) +
  geom_point(color = "gray") + 
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE,
              fill = "salmon") + sin_lineas +
  xlab("Edad") + ylab("Ingreso")

g.log <- data |>
  mutate(wage.plt = ifelse(hi.income == 1, .20, 0 )) |>
  mutate(age.group = cut(age, breaks = c(-Inf, 50, Inf), right = FALSE)) |>
  ggplot(aes(age, wage.plt, group = age.group)) +
  geom_point(color = "gray") + # geom_jitter(color = "gray", width = 1, height = .01) + 
  geom_smooth(aes(age, hi.income),
              formula = y ~ poly(x, 2),
              method = "glm",
              method.args = list(family = "binomial"),
              se = TRUE,
              fill = "salmon") + sin_lineas +
  xlab("Edad") + ylab(expression(paste(P,"( Ingreso >",250,"|Edad)", sep = ""))) +
  coord_cartesian(ylim = c(0, 0.20))


g.reg + g.log

### Splines ------------------------------------
g.reg <- data |>
  mutate(age.group = cut(age, breaks = c(-Inf, 50, Inf), right = FALSE)) |>
  ggplot(aes(age, wage)) +
  geom_point(color = "gray") + 
  geom_smooth(method = "lm",
              formula = y ~ bs(x, knots = c(50), degree = 2),
              se = TRUE,
              fill = "salmon") + sin_lineas +
  geom_vline(xintercept = 50, lty = 2) + 
  xlab("Edad") + ylab("Ingreso")

g.log <- data |>
  mutate(wage.plt = ifelse(hi.income == 1, .20, 0 )) |>
  mutate(age.group = cut(age, breaks = c(-Inf, 50, Inf), right = FALSE)) |>
  ggplot(aes(age, wage.plt)) +
  geom_point(color = "gray") + # geom_jitter(color = "gray", width = 1, height = .01) + 
  geom_smooth(aes(age, hi.income),
              formula = y ~ bs(x, knots = c(50), degree = 2),
              method = "glm",
              method.args = list(family = "binomial"),
              se = TRUE,
              fill = "salmon") + sin_lineas +
  geom_vline(xintercept = 50, lty = 2) + 
  xlab("Edad") + ylab(expression(paste(P,"( Ingreso >",250,"|Edad)", sep = ""))) +
  coord_cartesian(ylim = c(0, 0.20))


g.reg + g.log

### Splines naturales --------------------------
set.seed(108727)
g.cubic <- data |>
  sample_frac(.05) |>
  ggplot(aes(age, wage)) +
  geom_point(color = "gray") + 
  stat_smooth(aes(age, wage, fill = "Spline"), color = 'salmon',
              method = "lm",
              formula = y ~ bs(x, knots = c(35, 50, 65), degree = 3),
              se = TRUE, lty = 1, alpha = .2, fullrange = TRUE) +
  stat_smooth(aes(age, wage, fill = "Spline-natural"),
              method = "lm",
              formula = y ~ ns(x, knots = c(35, 50, 65)), color = 'blue',
              se = TRUE, lty = 1, alpha = .2, fullrange = TRUE) + sin_lineas +
  geom_vline(xintercept = c(35, 50, 65), lty = 2) +
  scale_x_continuous(limits = c(10, 80), expand = c(0,0)) +
  xlab("Edad") + ylab("Ingreso") +
coord_cartesian(ylim = c(0, 300))

g.cubic

g.polsplines <- data |>
    ggplot(aes(age, wage)) +
    geom_point(color = "gray") + 
    stat_smooth(aes(age, wage, fill = "Polinomio"),
                color = 'salmon',
                method = "lm",
                formula = y ~ poly(x, 14),
                se = TRUE, lty = 1,
                alpha = .2, fullrange = TRUE) +
    stat_smooth(aes(age, wage, fill = "Spline-natural"),
                method = "lm",
                formula = y ~ ns(x, df = 14),
                color = 'blue',
                se = TRUE, lty = 1,
                alpha = .2, fullrange = TRUE) + sin_lineas +
    scale_x_continuous(limits = c(10, 80), expand = c(0,0)) +
    xlab("Edad") + ylab("Ingreso") +
    coord_cartesian(ylim = c(0, 300))

g.polsplines

## Suavizamiento (splines) --------------------
library(ggformula)
set.seed(108727)
g1.ssplines <- data |>
  sample_frac(.05)|>
  ggplot(aes(age, wage)) +
  geom_point(color = "gray") +
  geom_spline(aes(age, wage, color = "Suavizamiento"),
            df = 2, 
            color = 'red',
            lty = 1,
            show.legend = TRUE) + 
  sin_lineas +
  ## scale_x_continuous(limits = c(10, 80), expand = c(0,0)) +
  xlab("Edad") + ylab("Ingreso") + ggtitle("df = 2")
  coord_cartesian(ylim = c(0, 300))

set.seed(108727)
g2.ssplines <- data |>
  sample_frac(.05)|>  
  ggplot(aes(age, wage)) +
  geom_point(color = "gray") +
  geom_spline(aes(age, wage, color = "Suavizamiento"),
            df = 15, 
            color = 'red',
            se = TRUE, lty = 1,
            fullrange = TRUE, show.legend = TRUE) + 
  sin_lineas +
  ## scale_x_continuous(limits = c(10, 80), expand = c(0,0)) +
  xlab("Edad") + ylab("Ingreso") + ggtitle("df = 15")
  coord_cartesian(ylim = c(0, 300)) 

set.seed(108727)
g3.ssplines <- data |>
  sample_frac(.05)|>  
  ggplot(aes(age, wage)) +
  geom_point(color = "gray") +
  geom_spline(aes(age, wage, color = "Suavizamiento"),
            df = 40, 
            color = 'red',
            se = TRUE, lty = 1,
            fullrange = TRUE, show.legend = TRUE) + 
  sin_lineas +
  ## scale_x_continuous(limits = c(10, 80), expand = c(0,0)) +
  xlab("Edad") + ylab("Ingreso") + ggtitle("df = 40")
  coord_cartesian(ylim = c(0, 300)) 

g1.ssplines + g2.ssplines + g3.ssplines

## Suavizamiento (regresion local) ----------------------------------
set.seed(108727)
data.plt <- data |>
  sample_frac(.1) |>
  mutate(region = ifelse((20 <= age & age <= 30),
                         TRUE, FALSE))
  g1 <- data.plt |>
    ggplot(aes(age, wage)) +
    geom_smooth(method = "loess",
                span = .35,
                method.args = list(degree = 1),
                color = 'blue', 
                se = TRUE, lty = 1,
                alpha = .2, fullrange = TRUE) +
    geom_smooth(data = filter(data.plt, region),
                aes(age, wage),
                method = "loess",
                span = 10,
                method.args = list(degree = 1),
                color = 'red', fill = 'red',
                se = TRUE, lty = 1,
                alpha = .2, fullrange = TRUE) +
    geom_point(color = "gray", shape = 4) +
    geom_point(data = filter(data.plt, region),
                aes(age, wage),
               color = "salmon") + 
    sin_lineas +
    xlab("Edad") + ylab("Ingreso") +
    coord_cartesian(ylim = c(0, 300)) +
    ggtitle("Centro en x = 25")

set.seed(108727)
data.plt <- data |>
  sample_frac(.1) |>
  mutate(region = ifelse((35 <= age & age <= 45),
                           TRUE, FALSE))
g2 <- data.plt |>
  ggplot(aes(age, wage)) +
  geom_smooth(method = "loess",
              span = .35,
              method.args = list(degree = 1),
                color = 'blue', 
              se = TRUE, lty = 1,
              alpha = .2, fullrange = TRUE) +
  geom_smooth(data = filter(data.plt, region),
              aes(age, wage),
              method = "loess",
                span = 10,
              method.args = list(degree = 1),
              color = 'red', fill = 'red',
              se = TRUE, lty = 1,
              alpha = .2, fullrange = TRUE) +
  geom_point(color = "gray", shape = 4) +
  geom_point(data = filter(data.plt, region),
             aes(age, wage),
             color = "salmon") + 
  sin_lineas +
  xlab("Edad") + ylab("Ingreso") +
  coord_cartesian(ylim = c(0, 300)) +
    ggtitle("Centro en x = 40")

g1 + g2

set.seed(108727)
  data.plt <- data |>
    sample_frac(.1) |>
    mutate(region = ifelse((20 <= age & age <= 30),
                           TRUE, FALSE))
  g1 <- data.plt |>
    ggplot(aes(age, wage)) +
    geom_smooth(method = "loess",
                span = .05,
                method.args = list(degree = 1),
                color = 'blue', 
                se = TRUE, lty = 1,
                alpha = .2, fullrange = TRUE) +
    geom_point(color = "gray", shape = 4) +
    sin_lineas +
    xlab("Edad") + ylab("Ingreso") +
    coord_cartesian(ylim = c(0, 300)) +
    ggtitle("Span = 0.05")

  g2 <- data.plt |>
    ggplot(aes(age, wage)) +
    geom_smooth(method = "loess",
                span = .15,
                method.args = list(degree = 1),
                color = 'blue', 
                se = TRUE, lty = 1,
                alpha = .2, fullrange = TRUE) +
    geom_point(color = "gray", shape = 4) +
    sin_lineas +
    xlab("Edad") + ylab("Ingreso") +
    coord_cartesian(ylim = c(0, 300)) +
    ggtitle("Span = 0.15")

  g3 <- data.plt |>
    ggplot(aes(age, wage)) +
    geom_smooth(method = "loess",
                span = 0.5,
                method.args = list(degree = 1),
                color = 'blue', 
                se = TRUE, lty = 1,
                alpha = .2, fullrange = TRUE) +
    geom_point(color = "gray", shape = 4) +
    sin_lineas +
    xlab("Edad") + ylab("Ingreso") +
    coord_cartesian(ylim = c(0, 300)) +
    ggtitle("Span = 0.50")

g1 + g2 + g3

library(mgcv)
library(mgcViz)
set.seed(108727)
data.plt <- data |>
  sample_frac(.75) |>
  mutate(year = as.numeric(year),
         education = factor(as.numeric(education))) 

gam.model <- gam(wage ~ ns(year, df = 5) +
                   ns(age, df = 5) +
                   education, data = data.plt)
b <- getViz(gam.model)
print(plot(b, allTerms = TRUE) +
      l_fitLine(linetype = 1) +      
      l_ciLine(mul = 1, linetype = 3) + 
      l_ciPoly(mul = 2) +
      l_rug(alpha = 0.8) +
      ## l_points(shape = 19, size = 1, alpha = 0.1) +
      l_ciBar(mul = 2) + l_fitPoints(size = 1, col = 2) +
      theme_get() + sin_lineas,
      pages = 1)
