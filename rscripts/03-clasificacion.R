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

## Datos: credito ---------------------------
library(ISLR)
data <- Default
data |> colnames()
data |> head()

g1 <- data |>
  ggplot(aes(balance, income)) +
  geom_point(aes(color = default, shape = default),
             size = 2.5, alpha = .6) +
  sin_leyenda

g2 <- data |>
  ggplot(aes(default, balance)) +
  geom_boxplot(aes(fill = default)) +
  sin_leyenda

g3 <- data |>
  ggplot(aes(default, income)) +
  geom_boxplot(aes(fill = default)) +
  sin_leyenda

g1 + g2 + g3 + plot_layout(ncol = 3, widths = c(3,1,1))

g1 <- data |>
  mutate(default = ifelse(default == "Yes", 1, 0)) |>
  ggplot(aes(balance, default)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() + sin_lineas +
  geom_hline(yintercept = c(1,0) , lty = 2) +
  ggtitle("Regresión lineal")


g2 <- data |>
  mutate(default = ifelse(default == "Yes", 1, 0)) |>
  ggplot(aes(balance, default)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  geom_point() + sin_lineas +
  geom_hline(yintercept = c(1,0) , lty = 2) +
  ggtitle("Regresión logística")


g1 + g2

## Modelo logistico -------------------------

modelo <- glm(default ~ balance, family = "binomial", data = data)

modelo |>
  summary()

modelo |>
  broom::tidy() |>
  as.data.frame()

logistic.respuestas <- tibble(type = c("response", "link")) |>
  mutate(preds = map(type, function(type.str){
                 predict(modelo,
                         tibble(balance = c(1000, 2000)),
                         type = type.str) |>
                   as_tibble()               
  })) |>
  unnest(preds) |>
  mutate(balance = rep(c(1000, 2000), 2)) |>
  pivot_wider(values_from = value, names_from = type) |>
  as.data.frame() |>
   mutate(`sigma(link)` = map(link, function(x){
      exp(x)/(1 + exp(x))
      }))

logistic.respuestas

modelo <- glm(default ~ balance + income + student,
              data = data,
              family = "binomial")

modelo |>
  broom::tidy() |>
  as.data.frame()

## Una paradoja ----------------------------------
modelo.1 <- glm(default ~ student,
                data = data,
              family = "binomial")

modelo.2 <- glm(default ~ balance + income + student,
              data = data,
              family = "binomial")

modelo.1 |> tidy() |> as.data.frame()

modelo.2 |> tidy() |> as.data.frame()

g1 <- data |>
  filter(balance <= 2200) |>
    mutate(balance.discrete = cut(balance, breaks = 20)) |>
  group_by(student, balance.discrete) |>
  summarise(count = n(),
            defaults = sum(ifelse(default == 'Yes', 1, 0)),
            rate  = defaults/count) |>
  ungroup() |>
  ggplot(aes(balance.discrete, rate)) +
  geom_line(aes(group = student, color = student)) +
  geom_hline(data = data |>
               group_by(student) |>
               summarise(rate = mean(ifelse(default == "Yes", 1, 0))),
             aes(yintercept = rate, color = student), lty = 2) + 
  sin_leyenda + sin_lineas +
  theme(axis.text.x = element_blank()) +
  xlab("balance") + ylab("Tasa default")

g2 <- data |>
  ggplot(aes(student, balance)) +
  geom_boxplot(aes(fill = student)) + sin_lineas + sin_leyenda

g1 + g2

## Ejemplo analisis discriminante ----------------- 
g1 <- tibble(x = seq(-4, 4, length.out = 100)) |>
  mutate(f.1 = dnorm(x, -2),
         f.2 = dnorm(x,  2)) |>
  pivot_longer(cols = f.1:f.2) |>
  ggplot(aes(x, value)) +
  geom_line(aes(group = name, color = name)) +
  sin_leyenda + sin_lineas + 
  geom_vline(xintercept = 0, lty = 2) +
  ggtitle(expression(pi[1]==pi[2])) 

g2 <- tibble(x = seq(-4, 4, length.out = 100)) |>
  mutate(f.1 = .3 * dnorm(x, -2),
         f.2 = .7 * dnorm(x,  2)) |>
  pivot_longer(cols = f.1:f.2) |>
  ggplot(aes(x, value)) +
  geom_line(aes(group = name, color = name)) +
  sin_leyenda + sin_lineas + 
  geom_vline(xintercept = -0.225, lty = 2) +
  ggtitle(expression(pi[1]<pi[2]))

g1 + g2

## Graficando un lda con K = 3, p = 2 -------------------------------
library(mvtnorm)

Sigma <- matrix(c(1, .6, .6, 1), nrow = 2)

poblacion <- tibble(class = c(1, 2, 3),
       mu = list(c(-1,-1), c(1,2), c(2,1))) |>
  mutate(samples = map(mu, function(mean){
    rmvnorm(1000, mean = mean, sigma = Sigma) |>
      as_tibble()
  }))

modelo.lda <- MASS::lda(class ~ V1 + V2, poblacion |> unnest(samples))

expand.grid(V1 = seq(-4, 5, length.out = 100),
            V2 = seq(-4, 4, length.out = 100)) |>
  as_tibble() |>
  nest(data = c(V1, V2)) |>
  mutate(preds = map(data, function(datos){
    tibble(class = predict(modelo.lda, newdata = datos)$class,
           pi.1  = dmvnorm(datos, mean = c(-1,-1), sigma = Sigma), 
           pi.2  = dmvnorm(datos, mean = c(1,2), sigma = Sigma),
           pi.3  = dmvnorm(datos, mean = c(2,1), sigma = Sigma))
  })) |>
  unnest(data, preds) |>
  ggplot(aes(V1, V2, color=class)) +
    geom_point(size = 1, alpha = .4) + sin_leyenda + sin_lineas + 
  geom_contour(aes(V1, V2, z = pi.1), breaks = c(2e-2), color = "#F8766D") +
  geom_contour(aes(V1, V2, z = pi.2), breaks = c(2e-2), color = "#7CAE00") +
  geom_contour(aes(V1, V2, z = pi.3), breaks = c(2e-2), color = "#00BFC4") +
  coord_equal()

## Clasificacion y métricas -----------------
options(digits = 3)

data <- Default
data |> head()

data <- data |>
  mutate(default = factor(default, levels = c("Yes", "No")))

lda.model <- MASS::lda(default ~ balance, data)

library(yardstick)
data <- data |>
  as_tibble() |>
  mutate(predicted = predict(lda.model)$class,
         probability = predict(lda.model)$posterior[,1])
data |>
  conf_mat(truth = default, estimate = predicted)

data |>
  accuracy(truth = default, estimate = predicted) |>
  as.data.frame()

data |>
  recall(truth = default, estimate = predicted) |>
  as.data.frame()

data |>
  recall(truth = default, estimate = predicted, event_level = 'second') |>
  as.data.frame()

data |>
  f_meas(truth = default, estimate = predicted) |>
  as.data.frame()

### Modificamos el punto de corte ---------------------

data <- data |>
  mutate(predicted.score = factor(ifelse(probability >= .2,"Yes", "No"), levels = c("Yes", "No")))

data |>
  accuracy(truth = default, estimate = predicted.score) |>
  as.data.frame()

data |>
  recall(truth = default, estimate = predicted.score) |>
  as.data.frame()

data |>
  recall(truth = default, estimate = predicted.score, event_level = 'second') |>
  as.data.frame()

### Grafico ROC --------------------------

g1 <- data |>
  roc_curve(default, probability) |>
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  sin_lineas

g2 <- data |>
  roc_curve(default, probability) |>
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  sin_lineas +
  xlab("Tasa de Falsos Positivos") +
  ylab("Tasa de Verdaderos Positivos")

g1 + g2

data |>
  roc_auc(default, probability) |>
  as.data.frame()

data |>
  ## Bin the probability in buckets
  mutate(bins = cut(probability,
                    seq(0,1,length.out = 11))) |>
  ## Group by bin to get summaries
  group_by(bins) |>
  summarise(events = sum(ifelse(default == "Yes", 1, 0)),
            count = n(),
            observed.rate = events/count) |>
  ## Compute expected rates
  mutate(predicted.rate = seq(5,100,by=10)/100) |>
  ggplot(aes(predicted.rate, observed.rate)) +
  geom_line() + geom_point() +
  geom_abline(slope = 1, intercept = 0, lty = 2, color = 'grey') +
  sin_lineas

g1 <- data |>
  ## Muestreo aleatorio para mantener clases balanceadas 50%
  group_by(default) |>
  sample_n(300, replace = FALSE) |>
  ungroup() |>
  ## Ordenamos por score
  arrange(-probability) |>
  mutate(found = cumsum(ifelse(default == "Yes", 1, 0))/3,
         tested = 1:n()/n()*100) |>
  select(probability, found, tested) |>
  ggplot(aes(tested, found)) +
  geom_polygon(data = tibble(x = c(0,50,100),
                             y = c(0,100,100)),
               aes(x,y), alpha = .4, fill = 'gray') +
  geom_abline(slope = 1, intercept = 0, lty = 2, color = 'gray') +
  geom_line() +
  sin_lineas +
  ylab("Casos encontrados (%)") +
  xlab("Casos probados (%)")  
g1
