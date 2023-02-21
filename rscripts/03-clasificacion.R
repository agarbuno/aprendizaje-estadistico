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

## Datos: credito ---------------------------
library(ISLR)
data <- Default
data |> colnames()
data |> head()

g1 <- data |>
  ggplot(aes(balance, income)) +
  geom_point(aes(color = default, shape = default),
             size = 2.5, alpha = .6) +
  sin_leyenda + sin_lineas

g2 <- data |>
  ggplot(aes(default, balance)) +
  geom_boxplot(aes(fill = default)) +
  sin_leyenda + sin_lineas

g3 <- data |>
  ggplot(aes(default, income)) +
  geom_boxplot(aes(fill = default)) +
  sin_leyenda + sin_lineas

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

error_1 <- (1 - 0.01)**2
error_2 <- (1 - 0.00001)**2

c(error_1 = error_1, error_2 = error_2, diferencia = abs(error_1-error_2))

tibble(px = seq(0,1, length.out = 1000)) |>
  mutate(brier.loss = (1 - px)**2, log.loss = -log(px)) |>
  pivot_longer(cols = c(brier.loss, log.loss)) |>
  ggplot(aes(px, value, group = name, color = name)) +
  geom_line(lwd = 2) +
  coord_cartesian(ylim = c(0, 4.5)) + 
  sin_lineas

## Modelo logistico ----------------------------------------------------------

modelo <- glm(default ~ balance, family = "binomial", data = data)

modelo |>
  summary()

modelo |>
  broom::tidy()

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
   mutate(`sigma(link)` = map_dbl(link, function(x){
      exp(x)/(1 + exp(x))
      }))

logistic.respuestas

modelo <- glm(default ~ balance + income + student,
              data = data,
              family = "binomial")

modelo |>
  broom::tidy()

## Una paradoja ----------------------------------
modelo.1 <- glm(default ~ student,
                data = data,
              family = "binomial")

modelo.2 <- glm(default ~ balance + income + student,
              data = data,
              family = "binomial")

modelo.1 |> broom::tidy()

modelo.2 |> broom::tidy()

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
data <- data |> as_tibble() |>
  mutate(default = ifelse(default == "Yes", "positive", "negative"))
data |> head()

data <- data |>
  mutate(default = factor(default, levels = c("positive", "negative")))

lda.model <- MASS::lda(default ~ balance, data)

## Evaluación de modelos -----------------------------------------------------

library(yardstick)
data <- data |>
  as_tibble() |>
  mutate(predicted = predict(lda.model)$class,
         probability = predict(lda.model)$posterior[,1])
data |>
  conf_mat(truth = default, estimate = predicted)

data |>
  accuracy(truth = default, estimate = predicted)

data |>
  recall(truth = default, estimate = predicted)

data |>
  precision(truth = default, estimate = predicted)

data |>
  recall(truth = default, estimate = predicted, event_level = 'second')

data |>
  spec(truth = default, estimate = predicted)

data |>
  precision(truth = default, estimate = predicted, event_level = 'second')

data |>
  f_meas(truth = default, estimate = predicted)

## Cambio de punto de corte --------------------------------------------------

data <- data |>
  mutate(predicted.score = factor(
           ifelse(probability >= .2,"positive", "negative"),
           levels = c("positive", "negative")
         ))

data |>
  accuracy(truth = default, estimate = predicted.score)

data |>
  recall(truth = default, estimate = predicted.score)

data |>
  precision(truth = default, estimate = predicted.score)

data |>
  f_meas(truth = default, estimate = predicted.score)

### Grafico ROC --------------------------------------------------------------

g1 <- data |>
  roc_curve(default, probability) |>
  ggplot(aes(1 - specificity, sensitivity, color = .threshold)) +
  geom_path(size = 1.8) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  sin_lineas

g2 <- data |>
  roc_curve(default, probability) |>
  ggplot(aes(1 - specificity, sensitivity, color = .threshold)) +
  geom_path(size = 1.8) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  sin_lineas +
  xlab("Tasa de Falsos Positivos") +
  ylab("Recall")

g1 + g2

data |>
  roc_auc(default, probability)

g1 <- data |>
  pr_curve(default, probability) |>
  ggplot(aes(recall, precision, color = .threshold)) +
  geom_path(size = 1.8) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  sin_lineas +
  xlab("Recall") +
  ylab("Precision")
g1

data |>
  ## Bin the probability in buckets
  mutate(bins = cut(probability,
                    seq(0,1,length.out = 11))) |>
  ## Group by bin to get summaries
  group_by(bins) |>
  summarise(events = sum(ifelse(default == "positive", 1, 0)),
            count = n(),
            observed.rate = events/count) |>
  ## Compute expected rates
  mutate(predicted.rate = seq(5,100,by=10)/100) |>
  ggplot(aes(predicted.rate, observed.rate)) +
  geom_line() + geom_point() +
  geom_abline(slope = 1, intercept = 0, lty = 2, color = 'grey') +
  sin_lineas

data |>
  gain_curve(default, probability) |>
  ggplot(aes(.percent_tested, .percent_found)) +
  geom_polygon(data = tibble(x = c(0, 3.33,100),
                             y = c(0,100,100)),
               aes(x,y), alpha = .4, fill = 'gray') +
  geom_path(size = 1.8) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  xlab("% Clasificado positivo") + 
  ylab("Sensibilidad") + 
  sin_lineas

data |>
  lift_curve(default, probability) |>
  ggplot(aes(.percent_tested, .lift)) +
  geom_path(size = 1.8) +
  xlab("% Clasificado positivo") + 
  ylab("Lift") + 
  sin_lineas


## Modelo de clasificacion ----------------------------------------------------

library(tidyverse)
base_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/"

attendance <- read_csv(paste(base_url, "attendance.csv", sep = ""),
                       progress = FALSE, show_col_types = FALSE)
standings <- read_csv(paste(base_url, "standings.csv", sep = ""),
                      progress = FALSE, show_col_types = FALSE)

capacity <- attendance |> 
  group_by(team_name) |> 
  summarise(capacity = max(weekly_attendance, na.rm = TRUE))            


attendance_joined <- attendance |>
  left_join(standings, by = c("year", "team_name", "team")) |> 
  left_join(capacity, by = c("team_name"))  

attendance_joined |> 
  mutate(proportion = weekly_attendance/capacity) |> 
  ggplot(aes(proportion)) + 
  geom_histogram() + sin_lineas

attendance_joined |> 
  mutate(proportion = weekly_attendance/capacity) |> 
  ggplot(aes(team_name, proportion)) + 
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.5) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
  sin_lineas

attendance_df <- attendance_joined |> 
  mutate(proportion = weekly_attendance/capacity, 
         sellout    = factor(ifelse(proportion >= .85, "positive", "negative"), 
         levels = c("positive", "negative"))) |> 
  filter(!is.na(weekly_attendance)) |>
  select(
    sellout, team_name, year, week, strength_of_schedule, playoffs
  )

glimpse(attendance_df)

## Ajuste del modelo ----------------------------------------------------------
## Objetivo: Predicciones, dl, multimetrics


library(tidymodels)

attendance_split <- attendance_df |>
  initial_split(strata = playoffs)

nfl_train <- training(attendance_split)
nfl_test <- testing(attendance_split)


set.seed(108727)
log_spec <- logistic_reg() |> 
  set_engine(engine = "glm")

log_fit <- log_spec |>
  fit(sellout ~ .,
      data = nfl_train
      ) 

log_fit |> broom::tidy()

log_fit |> broom::glance()

augment(log_fit, new_data = attendance_df) |>
  conf_mat(truth = sellout, estimate = .pred_class)

augment(log_fit, new_data = nfl_test) |>
  conf_mat(truth = sellout, estimate = .pred_class)

augment(log_fit, new_data = nfl_test) |> 
  accuracy(sellout, .pred_class)

augment(log_fit, new_data = nfl_test) |> 
  recall(sellout, .pred_class)

augment(log_fit, new_data = nfl_test) |> 
  precision(sellout, .pred_class)

augment(log_fit, new_data = nfl_test) |> 
  f_meas(sellout, .pred_class)

augment(log_fit, new_data = nfl_test) |> 
  roc_auc(sellout, .pred_positive)

augment(log_fit, new_data = nfl_test) |> 
  roc_curve(sellout, .pred_positive) |> 
  autoplot()

augment(log_fit, new_data = nfl_test) |> 
  gain_curve(sellout, .pred_positive) |> 
  autoplot()


## Atajos de computo ----------------------------------------------------------

class_metrics <- metric_set(accuracy, recall, precision, f_meas)

augment(log_fit, new_data = nfl_train) |>
    class_metrics(truth = sellout, estimate = .pred_class)


augment(log_fit, new_data = nfl_test) |>
    class_metrics(truth = sellout, estimate = .pred_class)
