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

## Datos de marketing ---------------------------------
data <- read_csv("https://www.statlearning.com/s/Advertising.csv", col_select = 2:5)
data |> colnames()
data |> head()

g1 <- ggplot(data, aes(TV, sales)) + geom_point(color = 'red') + geom_smooth(method = "lm", se = FALSE) + sin_lineas
g2 <- ggplot(data, aes(radio, sales)) + geom_point(color = 'red') + geom_smooth(method = "lm", se = FALSE) + sin_lineas
g3 <- ggplot(data, aes(newspaper, sales)) + geom_point(color = 'red') + geom_smooth(method = "lm", se = FALSE) + sin_lineas

g1 + g2 + g3

## Modelo lineal simple --------------------------------

model <- lm(sales ~ TV, data)

data |>
  mutate(fitted = fitted(model)) |>
  ggplot(aes(TV, sales)) + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_errorbar(aes(ymin = fitted, ymax = sales),
                lty = 1, color = "gray") +
  geom_point(color = 'red') + sin_lineas

### Resumenes de modelos --------------------------

model |> 
      summary()

model |>
  broom::tidy()

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
  broom::tidy()

### Resumenes globales --------------------------------

model |>
  broom::glance() |>
  select(statistic, p.value, df, df.residual)

## Modelos con interacciones ------------------------

model.1 <- lm(sales ~ TV + radio, data)
model.2 <- lm(sales ~ TV + radio + TV:radio, data)

tibble(modelo = list(model.1, model.2),
       tipo   = c("lineal", "interaccion")) |>
  mutate(resultados = map(modelo, broom::tidy)) |>
  select(-modelo) |>
  unnest(resultados) |>
  select(tipo, term, estimate, p.value)

tibble(modelo = list(model.1, model.2)) |>
  mutate(resultados = map(modelo, broom::glance)) |>
  select(-modelo)|>
  unnest(resultados) |>
  select(r.squared, sigma, AIC, deviance)

data <- ISLR::Credit
data |>
  ggplot(aes(Income, Balance, group = Student, color = Student)) +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_point()

## Aplicación: Modelo de regresión asistencia partidos -----------------------

library(tidyverse)
base_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/"

attendance <- read_csv(paste(base_url, "attendance.csv", sep = ""),
                       progress = FALSE, show_col_types = FALSE)
standings <- read_csv(paste(base_url, "standings.csv", sep = ""),
                      progress = FALSE, show_col_types = FALSE)

attendance_joined <- attendance |>
  left_join(standings, by = c("year", "team_name", "team"))
attendance_joined

attendance_joined |>
  filter(!is.na(weekly_attendance)) |>
  ggplot(aes(fct_reorder(team_name, weekly_attendance),
             weekly_attendance,
             fill = playoffs)) +
  geom_boxplot(outlier.alpha = 0.5) +
  labs(
    fill = NULL, x = NULL,
    y = "Weekly NFL game attendance"
  ) + sin_lineas + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

attendance_joined |>
  distinct(team_name, year, margin_of_victory, playoffs) |>
  ggplot(aes(margin_of_victory, fill = playoffs)) +
  geom_histogram(position = "identity", alpha = 0.7) +
  labs(
    x = "Margin of victory",
    y = "Number of teams",
    fill = NULL
  ) + sin_lineas

attendance_joined |>
  mutate(week = factor(week)) |>
  ggplot(aes(week, weekly_attendance, fill = week)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.5) +
  labs(
    x = "Week of NFL season",
    y = "Weekly NFL game attendance"
  ) + sin_lineas

attendance_df <- attendance_joined |>
  filter(!is.na(weekly_attendance)) |>
  select(
    weekly_attendance, team_name, year, week,
    margin_of_victory, strength_of_schedule, playoffs
  )

attendance_df

library(tidymodels)

set.seed(108727)
attendance_split <- attendance_df |>
  initial_split(strata = playoffs)

nfl_train <- training(attendance_split)
nfl_test <- testing(attendance_split)

lm_spec <- linear_reg() |>
  set_engine(engine = "lm")

lm_spec

lm_fit <- lm_spec |>
  fit(weekly_attendance ~ .,
      data = nfl_train
      )

lm_fit |> broom::tidy()

lm_fit |> broom::tidy(conf.int = TRUE)

lm_fit |> broom::glance()

results_train <- lm_fit |>
  predict(new_data = nfl_train) |>
  mutate(truth = nfl_train$weekly_attendance,
         model = "lm")
results_train |>
  rmse(truth = truth, estimate = .pred)

results_test <- lm_fit |>
  predict(new_data = nfl_test) |>
  mutate(truth = nfl_test$weekly_attendance,
         model = "lm")
results_test |>
  rmse(truth = truth, estimate = .pred)

results_test %>%
  mutate(train = "testing") %>%
  bind_rows(results_train %>%
    mutate(train = "training")) %>%
  ggplot(aes(truth, .pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(lty = 2, color = "salmon", size = 1.5) +
  facet_wrap(~train) +
  labs(
    x = "Truth",
    y = "Predicted attendance",
    color = "Type of model"
  ) + sin_lineas
