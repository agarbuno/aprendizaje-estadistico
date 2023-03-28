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

library(tidymodels)
library(bonsai)
library(vip)

nexp <- 5000; nsamples <- 100;
generate_data <- function(nsamples = 100){
  tibble(id = 1:nsamples) |>
    mutate(  y = map_dbl(id, ~rnorm(1)),
           ch2 = map_dbl(id, ~rchisq(1, 2)),
           m2  = factor(map_dbl(id, ~sample(1:2, 1))),
           m4  = factor(map_dbl(id, ~sample(1:4, 1))),
           m10 = factor(map_dbl(id, ~sample(1:10, 1))),
           m20 = factor(map_dbl(id, ~sample(1:20, 1))),
           nor = map_dbl(id, ~rnorm(1)),
           uni = map_dbl(id, ~runif(1))) |>
    select(-id)
}

set.seed(108727)
generate_data(nsamples = 100) |>
    print(n = 3)

fit_tree <- function(engine){
  data_train <- generate_data() 

  tree_spec <- decision_tree(tree_depth = 2) |>
    set_engine(engine) |>
    set_mode("regression")

  tree_spec |>
    fit(y ~ ., data = data_train) |>
    extract_fit_engine() |>
    vi() |>
    filter(Importance > 0) |> 
    mutate(rank = min_rank(desc(Importance)))
}

## Cuidado! Tarda mucho en correr
nexp <- 5000
results <- tibble(id = 1:(2*nexp)) |>
  mutate(engine = rep(c("rpart", "partykit"), each = nexp)) |>
  mutate(model  = map(engine, fit_tree))

results |>
  unnest(model) |>
  filter(rank == 1) |>
  group_by(engine, Variable) |>
  summarise(prop = sum(rank)/nexp, .groups = "drop") |>
  mutate(engine = factor(engine, levels = c("rpart", "partykit"))) |>
  ggplot(aes(Variable)) +
  geom_bar(aes(y = prop), stat = "identity") + sin_lineas +
  geom_hline(yintercept = 1/7, lty = 2) +
  geom_hline(yintercept = 0.05/7, lty = 2, color = 'salmon') +
  facet_wrap(~engine) + 
  ylab("Proporción como nodo raíz") + xlab("")

library(ISLR2)
hitters <- as_tibble(Hitters) |>
  select(Hits, Years, Salary) |>
  filter(complete.cases(Salary))

library(coin)
independence_test(Salary ~ Years, data = hitters, teststat = "quadratic")

fake_data <- generate_data()
independence_test(y ~ nor, data = fake_data, teststat = "quadratic")

## Clasificacion: Scooby doo -------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2021-07-13')
scooby_raw <- tuesdata$scoobydoo

set.seed(123)
scooby_data <- scooby_raw |>
  mutate(
    imdb = parse_number(imdb),
    year_aired = lubridate::year(date_aired)
  ) |>
  filter(monster_amount > 0, !is.na(imdb)) |>
  mutate(
    monster_real = case_when(
      monster_real == "FALSE" ~ "fake",
      TRUE ~ "real"
    ),
    caught_scooby = case_when(
      caught_scooby == "FALSE" ~ "not caught",
      TRUE ~ "caught"
    ),
    monster_real = factor(monster_real),
    caught_scooby = factor(caught_scooby)
  ) |>
  filter(complete.cases(monster_real, caught_scooby))

independence_test(monster_real ~ caught_scooby,
                  data = scooby_data, teststat = "quadratic")

independence_test(monster_real ~ caught_scooby,
                  data = scooby_data, teststat = "quadratic",
                  distribution = approximate(nresample = 10000))
