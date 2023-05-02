## Setup --------------------------------------------
source("./.utilities/plot-theme.R")

## Graficando un lda con K = 3, p = 2 -------------------------------
set.seed(108)
library(mvtnorm)

Sigma <- .5 * matrix(c(1, .6, .6, 1), nrow = 2)

muestra <- tibble(class = factor(c(1, 2)),
       mu = list(c(-2, 0), c(2, 0)), 
       color.class = c("#F8766D", "#00BFC4")) |>
  mutate(samples = map(mu, function(mean){
    rmvnorm(100, mean = mean, sigma = Sigma) |>
      as_tibble()
  }))

muestra |> 
    unnest(samples) |> 
    ggplot(aes(V1, V2)) + 
    geom_point(aes(color = color.class)) + 
    coord_equal() + 
    sin_lineas + sin_leyenda

modelo.log <- glm(class ~ V1 + V2, data = muestra |> unnest(samples), family = "binomial")

summary(modelo.log)

modelo.log |> broom::tidy()
modelo.log |> broom::glance()

expand.grid(V1 = seq(-4, 4, length.out = 200),
            V2 = seq(-2, 2, length.out = 100)) |>
  as_tibble() |>
  nest(data = c(V1, V2)) |>
  mutate(preds = map(data, function(datos){
    tibble(pred.probs = predict(modelo.log, newdata = datos, type = "response")) |> 
        mutate(pred.class = ifelse(pred.probs <= .5, "#F8766D", "#00BFC4"))
  })) |>
  unnest(data, preds) |>
  ggplot(aes(V1, V2, color= pred.class)) +
    geom_point(size = 1, alpha = .1) + sin_leyenda + sin_lineas + 
    geom_point(data = muestra |> unnest(samples), aes(V1, V2, color = color.class)) +
    coord_equal() 


modelo.lda <- MASS::lda(class ~ V1 + V2, muestra |> unnest(samples))


modelo.lda
