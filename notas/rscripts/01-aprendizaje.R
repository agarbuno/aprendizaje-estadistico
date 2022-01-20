library(tidyverse)
library(patchwork)

data <- read_csv("https://www.statlearning.com/s/Advertising.csv", col_select = 2:5)
data |> colnames()
data |> head()

g1 <- ggplot(data, aes(TV, sales)) + geom_point(color = 'red') + geom_smooth(method = "lm", se = FALSE) 
g2 <- ggplot(data, aes(radio, sales)) + geom_point(color = 'red') + geom_smooth(method = "lm", se = FALSE) 
g3 <- ggplot(data, aes(newspaper, sales)) + geom_point(color = 'red') + geom_smooth(method = "lm", se = FALSE) 

g1 + g2 + g3
