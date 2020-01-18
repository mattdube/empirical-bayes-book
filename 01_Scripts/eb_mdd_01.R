library(ggplot2)
library(dplyr)

num_trials <-  10e6

simulations <- tibble(
  true_average = rbeta(num_trials, 81, 219),
  hits = rbinom(num_trials, 300, true_average)
)

simulations

hit_100 <- simulations %>%
  filter(hits == 100)

hit_100

hit_100 %>%
  ggplot(aes(x = true_average)) +
  geom_histogram(aes(y=..density..),color="black", fill="gray") +
  geom_density(color="red") +
  theme_minimal()


# wha itf our player got 60 hits, or 80, or 100?
simulations %>%
  filter(hits %in% c(60, 80, 100)) %>%
  ggplot(aes(true_average, color=factor(hits))) +
  geom_density() +
  labs(x = "True average of players with H hits / 300 at-bats",
       color = "H") +
  theme_minimal()
