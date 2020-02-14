library(dplyr)
library(ggplot2)
library(tidyr)
library(Lahman)
library(data.table)
library(stats4)

# filter out pitchers
career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(Pitching, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>%
  mutate(average = H / AB)

# include names along with player IDs
career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID") %>%
  dplyr::select(-playerID)

career

# find the best and worst hitters
# step 1: esimate a prior from all your data

# histogram of battering averages of all players
# with more than 500 ABs
career %>%
  filter(AB > 500) %>%
  ggplot(aes(average)) +
  geom_histogram(color="black", fill="gray") +
  theme_minimal()


career_filtered <- career %>%
  filter(AB > 500)

# log-likelihood function
ll <- function(alpha, beta) {
  x <- career_filtered$H
  total <- career_filtered$AB
  -sum(VGAM::dbetabinom.ab(x, total, alpha, beta, log = TRUE))
}

m <- mle(ll, start = list(alpha = 1, beta = 10), method = "L-BFGS-B",
          lower = c(0.0001, .1))


ab <- coef(m)

alpha0 <- ab[1]
beta0 <- ab[2]

alpha0
beta0

# empirical Bayes estimate
career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0))

career_eb %>%
  arrange(desc(eb_estimate))
