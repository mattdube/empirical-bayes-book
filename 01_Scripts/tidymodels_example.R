# Tidymodels
library(tune)
library(dials)
library(parsnip)
library(rsample)
library(recipes)
library(textrecipes)
library(yardstick)
library(vip)

# Parallel Processing
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

# Data Cleaning
library(janitor)

# EDA
library(skimr)
library(correlationfunnel)
library(DataExplorer)

# ggplot2 Helpers
library(gghighlight)
library(patchwork)

# Core
library(tidyverse)
library(tidyquant)
library(knitr)

car_prices_tbl <- read_csv("00_Data/data.csv") %>%
  clean_names() %>%
  select(msrp, everything())

car_prices_tbl %>%
  head(5) %>%
  kable()


# plot Engine HP vs MSRP
car_plot_data_tbl <- car_prices_tbl %>%
  mutate(make_model = str_c(make, " ", model)) %>%
  select(msrp, engine_hp, make_model)

car_plot_data_tbl %>%
  ggplot(aes(engine_hp, msrp)) +
  geom_point(color = palette_light()["blue"], alpha = 0.15) +
  geom_smooth(method = "lm") +
  scale_y_log10(label = scales::dollar_format()) +
  labs(title = "Engine Horsepower vs MSRP by Vehicle Age",
       x = "Engine HP", y = "MSRP (log scale)") +
  theme_tq()


p1 <- car_plot_data_tbl %>%
  ggplot(aes(engine_hp, msrp)) +
  geom_point(color = palette_light()["blue"]) +
  scale_y_log10(label = scales::dollar_format()) +
  gghighlight(msrp > 650000, label_key = make_model,
              unhighlighted_colour = alpha("grey", 0.05),
              label_params = list(size = 2.5),) +
  theme_tq() +
  labs(title = "Vehicles with MSRP > $650K",
       x = "Engine HP", y = "MSRP (log scale)")

p2 <- car_plot_data_tbl %>%
  ggplot(aes(engine_hp, msrp)) +
  geom_point(color = palette_light()["blue"]) +
  scale_y_log10(label = scales::dollar_format()) +
  gghighlight(msrp < 10000, engine_hp > 350, label_key = make_model,
              unhighlighted_colour = alpha("grey", 0.05),
              label_params = list(size = 2.5), ) +
  theme_tq() +
  labs(title = "Vehicles with MSRP < $10K and Engine HP > 350",
       x = "Engine HP", y = "MSRP (log scale)")

# Patchwork for stacking ggplots on top of each other
p1 / p2

# correlationfunnel 3-step process
car_prices_tbl %>%
  drop_na(engine_hp, engine_cylinders, number_of_doors, engine_fuel_type) %>%
  binarize(n_bins = 5) %>%
  correlate(`msrp__-Inf_18372`) %>%
  plot_correlation_funnel()


car_plot_data_tbl <- car_prices_tbl %>%
  mutate(make_model = str_c(make, " ", model),
         year_lte_2000 = ifelse(year <= 2000, "2000 or Older", "2001 or Newer")
         ) %>%
  select(msrp, year_lte_2000, engine_hp, make_model)

car_plot_data_tbl %>%
  ggplot(aes(engine_hp, msrp, color = year_lte_2000)) +
  geom_point(alpha = 0.05) +
  scale_color_tq() +
  scale_y_log10(label = scales::dollar_format()) +
  geom_smooth(method = "lm") +
  theme_tq() +
  labs(title = "Engine Horsepower vs MSRP by Vehicle Age",
       x = "Engine HP", y = "MSRP (log scale)")



# EDA using skimr and DataExplorer
skim(car_prices_tbl)

plot_missing(car_prices_tbl) + theme_tq()
car_prices_tbl %>% plot_bar(maxcat = 50, nrow = 2)
car_prices_tbl %>% count(market_category, sort = TRUE)
car_prices_tbl %>% plot_histogram()

car_prices_tbl %>%
ggplot(aes(as.factor(engine_cylinders), msrp)) +
  geom_violin() +
  geom_jitter(alpha = 0.05) +
  scale_y_log10(label=scales::dollar_format())
