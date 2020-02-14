library(Lahman)
library(dplyr)
library(tidyr)
library(data.table)

# grab career batter averages of non-pitchers
# allow players that have pitched <= 3 games, like Ty Cobb
pitchers <- Pitching %>%
  group_by(playerID) %>%
  summarize(gamesPitched = sum(G)) %>%
  filter(gamesPitched > 3)

# add player names
player_names <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast, bats) %>%
  unite(name, nameFirst, nameLast, sep = " ")

# include the "bats" and "year" column for later
career_full <- Batting %>%
  filter(AB > 0) %>%
  anti_join(pitchers, by = "playerID") %>%
  group_by(playerID) %>%
  summarise(H = sum(H), AB = sum(AB), year = mean(yearID)) %>%
  inner_join(player_names, by = "playerID") %>%
  filter(!is.na(bats))

# we do'nt need all this data for every step
career <- career_full %>%
  select(-bats, -year)

#morgajo02
