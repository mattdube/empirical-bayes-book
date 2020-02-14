library(dplyr)
library(ggplot2)
library(Lahman)
library(data.table)
library(tidyr)

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

# battingDT = setDT(copy(Batting))
#
# battingDT[AB > 0][!Pitching, on = "playerID"][
#   ,lapply(.SD, sum, na.rm=TRUE), by=playerID, .SDcols=c("H", "AB")][
#     average:=H/AB]
#
# battingDT[AB > 0][!Pitching, on="playerID"][
#   ,`:=`(H=sum(H), AB=sum(AB), average=(H/AB)),by=playerID]

# compute credible interval
career_eb <- career_eb %>%
  mutate(alpha1 = alpha0 + H,
         beta1 = beta0 + AB - H)


reds_75_career_eb <- career_eb %>%
  filter(name %in% c("Johnny Bench", "George Foster", "Joe Morgan", "Tony Perez",
                     "Cesar Geronimo", "Pete Rose", "Dan Driessen")) %>%
  filter(AB > 300)

reds_75_career_eb <- reds_75_career_eb %>%
  mutate(low = qbeta(.025, alpha1, beta1),
         high = qbeta(.975, alpha1, beta1))


reds_75_career_eb %>%
  ggplot(aes(eb_estimate, name)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low, xmax = high)) +
  geom_vline(xintercept = alpha0 / (alpha0 + beta0), color = "red", lty = 2) +
  xlab("Estimated batting average (w/ 95% interval)") +
  ylab("Player") +
  theme_minimal()
