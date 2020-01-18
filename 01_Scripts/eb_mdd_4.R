library(dplyr)
library(ggplot2)
library(Lahman)
library(data.table)

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

battingDT = setDT(copy(Batting))

battingDT[AB > 0][!Pitching, on = "playerID"][
  ,lapply(.SD, sum, na.rm=TRUE), by=playerID, .SDcols=c("H", "AB")][
    average:=H/AB]

battingDT[AB > 0][!Pitching, on="playerID"][
  ,`:=`(H=sum(H), AB=sum(AB), average=(H/AB)),by=playerID]

# compute credible interval
career_eb <- career_eb %>%
  mutate(alpha1 = alpha0 + H,
         beta1 = beta0 + AB - H)

