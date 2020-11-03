install.packages("brickr")
brickr::lego_colors
Pitching %>%
  filter(playerID == "cansejo01")
Batting %>%
  filter(playerID == "rijojo01")

Appearances %>%
  filter(playerID == "cansejo01", G_p == TRUE)

Fielding %>%
  filter(playerID == "cansejo01")

Pitching %>%
  filter(yearID >2000,G ==1)


#Position Player Pitching
Fielding %>%
  group_by(playerID, POS) %>%
  count() %>%
  mutate(POS_P = ifelse(POS == "P", "Pitcher", "NonPitcher")) %>%
  ungroup(POS) %>%
  group_by(playerID,POS_P) %>%
  summarize(games = sum(n)) %>%
  pivot_wider( names_from = POS_P, values_from = games,values_fill = 0) %>%
  mutate(diff = NonPitcher - Pitcher, PPP = ifelse(Pitcher > 0  &diff >20, 1,0)) %>%
  filter(PPP == 1) %>%
  ungroup() %>%
  left_join(Pitching, by = "playerID") %>%
  select(playerID, G, yearID,IPouts, R, SO)  %>%
  filter(yearID >1950) %>%
  arrange(-SO) %>%
  left_join(People, by = c("playerID" = "bbrefID")) %>%
  select(nameFirst, nameLast, yearID, SO) %>%
  View()
  

Batting %>%
  group_by(playerID) %>%
  summarize(SBtot = sum(SB)) %>%
  left_join(People, by = c("playerID" = "bbrefID")) %>%
  select(First = nameFirst, Last = nameLast, SBtot, birthCountry) %>%
  ungroup() %>%
  group_by(birthCountry) %>%
  mutate(totplayer = n()) %>%
  filter(SBtot > 200) %>%
  group_by(totplayer) %>%
  count(birthCountry) %>%
  mutate(percfast = n / totplayer) %>%
  arrange(-percfast)
