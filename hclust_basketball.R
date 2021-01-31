year <- "2019"
url <- paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_per_game.html")

playerdata <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="per_game_stats"]') %>%
  html_table()

df <- data.frame(playerdata) %>%
  filter(Player !="Player")


#Get rid of non-TOT for 2 team players
#make numeric and remove charater
df1 <- df %>%
  distinct(Player, .keep_all = T) %>%
  select(-c(Rk,Tm)) %>%
  mutate_at(vars(-c(Player,Pos)),as.numeric) %>%
  filter(MP > 25, G > 30) %>%
  select(-FG.,-X3P., -X2P.,-FT., -eFG.) %>%
  mutate(Pos = ifelse(Pos %in% c("PF-SF","SF-SG"),"SF",Pos))

scaled <- df1 %>%
  select(-Player,-Pos) %>%
  mutate_all(scale)



t <- hclust(d = dist(scaled),method = "complete")
plot(t)

t$labels <- df1$Player
plot(t, cex = .5, hang = -1)

clust <- cutree(t, k = 3)

df1$clust <- clust
