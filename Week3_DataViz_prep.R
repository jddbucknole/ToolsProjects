pizza_jared <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_jared.csv")
pizza_barstool <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_barstool.csv")
pizza_datafiniti <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_datafiniti.csv")


video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")
video_games %>%
  ggplot(aes(x = average_playtime, y= metascore)) + geom_point()


library(tidyverse)
library(babynames)
library(scales)
library(ggrepel)
babynames %>%
  group_by(sex, year) %>%
  filter(n == max(n)) %>%
  mutate(sex = ifelse(sex == "M", "Male", "Female")) %>%
  ggplot(aes(x = year, y = prop, color = sex)) + 
    geom_line() + 
    facet_wrap(~sex) +
    geom_text_repel(aes(label = name), size = 2, nudge_x = 2) + 
    geom_point() + 
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Most common name by year", x = "Year", y = "Percent of babies", color = "Sex")

babynames %>%
  group_by(sex, year) %>%
  filter(n == max(n)) %>%
  group_by(sex)

babynames %>%
  filter(name %in% c("John", "Paul", "George", "Ringo"), sex == "M") %>%
  ggplot(aes(x = year, y = n, color = name)) + geom_line() +
    geom_vline(xintercept = 1964) + 
    annotate("text", x = 1989, y = 100000, label = "Beatles appear on Ed Sullivan", size = 5) + 
    labs(title = "The Beatles", x = "Year", y = "Number of Baby Beatles")


ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

ikea %>%
  group_by(category) %>%
  summarize(avgprice = mean(price)) %>%
  ggplot(aes(x = fct_reorder(category, avgprice), y = avgprice)) + 
    geom_col() + coord_flip()
  
tickets <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

install.packages("ggmap")
library(ggmap)
?register_google


register_google(key = "AIzaSyDPwBZWah5bBRRjhIU-HcycANbd1YY9m5U", write = TRUE)

map <- get_googlemap("Philadelphia, PA", zoom = 13, maptype = "roadmap")
ggmap(map) + 
  geom_point(data = tickets, aes(x = lon, y = lat))
library(scales)
 devtools::install_github("sjmgarnier/viridis")
 
 library(viridis)
tickets1<- tickets[sample(nrow(tickets), 1000),]
tickets2 <- tickets %>% filter(violation_desc == "SCHOOL ZONE")
ggmap(map) + 
  stat_density_2d(data = tickets2, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), 
                                       geom= "polygon", size= 0.01, bins = 5)+
  scale_fill_viridis() +
  scale_alpha(range=c(0.2, 0.4), guide=FALSE)

SCtickets3 <- tickets %>%
  count(violation_desc) %>%
  arrange(-n)
  
library(nycflights13)
library(viridis)
library(ggmap)
dat <- flights  %>%
  count(dest, carrier) %>%
  left_join(airports, by = c("dest" = "faa"))
map <- get_googlemap("United States", zoom = 4, maptype = "roadmap")

ggmap(map) +
  geom_point(data = dat, aes(x = lon, y = lat, size = n, color = carrier, alpha = .5)) + 
  facet_wrap(~carrier)





