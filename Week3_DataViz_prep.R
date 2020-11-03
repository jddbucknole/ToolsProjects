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
    annotate("text", x = 1985, y = 100000, label = "Beatles appear on Ed Sullivan", size = 6) + 
    labs(title = "The Beatles", x = "Year", y = "Number of Baby Beatles")



