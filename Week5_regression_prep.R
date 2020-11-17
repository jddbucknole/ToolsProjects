install.packages('png')
library(png)
x <- readPNG("gorilla.png")

subsamp <- sample(1760, 1:dim(x)[1])


library(tidyverse)
ggplot(tibble(x))

women<-read_delim('https://www.dropbox.com/s/685pkte3n3879mn/data9b_w.txt/?dl=1', delim = '\t')
men <- read_delim('https://www.dropbox.com/s/r3wyn2ex20glsoa/data9b_m.txt/?dl=1', delim = '\t')

bmi <- women %>%
  mutate(gender = "Female") %>%
  bind_rows(men %>%
              mutate(gender = "Male")) %>%
  filter(ID != 1786)

bmi %>%
  ggplot(aes(x = steps, y = bmi)) +geom_point()

write_csv(bmi, "bmi.csv")

coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')
summary(lm(total_cup_points ~ aroma + flavor +acidity + body + balance + uniformity + clean_cup + sweetness +cupper_points,coffee_ratings))
cor(coffee_ratings)
