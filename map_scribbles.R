library(tidyverse)
library(readxl)
GNI <- read_excel("Income by Country.xlsx", 
                  sheet = "GNI per capita")
View(GNI)

world <- map_data("world")
head(world)
GNI2000 <- GNI %>%
  select(Country, `2000`)%>%
  mutate(GNIval = as.numeric(`2000`))

library(ggmap)
GNI2000 %>%
  left_join(world, by = c("Country" = "region")) %>%
  ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = GNIval), color = "white")  + 
  scale_fill_viridis_c(option = "C") + theme_map()


test <-GNI2000 %>%
  left_join(world, by = c("Country" = "region"))


mapoh <- map_data("county") %>% filter(region=="ohio")
ggplot() +
  geom_path(data = mapoh, aes(long, lat, group = group), color = "gray70") + 
  geom_point(data = sbohll%>%filter(Longitude < -1), aes(Longitude, Latitude, color = Brand, shape = `Ownership Type`), size = 1, alpha = .5) + 
  #coord_map('albers', lat0 = 40, lat1 = 41) + 
  theme_minimal()


COVID19_state <- read_csv("~/R/Inclass_Tools/COVID19_state.csv")
"state" %>%
  map_data() %>%
  head()

COVID19_state %>%
  mutate(region = tolower(State)) %>%
  left_join(map_data("state")) %>%
  ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = Deaths), color = 'white')


big_mac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')
big_mac %>%
  mutate(country = tolower(name)) %>%
  left_join(map_data("world"), by = c("name" = "region")) %>%
  ggplot() +
    geom_polygon(aes(x = long, y = lat, group = group, fill = dollar_price), color = 'white')


chipotle <- read_csv("chipotle_stores.csv")
library(scales)
chipotle %>%
  mutate(region = tolower(state)) %>%
  count(region) %>%
  left_join(map_data("state")) %>%
  ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = n), color = 'white') +
  scale_fill_gradient2(low = "red",mid = "yellow", midpoint = 200,high = "green") + 
  coord_map() +
  theme_void()

statepop <- read_csv("state_pop_2018.csv")
chipotle %>%  
 # mutate(region = tolower(state)) %>%
  count(state) %>%
  left_join(statepop, by = c("state"="State")) %>%
  mutate(region = tolower(state)) %>%
  left_join(map_data("state")) %>%
  mutate(chip_per_mil = n / (Population2018/1000000)) %>%
  ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = chip_per_mil), color = 'white') +
  scale_fill_gradient2(low = "red",mid = "yellow", midpoint = 9,high = "green") + 
  coord_map() +
  theme_void()


register_google(key = 'AIzaSyDPwBZWah5bBRRjhIU-HcycANbd1YY9m5U')
fisher <- geocode('Fisher College of Business')
myMap <- get_map(location = fisher, source = "google", zoom = 12)
ggmap(myMap) + 
  geom_point(data = chipotle, aes(x = longitude, y = latitude), color = 'darkgreen', size = 4) + 
  geom_point(data = fisher, aes(x = lon, y = lat), color = 'red', size = 5) + 
  labs(title = "Chipotle in Columbus")
