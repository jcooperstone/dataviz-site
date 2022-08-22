library(tidyverse)
library(tidytuesdayR)
library(ggbreak)
library(ggsci)
library(countrycode)
library(ggimage)

# Repo
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-10-19

gian_crops <- tidytuesdayR::tt_load(2021, week = 43)


pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')
glimpse(pumpkins)

# Crop names
crop_names <- c(`F` = "Field pumpkin", P = "Giant pumpkin",
                S = "Giant squash", W = "Giant watermelon")

pumpkins %>%
  filter(place == 1) %>% 
  separate(col = id, into = c("Year", "Category")) %>% 
  filter(Category %in% c("F", "P", "S", "W")) %>% 
  mutate(Year = ymd(Year, truncated = 2L),
         weight_lbs = as.numeric(gsub(",", "", weight_lbs))) %>% 
  ggplot(aes(Year, weight_lbs, color = Category)) +
  #geom_smooth() +
  geom_point(aes(shape = Category), size = 3) + geom_line() +
  facet_wrap("Category", scales = "free_y",
             labeller = labeller(Category = crop_names)) +
  scale_color_aaas() +
  theme_classic() +
  labs(x = "Year", y = "Weight (lbs)", color = "Crop", shape = "Crop",
       title = "Weight in pounds of the first place of giant crop competition",
       subtitle = "Data downloaded from tidytuesday initiative: 2021 week 43")







chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')
glimpse(chocolate)

chocolate %>%
  filter(review_date == 2010,
         country_of_bean_origin != "Blend") %>% 
  mutate(cocoa_percent = as.numeric(sub("%", "", cocoa_percent))) %>% 
  group_by(country_of_bean_origin) %>% 
  summarise(rating = max(rating/(cocoa_percent/100))) %>% 
  mutate(iso2 = countrycode(.$country_of_bean_origin,
                                 "country.name", "iso2c"),
         Continent = countrycode(.$country_of_bean_origin,
                                 "country.name", "continent")) %>% 
  ggplot(aes(x = rating, y = reorder(country_of_bean_origin, rating),
             fill = Continent)) +
  geom_col() + geom_flag(x= -0.5, aes(image = iso2)) +
  #scale_x_break(c(0.5, 3), scales = 3) +
  expand_limits(x = -.5)  +
  theme_classic() +
  labs(x = "Chocoloate rating",
       y = "Country of bean origing") +
  scale_fill_d3(alpha = 0.5)






















 