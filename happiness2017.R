# setwd("C:/Users/Kanishka/happiness/")
setwd("happiness/")
library(tidyverse)
library(kani)
library(extrafont)
library(ggrepel)
options(scipen = 99)

red = "#ff7473"
blue = "#47b8e0"
yellow = "#ffc952"
green = "#8cd790"
purple = "#6a60a9"
orange = "#f68657"
dark_pink = "#d81159"
dark_grey = "#6e7783"

happiness_2015 <- read.csv("data/2015.csv") %>% mutate(year = "2015")
happiness_2016 <- read.csv("data/2016.csv") %>% mutate(year = "2016")
happiness_2017 <- read.csv("2017.csv") %>% mutate(year = "2017")

countries <- happiness_2017 %>%
  select(Country, Happiness.Score) %>%
  inner_join(happiness_2016 %>% select(Country, Happiness.Score), by = "Country") %>%
  mutate(score_2017 = Happiness.Score.x,
         score_2016 = Happiness.Score.y,
         difference = round(score_2017 - score_2016, 4)) %>%
  select(-c(Happiness.Score.x, Happiness.Score.y))

blue = "#30a2da"
light_red = "#f8766d"
light_blue = "#00bfc4"

happier <- countries %>%
  top_n(10, difference) %>%
  arrange(desc(difference))

sadder <- countries %>%
  top_n(10, -difference) %>%
  arrange(-difference)

top_10 <- rbind(happier, sadder) %>%
  mutate(change = ifelse(difference < 0, "Worsened", "Improved"),
         Country = gsub(" ", "\n", Country))
top_10_plot <- top_10 %>%
  mutate(Country = reorder(Country, difference)) %>%
  ggplot(aes(Country, difference, color = change)) +
  geom_segment(aes(x = Country, xend = Country, y = 0, yend = difference), size = 2, alpha = 0.7) + 
  geom_point(size = 3) +
  geom_point(size = 5.5, alpha = 0.4) +
  geom_hline(yintercept = 0, lty = 1, size = 1) + 
  geom_text(aes(x = "Brazil", y = 0.5), label = "Happier in 2016 than\nthey were in 2015", color = light_blue, size = 5, family = "Roboto", fontface = 2) +
  geom_text(aes(x = "Egypt", y = -0.5), label = "Unhappier in 2016 than\nthey were in 2015", color = light_red, size = 5, family = "Roboto", fontface = 2) +
  coord_flip() +
  scale_color_manual(values = c(light_blue, light_red)) +
  scale_y_continuous(breaks = seq(-1,1.25, by = 0.25), limits = c(-1,1)) + 
  theme_kani() +
  labs(title = "Top 10 happier/sadder countries from 2015 to 2016", 
       subtitle = "Shows difference in Happiness Scores calculated\nby the World Happiness Report in 2016 and 2017",
       y = "Difference in Happiness Score",
       caption = "By Kanishka Misra\nData Source: Kaggle"
  ) + 
  theme(legend.position = "None")
top_10_plot
ggsave("2015to2016.png", top_10_plot, height = 9, width = 15)

View(happiness_2017 %>% select(-c(Happiness.Rank, Whisker.high, Whisker.low)))
View(happiness_2015 %>% select(-c(Happiness.Rank, Standard.Error, Region)))


happiness_all <- rbind(happiness_2015 %>% select(Country, Happiness.Score, year), happiness_2016 %>% select(Country, Happiness.Score, year), happiness_2017 %>% select(Country, Happiness.Score, year))

happiness_all <- happiness_all %>% mutate(year = ifelse(year == "2015", "2014",
                                                        ifelse(year == "2016", "2015", "2016")))
blog <- happiness_all %>%
  mutate(year = as.numeric(year)) %>%
  filter(Country %in% c("Algeria", "Afghanistan", "Brazil", "Liberia", "Syria", "Venezuela"))

b <- blog %>%
  ggplot(aes(year, Happiness.Score, color = Country)) +
  geom_point(size = 5, alpha = 0.4) + 
  geom_point(size = 3) + 
  geom_line(aes(group = Country), size = 1.5) + 
  scale_color_kani() + 
  scale_y_continuous(breaks = seq(3,7.5, by = 0.25), limits = c(3,7.25)) +
  # scale_x_continuous(breaks = seq(2014,2016, by = 1), limits = c(2014,2016)) + 
  scale_x_continuous(breaks= seq(2014,2016,by=.2), 
                     labels = c(2014, rep("",4), 2015, rep("",4), 2016), 
                     limits = c(2013.9,2016.1), expand = c(0,0)) +
  theme_kani() + 
  labs(title = "Happiness levels in selected countries from 2014 to 2016", 
       subtitle = "Shows Happiness Scores calculated\nby the World Happiness Report in 2015, 2016 and 2017",
       y = "Happiness Score", x = "Year",
       caption = "By Kanishka Misra\nData Source: Kaggle"
  ) + 
  theme(axis.text.x = element_text(size = 15, family = "Roboto"),
        axis.text.y = element_text(size = 15, family = "Roboto"))
b2 <- b + 
  geom_text(aes(x = 2015, y = 5.7), label = "Presidential Elections and\nnationwide protests", size = 4.5, color = dark_grey, fontface = 2) + 
  geom_text(aes(x = 2015.6, y = 5.15), label = "Largest public demonstration in the\nhistory of Venezuela", size = 4.5, color = dark_grey, fontface = 2) + 
  geom_text(aes(x = 2015.7, y = 7), label = "Rio 2016 hits the\npoor populations", family = "Roboto", color = "#ffc952", fontface = 2, size = 4.5) + 
  geom_text(aes(x = 2014.2, y = 7.25), label = "Brazil football team\nloses 7-1 to Germany", family = "Roboto", color = "#ffc952", fontface = 2, size = 4.5) + 
  geom_text(aes(x = 2015, y = 7.25), label = "WHO announces national\nspread of the Zika Virus", family = "Roboto", color = "#ffc952", fontface = 2, size = 4.5) +
  geom_text(aes(x = 2014.4, y = 4.5), label = "Liberia affected by mass\nspread of Ebola", family = "Roboto", color = green, fontface = 2, size = 4.5) +
  geom_text(aes(x = 2015.1, y = 3.875), label = "U.N. declares Liberia free\nof Ebola", family = "Roboto", color = green, fontface = 2, size = 4.5)
b2
ggsave("countries.png", b2, height = 12, width = 12)

test = happiness_2015 %>% select(-c(Happiness.Rank, Standard.Error, Region, year))
colnames(test)[2] = "Happiness Score"
colnames(test)[3] = "GDP per capita"
colnames(test)[5] = "Life Expectancy"
colnames(test)[7] = "Perceived Corruption in Govt"
colnames(test)[9] = "Dystopia and Residual"
test$`Happiness Score`
