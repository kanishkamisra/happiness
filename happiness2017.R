setwd("C:/Users/Kanishka/happiness/")
library(tidyverse)
library(kani)
library(extrafont)
library(twidlr)
library(broom)

happiness_2015 <- read.csv("data/2015.csv") %>% mutate(year = "2015")
happiness_2016 <- read.csv("data/2016.csv") %>% mutate(year = "2016")
happiness_2017 <- read.csv("2017.csv") %>% mutate(year = "2017")

countries <- happiness_2017 %>%
  select(Country, Happiness.Score) %>%
  inner_join(happiness_2016 %>% select(Country, Happiness.Score), by = "Country") %>%
  mutate(score_2017 = Happiness.Score.x,
         score_2016 = Happiness.Score.y,
         difference = score_2017 - score_2016) %>%
  select(-c(Happiness.Score.x, Happiness.Score.y))

