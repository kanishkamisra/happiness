setwd("C:/Users/Kanishka/happiness/")
library(tidyverse)
library(kani)
library(extrafont)
library(twidlr)
library(broom)

happiness_2015 <- read.csv("data/2015.csv") %>% mutate(year = "2015")
happiness_2016 <- read.csv("data/2016.csv") %>% mutate(year = "2016")
happiness_2017 <- read.csv("2017.csv") %>% mutate(year = "2017")

happiness_2015_16 <- rbind(happiness_2015 %>% select(-Standard.Error), happiness_2016 %>% select(-c(Lower.Confidence.Interval, Upper.Confidence.Interval)))

happiness_2015_16 %>%
  arrange(year) %>%
  filter(Happiness.Rank <= 10) %>%
  ggplot(aes(Country, Happiness.Score)) +
  geom_col(aes(fill = year), position = "dodge") + 
  # geom_line(aes(group = year, color = year), size = 1.4) + 
  scale_color_kani() + 
  theme_kani() + 
  labs(y = "Happiness Score") + 
  scale_y_continuous(limits = c(4,8))

countries <- happiness_2015 %>%
  select(Country, Region, Happiness.Score) %>%
  inner_join(happiness_2016 %>% select(Country, Happiness.Score), by = "Country") %>%
  mutate(score_2015 = Happiness.Score.x,
         score_2016 = Happiness.Score.y,
         difference = score_2016 - score_2015) %>%
  select(-c(Happiness.Score.x, Happiness.Score.y))

blue = "#30a2da"
red <- "#fc4f30"
red <- "#EE7785"
green <- "#67d5b5"
green <- "#77ab43"
light_red = "#f8766d"
light_blue = "#00bfc4"

c <- countries %>%
  ggplot(aes(difference)) +
  geom_density(size = 1.2, color = blue) + 
  facet_wrap(~Region) + 
  geom_hline(yintercept = 0, size = 1) + 
  geom_vline(xintercept = 0, linetype = 2) +
  annotate("text", x = -0.5, y = 3, label = "bold('Generally\nLess Happy')", size = 4, parse = T, color = "#EE7785") + 
  annotate("text", x = 0.6, y = 3, label = "bold('Generally\nMore Happy')", size = 4, parse = T, color = "#67d5b5") + 
  theme_kani() + 
  labs(title = "Happiness Difference from 2015 in 2016 by Region")
stuff <- data.frame(ggplot_build(c)$data[[1]])
c

ggsave("plot_test1.png", c, height = 11, width = 22)


regional_differences <- countries %>%
  group_by(Region) %>%
  summarise(
    mean_diff = mean(difference),
    median_diff = median(difference)
  ) %>%
  mutate(
    median_change = ifelse(median_diff > 0, "Happier", "Less Happier"),
    mean_change = ifelse(mean_diff > 0, "Happier", "Less Happier")
         )

morestuff <- countries %>%
  filter(Region == "Central and Eastern Europe")

sdev <- sd(morestuff$difference)
pi
1/((sqrt(2*pi)) * sdev)

regional_differences <- regional_differences %>%
  mutate(Region = gsub(" and ", "\nand ", Region))

regional_differences %>%
  arrange(median_diff) %>%
  mutate(Region = reorder(Region, median_diff)) %>%
  ggplot(aes(Region, median_diff, color = median_change)) + 
  geom_segment(aes(x = Region, xend = Region, y = 0, yend = median_diff), size = 2, alpha = 0.8) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, lty = 1, size = 1) + 
  coord_flip() +
  scale_color_kani() +
  theme_kani() + 
  labs(title = "Regional Change in Happiness from 2015 to 2016")


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
  geom_text(aes(x = "Sudan", y = 0.5), label = "Happier in 2016 than\nthey were in 2015", color = light_blue, size = 5, family = "Roboto", fontface = 2) + 
  geom_text(aes(x = "Hungary", y = -0.5), label = "Sadder in 2016 than\nthey were in 2015", color = light_red, size = 5, family = "Roboto", fontface = 2) + 
  coord_flip() +
  scale_color_manual(values = c(light_blue, light_red)) +
  scale_y_continuous(breaks = seq(-1,1.25, by = 0.25), limits = c(-1,1)) + 
  theme_kani() + 
  labs(title = "Top 10 happier/sadder countries from 2015 to 2016", 
       subtitle = "Shows difference in Happiness Scores calculated\nby the World Happiness Report in 2015 and 2016",
       y = "Difference in Happiness Score",
       caption = "By Kanishka Misra\nData Source: Kaggle"
      ) + 
  theme(legend.position = "None")
top_10_plot
ggsave("plot_test2.png", top_10_plot, height = 9, width = 15)



nishi <- top_10 %>% inner_join(happiness_2016 %>% select(Country, Dystopia.Residual))
