library(xlsx)
library(dplyr)

happiness_2017 <- read.xlsx("data/2017_data.xlsx", sheetIndex = 3)

happiness_2017 <- happiness_2017 %>%
  select(-c(NA., NA..1))
colnames(happiness_2017)[2] <- "Happiness.Score"
colnames(happiness_2017)[5] <- "Economy..GDP.per.Capita."
colnames(happiness_2017)[6] <- "Family"
colnames(happiness_2017)[7] <- "Health..Life.Expectancy."
colnames(happiness_2017)[8] <- "Freedom"
colnames(happiness_2017)[9] <- "Generosity"
colnames(happiness_2017)[10] <- "Trust..Government.Corruption."
colnames(happiness_2017)[11] <- "Dystopia.Residual"

happiness_2017 <- happiness_2017 %>% mutate(Happiness.Rank = row_number()) %>%
  select(Country, Happiness.Rank, Happiness.Score, Whisker.high:Dystopia.Residual)

write.csv(happiness_2017, "2017.csv", row.names = F)

test <- happiness_2017 %>%
  mutate(kanis_score = Economy..GDP.per.Capita. + Family + Health..Life.Expectancy. + Freedom + Generosity + Trust..Government.Corruption. + Dystopia.Residual) %>%
  select(Country, Happiness.Rank, Happiness.Score, kanis_score)
