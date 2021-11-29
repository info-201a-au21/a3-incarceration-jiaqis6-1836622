incar_trend <- read.csv("data/incarceration_trends.csv")

library(tidyverse)
library(dplyr)
library(ggplot2)

color_prop <- incar_trend %>% 
  filter(year == 2018) %>%
  group_by(state) %>%
  summarise(
    color_pop_prop = 1 - sum(white_pop_15to64, na.rm=TRUE)/sum(total_pop_15to64, na.rm=TRUE),
    color_jail_prop = 1 - sum(white_jail_pop, na.rm=TRUE)/sum(total_jail_pop, na.rm=TRUE)
  ) %>%
  mutate_all(~replace(., is.na(.), 0))

chart2 <- ggplot(color_prop) +
  geom_point(mapping = aes(x = color_pop_prop, y = color_jail_prop)) +
  xlab("The Proportion of People of Color in Local Residence") +
  ylab("The Proportion of People of Color in Jail") +
  ggtitle("The relation because local residence proportion and incarceration proportion")