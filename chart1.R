incar_trend <- read.csv("data/incarceration_trends.csv")

library(tidyverse)
library(dplyr)
library(ggplot2)

race_incar_ratio <- incar_trend %>% 
  filter(year >= 1990) %>%
  group_by(year) %>%
  summarise(aapi_prop = sum(aapi_jail_pop, na.rm=TRUE)/sum(aapi_pop_15to64, na.rm=TRUE),
            black_prop = sum(black_jail_pop, na.rm=TRUE)/sum(black_pop_15to64, na.rm=TRUE),
            latinx_prop = sum(latinx_jail_pop, na.rm=TRUE)/sum(latinx_pop_15to64, na.rm=TRUE),
            native_prop = sum(native_jail_pop, na.rm=TRUE)/sum(native_pop_15to64, na.rm=TRUE),
            white_prop = sum(white_jail_pop, na.rm=TRUE)/sum(white_pop_15to64, na.rm=TRUE)) %>%
  gather(key = "variable", value = "value", -year)

chart1 <- ggplot(race_incar_ratio, aes(x = year, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  xlab("Year(1900 to 2018)") +
  ylab("Average Ratio of Incarceration") +
  ggtitle("Average Ratio of Incarceration for each race from 1990 to 2018") +
  theme(plot.title = element_text(size = 12,face = "bold"))