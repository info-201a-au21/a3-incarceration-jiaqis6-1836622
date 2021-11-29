incar_trend <- read.csv("data/incarceration_trends.csv")

library(tidyverse)
library(dplyr)

summaryl <- list()

# What is the average proportion of people of color in 2018?
summaryl$avg_color_prop_2018 <- incar_trend %>%
  filter(year == 2018) %>%
  summarise(avg_pop_prop = 1 - sum(white_pop_15to64, na.rm=TRUE)/sum(total_pop_15to64, na.rm=TRUE)) %>%
  pull(avg_pop_prop)


# average value of the proportion of people of color in jails across all the counties in 2018?
summaryl$avg_color_jail_prop_2018 <- incar_trend %>%
  filter(year == 2018) %>%
  summarise(avg_prop = 1 - sum(white_jail_pop, na.rm=TRUE)/sum(total_jail_pop, na.rm=TRUE)) %>%
  pull(avg_prop)


# Which state had the highest proportion of people of color in jails in 2018?
summaryl$highest_in_jail_state <- incar_trend %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarise(jail_prop_2018 = 1 - sum(white_jail_pop, na.rm=TRUE)/sum(total_jail_pop, na.rm=TRUE)) %>%
  filter(jail_prop_2018 == max(jail_prop_2018, na.rm=TRUE)) %>%
  pull(state)


# What was the highest proportion of people of color in jails of all states in 2018?
summaryl$highest_in_jail <- incar_trend %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarise(jail_prop_2018 = 1 - sum(white_jail_pop, na.rm=TRUE)/sum(total_jail_pop, na.rm=TRUE)) %>%
  filter(jail_prop_2018 == max(jail_prop_2018, na.rm=TRUE)) %>%
  pull(jail_prop_2018)


# What was the lowest proportion of people of color in jails of all states in 2018?
summaryl$lowest_in_jail <- incar_trend %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarise(jail_prop_2018 = 1 - sum(white_jail_pop, na.rm=TRUE)/sum(total_jail_pop, na.rm=TRUE)) %>%
  filter(jail_prop_2018 == min(jail_prop_2018, na.rm=TRUE)) %>%
  pull(jail_prop_2018)


# Which state had the lowest proportion of people of color in jails of all states in 2018?
summaryl$lowest_in_jail_state <- incar_trend %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarise(jail_prop_2018 = 1 - sum(white_jail_pop, na.rm=TRUE)/sum(total_jail_pop, na.rm=TRUE)) %>%
  filter(jail_prop_2018 == min(jail_prop_2018, na.rm=TRUE)) %>%
  pull(state)


# Which year were the proportion of people of color in jails highest?
summaryl$highest_in_jail_year <- incar_trend %>%
  filter(year >= 1990) %>%
  group_by(year) %>%
  summarise(jail_prop_color = 1 - sum(white_jail_pop, na.rm=TRUE)/sum(total_jail_pop, na.rm=TRUE)) %>%
  filter(jail_prop_color == max(jail_prop_color, na.rm=TRUE)) %>%
  pull(year)

# What is the average proportion of people of color in 1992?
summaryl$avg_color_prop_1992 <- incar_trend %>%
  filter(year == 1992) %>%
  summarise(avg_pop_prop = 1 - sum(white_pop_15to64, na.rm=TRUE)/sum(total_pop_15to64, na.rm=TRUE)) %>%
  pull(avg_pop_prop)

# average value of the proportion of people of color in jails across all the counties in 1992?
summaryl$avg_color_jail_prop_1992 <- incar_trend %>%
  filter(year == 1992) %>%
  summarise(avg_prop = 1 - sum(white_jail_pop, na.rm=TRUE)/sum(total_jail_pop, na.rm=TRUE)) %>%
  pull(avg_prop)


