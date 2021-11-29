library(tidyverse)
library(dplyr)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(maps)
library(mapproj)
library(patchwork)

incar_trend <- read.csv("data/incarceration_trends.csv")

prop_by_state <- incar_trend %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarise(
    total_color = sum(aapi_pop_15to64, black_pop_15to64, native_pop_15to64, latinx_pop_15to64, na.rm=TRUE),
    total_color_jail = sum(aapi_jail_pop, black_jail_pop, native_jail_pop, latinx_jail_pop, na.rm=TRUE),
    prop_by_state = total_color_jail/total_color
  ) %>%
  filter(state != "DC") %>%
  mutate(state = state.name[match(state, state.abb)]) %>%
  mutate(state = tolower(state)) %>%
  select(state, prop_by_state)


us_map_data <- map_data("state") %>%
  rename(state = region) %>%
  left_join(prop_by_state, by = "state")


blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remosve major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank(),     # remove border around plot
  ) 


chart3 <- ggplot(us_map_data) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = prop_by_state)) +
  coord_map() +
  scale_fill_continuous(low = "#132B43") +
  labs(fill = "Incarceration ratio of people of color") + 
  blank_theme +
  ggtitle("Incarceration proportion of people of color in 2018")
