library(tidyverse)
load('data/trades.RData')

trades <- bind_rows(trades)

import <- trades %>%
  filter(indic_et == 'Imports in million of ECU/EURO') %>%
  select(!c(indic_et, geo))

export <- trades %>%
  filter(indic_et == 'Exports in million of ECU/EURO') %>%
  select(!c(indic_et, geo))


export <- export %>%
  group_by(sitc06, time) %>%
  summarise(values = sum(values))

ggplot(data=export, aes(x = time, y = values, group = sitc06)) +
  geom_line(aes(color = sitc06)) +
  geom_point(aes(color = sitc06)) +
  labs(title = 'Сhanges in export values from 2008 to 2019',
       x = 'Years', 
       y = 'Sum values of export',
       color = 'Categories') + 
  theme_classic()

