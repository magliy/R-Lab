library(tidyverse)

load('data/ExpImp.Rdata')


ExpImp <- replace(ExpImp, ExpImp == '-', '0.0')
ExpImp[, c(2:13)] <- sapply(ExpImp[, c(2:13)], as.numeric)
ExpImp <- as_tibble(ExpImp)


regions = c('Белгородская область', 'Брянская область', 'Владимирская область', 
            'Воронежская область',	'Ивановская область', 'Калужская область',
            'Костромская область',	'Курская область',	'Липецкая область',
            'г. Москва', 'Московская область',	'Орловская область',
            'Рязанская область', 'Смоленская область', 'Тамбовская область',
            'Тверская область', 'Тульская область', 'Ярославская область')

Imp <- ExpImp %>%
  filter(Регион %in% regions) %>%
  select(c(1, grep('*Импорт', names(ExpImp)))) %>%
  mutate(sumImp = rowSums(across(where(is.numeric)))) %>%
  select(1, last_col())

Exp <- ExpImp %>%
  filter(Регион %in% regions) %>%
  select(c(1, grep('*Экспорт', names(ExpImp)))) %>%
  mutate(sumExp = rowSums(across(where(is.numeric)))) %>%
  select(1, last_col())


ExpImp <- Exp %>%
  left_join(Imp, by = 'Регион') %>%
  mutate(diff = sumExp - sumImp)


ExpImp %>% filter(Регион != 'г. Москва')


ggplot(ExpImp, aes(x = Регион, y = sumExp)) +
  geom_col(aes(fill = "Экспорт"), 
           width = 0.45,
           position = position_nudge(x = -0.225)) +
  geom_col(aes(y = sumImp, fill = "Импорт"), 
           width = 0.45,
           position = position_nudge(x = 0.225)) +
  coord_flip() +
  scale_y_continuous(trans='log10') +
  geom_text(aes(label = paste('Δ:' , diff)), size = 3.5,
            position = position_stack(vjust = 0.5), hjust = 1) +
  labs(title = 'Total import/export values for the Сentral district', 
       x = 'Region', y = 'Value of Import/Export (log scale)', fill = '')
