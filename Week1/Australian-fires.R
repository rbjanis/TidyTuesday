# Week 1: Australian Fires

require(tidyverse)
theme_set(theme_classic())
fires <- tidytuesdayR::tt_load(2020, week = 2)

rainfall <-  fires$rainfall
temp <- fires$temperature

temp <- mutate(temp, year_month = format(date, "%Y-%m"),
               year = as.numeric(format(date, "%Y")))

filter(temp, temp_type == "max") %>%
  filter(year >= 1919) %>%
  group_by(year, city_name) %>%
  summarize(temperature = mean(temperature, na.rm = T)) %>%
  ggplot(aes(x = year, y = temperature, group = city_name, color = city_name)) +
  geom_line() +
  geom_smooth(method = "lm", se = F, size = 1.2) +
  scale_x_continuous(name = "Year", breaks = seq(1910, 2020, 10)) +
  scale_y_continuous(name = "High Temperature (Celsius)", breaks = seq(14, 30, 2)) +
  scale_color_discrete(name = "City") +
  ggtitle("Average high temperatures in Australian cites are increasing") +
  labs(subtitle = "1919-2019") +
  bbplot::bbc_style() +

ggsave("Week1/high-temperatures.png")

