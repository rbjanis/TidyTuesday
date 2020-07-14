# Week 29
# Astronauts
library(tidyverse)
library(patchwork)
theme_set(theme_minimal())

tt_gh <- tidytuesdayR::tt_load_gh("2020-07-14")
astronauts <- tidytuesdayR::tt_download_file(tt_gh, 1)

# Women by year
# Women by occupation

mutate(astronauts, nationality = fct_infreq(nationality),
       nationality = fct_lump_n(nationality, 10),
       nationality = fct_rev(nationality)) %>%
  ggplot(aes(y = nationality, fill = sex)) +
  geom_bar() +
  labs(y = "Nationality",
       fill = "")

ggplot(astronauts, aes(y = reorder(occupation, desc(occupation)))) +
  geom_bar() +
  labs(y = "Occupation")

ggplot(astronauts, aes(x = year_of_mission)) +
  geom_bar() +
  labs(y = "Year")

ggplot(astronauts, aes(x = year_of_mission, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(y = "Year",
       fill = "Sex")

ages <- mutate(astronauts, mission_age = year_of_mission - year_of_birth, selection_age = year_of_selection - year_of_birth) %>%
  group_by(name) %>%
  summarize(selection_age = min(selection_age), oldest_mission_age = max(mission_age), sex = first(sex), nationality = first(nationality))

ggplot(ages, aes(x = selection_age, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(y = "Age at selection for first mission",
       fill = "Sex")

ggplot(ages, aes(x = oldest_mission_age, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(y = "Age at last mission",
       fill = "Sex")

ggplot(ages, aes(x = oldest_mission_age, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(y = "Age at selection for first mission",
       fill = "Sex")

hours <- group_by(astronauts, name) %>%
  summarize(sex = first(sex),
            nationality = first(nationality),
            total_hrs_sum = first(total_hrs_sum),
            total_eva_hrs = first(total_eva_hrs))

group_by(hours, nationality) %>%
  summarize(total_hrs_sum = sum(total_hrs_sum),
            total_eva_hrs = sum(total_eva_hrs)) %>%
  pivot_longer(-nationality, names_to = "hours_type", values_to = "hours_total") %>%
  ggplot(aes(y = nationality, x = hours_total, fill = hours_type)) +
  geom_col()

group_by(hours, nationality) %>%
  summarize(total_hrs_sum = sum(total_hrs_sum),
            total_eva_hrs = sum(total_eva_hrs)) %>%
  ggplot(aes(y = nationality, x = total_eva_hrs)) +
  geom_col()

group_by(hours, sex) %>%
  summarize(total_hrs_sum = sum(total_hrs_sum),
            total_eva_hrs = sum(total_eva_hrs)) %>%
  pivot_longer(-sex, names_to = "hours_type", values_to = "hours_total") %>%
  ggplot(aes(y = sex, x = hours_total, fill = hours_type)) +
  geom_col()

group_by(hours, sex) %>%
  summarize(total_hrs_sum = sum(total_hrs_sum),
            total_eva_hrs = sum(total_eva_hrs)) %>%
  ggplot(aes(y = sex, x = total_eva_hrs)) +
  geom_col()



# Time in space and and EVA hours by nationality and sex
hours2 <- mutate(hours,
                name = ifelse(sex == "female",
                               glue::glue("<strong><span style='color:mediumvioletred'>{name}</span></strong>"),
                               name))

p1 <- arrange(hours2, -total_hrs_sum) %>%
  slice(1:10) %>%
  mutate(name = fct_reorder(name, total_hrs_sum),
         total_days_sum = total_hrs_sum/24) %>%
  ggplot(aes(x = total_days_sum, y = name, fill = nationality)) +
  geom_col() +
  labs(x = "Total mission time (days)",
       y = "",
       fill = "") +
  scale_fill_manual(values = c("blue3", "red3")) +
  theme(plot.title.position = "plot",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, color = "gray25"),
        axis.text.y = ggtext::element_markdown(size = 14),
        panel.grid = element_line(color = "grey"))

p2 <- arrange(hours2, -total_eva_hrs) %>%
  slice(1:10) %>%
  mutate(name = fct_reorder(name, total_eva_hrs),
         total_eva_days = total_eva_hrs/24) %>%
  ggplot(aes(x = total_eva_days, y = name, fill = nationality)) +
  geom_col() +
  labs(x = "Total extravehicular activity time (days)",
       y = "",
       fill = "") +
  scale_fill_manual(values = c("blue3", "red3")) +
  theme(plot.title.position = "plot",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, color = "gray25"),
        axis.text.y = ggtext::element_markdown(size = 14),
        panel.grid = element_line(color = "grey"))

p1 + p2 + plot_layout(guides="collect") +
  plot_annotation(title = "Top 10 astronauts with the most mission time and extravehicular activity time",
                  subtitle = "<span style='color:red3';>U.S.S.R/Russian astronauts</span> have more time in space, while <span style='color:blue3';>U.S. astronauts</span> have more extravehicular hours.<br>
                  Most of the astronauts in the top 10 are men, but a few <strong><span style='color:mediumvioletred'>women</span></strong> made the list as well.",
                  theme = theme(plot.title = element_text(size = 22, hjust = .5),
                                plot.subtitle = ggtext::element_markdown(size = 14, hjust = .5),
                                plot.background = element_rect(fill = "gray95"),
                                legend.position = "none"))

ggsave(filename = "Week29/top-astronauts.png", width = 14, height = 8)






