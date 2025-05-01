### --- CONDITION & LEGIBILITY BY COMPOSITION ---
require(tidyverse)

nbg <- read_csv("tombstone_conditions.csv")

nbg_clean <- nbg %>%
  filter(!is.na(estimated_installation_year), !is.na(composition)) %>%
  add_count(composition, name = "comp_count") %>%
  mutate(
    composition = if_else(comp_count < 50, "other", composition),
    decade = if_else(estimated_installation_year < 1750, 1740,
                     floor(estimated_installation_year / 10) * 10)
  )

composition_by_decade <- nbg_clean %>%
  group_by(decade, composition) %>%
  summarise(count = n()) %>%
  select(decade, composition, count) %>%
  group_by(decade) %>%
  mutate(percent = count / sum(count) * 100)

# Composition by decade plot
composition_plot <- ggplot(composition_by_decade, aes(x = decade, y = percent, color = composition)) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks = seq(min(composition_by_decade$decade), max(composition_by_decade$decade), by = 20)) +
  labs(
    title = "Marble Tombstones Peaked in Popularity in the 1840's and Have Declined Since",
    subtitle = "Percentage of Tombstones Stone Types Installed at North Burial Grounds by Decade",
    caption = "Source: 1994-2000 Survey of North Burial Grounds by Sterling; NOTE THAT 1740 ACTUALLY REFERS TO 1600-1749 AGGREGATED",
    x = "Decade",
    y = "Percent of Tombstones",
    color = "Stone Type"
  ) +
  theme_minimal()

composition_plot
