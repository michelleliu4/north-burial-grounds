### --- CONDITION BY SECTION ---
require(tidyverse)

nbg <- read_csv("tombstone_conditions.csv") %>%
  na.omit() # remove na

nbg_clean <- nbg %>%
  filter(!is.na(section), !is.na(condition))

# Sort % poor + fair per section
section_order <- nbg_clean %>%
  group_by(section) %>%
  summarize(poor_fair_pct = sum(condition == "poor" | condition == "fair") / n()) %>%
  arrange(desc(poor_fair_pct))
nbg_clean <- nbg_clean %>% # sort by "poor" condition; stack good on bottom
  mutate(
    section = factor(section, levels = section_order$section),
    condition = factor(condition, levels = c("good", "fair", "poor")))

condition_section_plot <- ggplot(nbg_clean, aes(x = section, fill = condition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("good" = "#E0E0E0", "fair" = "#E69F00", "poor" = "#B22222")) +
  labs(x = "Section",
       fill = "Condition",
       title = "Sections FG and AK Need the Most Attention",
       subtitle = "Conditions of tombstones in Providence's North Burial Grounds",
       caption = "Source: 1994-2000 Survey of North Burial Grounds by Sterling") +
  theme_minimal()

condition_section_plot