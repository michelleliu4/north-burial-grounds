### --- CONDITION & LEGIBILITY BY COMPOSITION ---
require(tidyverse)

nbg <- read_csv("tombstone_conditions.csv") %>%
  na.omit() # remove na

nbg_clean <- nbg %>%
  filter(!is.na(section), !is.na(condition), !is.na(composition)) %>%
  add_count(composition, name = "comp_count") %>%
  mutate(composition = if_else(comp_count < 50, "other", composition)) %>%
  filter(!is.na(section), !is.na(condition), !is.na(composition))

nbg_clean_legibility <- nbg %>%
  filter(!is.na(section), !is.na(legibility), !is.na(composition)) %>%
  add_count(composition, name = "comp_count") %>%
  mutate(composition = if_else(comp_count < 50, "other", composition)) %>%
  filter(!is.na(section), !is.na(legibility), !is.na(composition))

# Sort % poor + fair (condition)
composition_order_condition <- nbg_clean %>%
  group_by(composition) %>%
  summarize(poor_fair_pct = sum(condition == "poor" | condition == "fair") / n()) %>%
  arrange(desc(poor_fair_pct))
nbg_condition <- nbg_clean %>%
  mutate(
    composition = factor(composition, levels = composition_order_condition$composition),
    condition = factor(condition, levels = c("good", "fair", "poor"))
  )

# Sort % poor + fair (legibility)
composition_order_legibility <- nbg_clean_legibility %>%
  group_by(composition) %>%
  summarize(poor_fair_pct = sum(legibility == "poor" | legibility == "fair") / n()) %>%
  arrange(desc(poor_fair_pct))
nbg_legibility <- nbg_clean_legibility %>%
  mutate(
    composition = factor(composition, levels = composition_order_legibility$composition),
    legibility = factor(legibility, levels = c("good", "fair", "poor"))
  )

# Condition plot
condition_plot <- ggplot(nbg_condition, aes(x = composition, fill = condition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("good" = "#E0E0E0", "fair" = "#E69F00", "poor" = "#B22222")) +
  labs(fill = "Condition",
       title = "Fieldstone and Sandstone Tombs in Worst Shape",
       subtitle = "Conditions of tombstones in Proivdence's North Burial Grounds",
       caption = "Source: 1994-2000 Survey of North Burial Grounds By Sterling; stone types with under 50 samples each grouped into 'other'"
       ) +
  theme_minimal()

# Legibility plot
legibility_plot <- ggplot(nbg_legibility, aes(x = composition, fill = legibility)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("good" = "#E0E0E0", "fair" = "#E69F00", "poor" = "#B22222")) +
  labs(fill = "Legibility",
       title = "North Burial Ground's Marble and Fieldstone Tombs Suffer Legibility Issues",
       subtitle = "Legibility of tombstones in Proivdence's North Burial Grounds",
       caption = "Dataset from 1994-2000 North Burial Grounds Survey; stone types with under 50 samples each grouped into 'other'"
       ) +
  theme_minimal()

condition_plot
legibility_plot
