### --- DETERIORATION SLOPE CHART & PLOTTING ---
require(tidyverse)

survey <- read_csv("new_survey.csv") %>%
  na.omit()  

# Filter to only stones with both old/new assessments, count & group small types
survey_clean <- survey %>%
  filter(!is.na(comp), !is.na(cond_sterling), !is.na(cond_rivera)) %>%
  add_count(comp, name = "comp_count") %>%
  mutate(comp = if_else(comp_count < 50, "other", comp))

# Long and relabel surveys
survey_long <- survey_clean %>%
  pivot_longer(
    cols         = c(cond_sterling, cond_rivera),
    names_to     = "survey",
    values_to    = "condition",
    names_prefix = "cond_"
  ) %>%
  mutate(
    survey = factor(
      survey,
      levels = c("sterling", "rivera"),
      labels = c("Older survey\n(1994–2000)", "Newer survey\n(2023–25)")
    )
  )

# Compute % fair+poor by comp & survey
summary_df <- survey_long %>%
  group_by(comp, survey) %>%
  summarize(
    pct_fp = sum(condition %in% c("fair", "poor")) / n() * 100,
    .groups = "drop"
  )

# Plot
slope_plot <- ggplot(summary_df, aes(x = survey, y = pct_fp, group = comp, color = comp)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_y_continuous(
    limits = c(0, NA),
    labels = scales::percent_format(scale = 1)
  ) +
  labs(
    x        = NULL,
    y        = "% Fair + Poor",
    color    = "Stone type",
    title    = "Change in % Fair + Poor Over Time (Section AA)",
    caption  = "Older: Sterling (1994–2000); Newer: Rivera (2023–25); stone types with <50 grouped into 'other'"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

slope_plot
