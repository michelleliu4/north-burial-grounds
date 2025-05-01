### --- MEDIAN YEAR OF INSTALLATION BY SECTION ---
library(tidyverse)

tombstone_year <- read_csv("tombstone_conditions.csv") %>%
  filter(
    !is.na(section),
    !is.na(estimated_installation_year),
    estimated_installation_year > 1600
  )

# Compute stats and order sections by median
stats <- tombstone_year %>%
  group_by(section) %>%
  summarize(
    min_year    = min(estimated_installation_year),
    median_year = median(estimated_installation_year),
    max_year    = max(estimated_installation_year)
  ) %>%
  arrange(-1* median_year) %>%
  mutate(section = factor(section, levels = section))

# Plot min to max plus median
ggplot(stats, aes(x = section)) +
  geom_linerange(aes(ymin = min_year, ymax = max_year), size = 1) +
  geom_point(aes(y = median_year), size = 7, color = "#B22222") +
  geom_text(aes(y = median_year, label = section), vjust = -1, hjust = 0.5, size = 3) +
  
  geom_text(aes(y = min_year, label = min_year), vjust = 0.5, size = 3) +
  geom_text(aes(y = max_year, label = max_year), vjust = 0.5, size = 3) +
  
  coord_flip() +
  
  scale_y_continuous(
    breaks = seq(1650, 2000, by = 50),
    limits = c(1650, 2000)
  ) +
  
  labs(
    x = "",
    y = "Installation Year",
    title = "Many Tombstones Older Than the United States",
    subtitle = "Min, Median, and Max Installation Years by Section",
    caption = "Source: 1994-2000 Survey of North Burial Grounds by Sterling"
  ) +
  theme_minimal()
