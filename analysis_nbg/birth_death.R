### --- BIRTH DEATH MAPS ---
# ChatGPT usage:
# 1. to get and use world shapefiles (Natural Earth: "rnaturalearth"/"rnaturalearthdata" libraries)
# 2. parsing longitutde/latitude
# 3. fixing same birth/death locations not being drawn (by jitter)

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# ------------
## Birth Death Map World
world <- ne_countries(scale = "medium", returnclass = "sf")

locations <- read_csv("tombstone_locations.csv") %>%
  separate(birth_city_lat_long, into = c("birth_lat", "birth_long"), sep = ",\\s*", convert = TRUE) %>%
  separate(death_city_latitude_longitude, into = c("death_lat", "death_long"), sep = ",\\s*", convert = TRUE)

# Fix same birth/death locations not drawn with jitter
jitter_coords <- function(coord) coord + runif(length(coord), -0.1, 0.1)

birth_death_lines <- locations %>%
  filter(!is.na(birth_lat), !is.na(birth_long),
         !is.na(death_lat), !is.na(death_long)) %>%
  mutate(
    same_point = birth_lat == death_lat & birth_long == death_long,
    death_lat = if_else(same_point, jitter_coords(death_lat), death_lat),
    death_long = if_else(same_point, jitter_coords(death_long), death_long)
  ) %>%
  select(birth_long, birth_lat, death_long, death_lat)

# Migration map
world_migration_plot <- ggplot() +
  geom_sf(data = world, fill = "#efefef", color = "white") +
  geom_curve(data = birth_death_lines,
             aes(x = birth_long, y = birth_lat,
                 xend = death_long, yend = death_lat),
             curvature = 0.1, color = "red", alpha = 0.3, size = 0.3,
             arrow = arrow(length = unit(0.15, "cm"), type = "closed")
  ) +
  theme_void() +
  labs(
    title = "North Burial Grounds Final Resting Place of Individuals All Over",
    subtitle = "Map of Birth to Death for NBG Tombstones from 17th-20th centuries",
    caption = "Source: 1994-2000 Survey of North Burial Grounds By Sterling; only known birth and death locations shown"
    )

world_migration_plot

# ------------
## Birth Death Map US Only

states <- st_read("states") %>%
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico")))
statesCartogram <- st_transform(states, crs = 5070)

locations_us <- locations

birth_death_us_lines <- locations %>%
  filter(birth_country == "United States of America",
         death_country == "United States of America") %>%
  filter(!is.na(birth_lat), !is.na(birth_long),
         !is.na(death_lat), !is.na(death_long)) %>%
  mutate(
    same_point = birth_lat == death_lat & birth_long == death_long,
    death_lat = if_else(same_point, jitter_coords(death_lat), death_lat),
    death_long = if_else(same_point, jitter_coords(death_long), death_long)
  )

# Coords to CRS 5070
birth_pts <- st_as_sf(birth_death_us_lines, coords = c("birth_long", "birth_lat"), crs = 4326) %>%
  st_transform(crs = 5070) %>%
  st_coordinates()
death_pts <- st_as_sf(birth_death_us_lines, coords = c("death_long", "death_lat"), crs = 4326) %>%
  st_transform(crs = 5070) %>%
  st_coordinates()

birth_death_us_proj <- birth_death_us_lines %>%
  mutate(birth_x = birth_pts[,1], birth_y = birth_pts[,2],
         death_x = death_pts[,1], death_y = death_pts[,2])

# Migration map
us_migration_plot <- ggplot() +
  geom_sf(data = statesCartogram, fill = "#efefef", color = "white") +
  geom_curve(data = birth_death_us_proj,
             aes(x = birth_x, y = birth_y,
                 xend = death_x, yend = death_y),
             curvature = 0.1, color = "red", alpha = 0.3, size = 0.3,
             arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  coord_sf(crs = st_crs("EPSG:5070")) +
  theme_void() +
  labs(
    title = "North Burial Grounds Final Resting Place of Individuals All Over",
    subtitle = "Map of Birth to Death for NBG Tombstones from 17th-20th centuries (US Only)",
    caption = "Dataset from 1994-2000 North Burial Grounds Survey; only known birth and death locations shown"
    )

us_migration_plot


# ------------
## Birth Points Map World 
birth_world_pts <- locations %>%
  filter(!is.na(birth_lat), !is.na(birth_long))

birth_world_plot <- ggplot() +
  geom_sf(data = world, fill = "#efefef", color = "white") +
  geom_point(data = birth_world_pts, aes(x = birth_long, y = birth_lat),
             color = "red", alpha = 0.6, size = 1) +
  theme_void() +
  labs(
    title = "People Born Far Away Came to Rest at Providence",
    subtitle = "Birth locations of individuals buried at Providence's North Burial Grounds",
    caption = "Dataset from 1994–2000 North Burial Grounds Survey; only known birth locations shown"
  )

birth_world_plot

# ------------
## Death Points Map World 
death_world_pts <- locations %>%
  filter(!is.na(death_lat), !is.na(death_long))

death_world_plot <- ggplot() +
  geom_sf(data = world, fill = "#efefef", color = "white") +
  geom_point(data = death_world_pts, aes(x = death_long, y = death_lat),
             color = "red", alpha = 0.6, size = 1) +
  theme_void() +
  labs(
    title = "People Who Died All Over Buried in Providence",
    subtitle = "Death locations of individuals buried at Providence's North Burial Grounds",
    caption = "Dataset from 1994–2000 North Burial Grounds Survey; only known death locations shown"
  )

death_world_plot



