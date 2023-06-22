CTA L Ridership Map
================

``` r
library(RSocrata)
library(tidyverse)
library(here)
library(sf)
library(lubridate)
library(cowplot)
```

``` r
monthly_riders <- read.socrata("https://data.cityofchicago.org/resource/t2rn-p8d7.csv")

# to get these KML files I downloaded the KMZ files, changed the
# extension to .zip, and unzipped them.
lines_sf <- read_sf(here("SHP", "CTA_RailLines", "doc.kml")) 
stations_sf <- read_sf(here("SHP", "CTA_RailStations", "doc.kml"))

# test
ggplot(lines_sf) +
  geom_sf() + 
  geom_sf(data = stations_sf)
```

![](cta-ridership_viz_files/figure-commonmark/Importing%20Data-1.png)

``` r
monthly_riders_clean <- monthly_riders %>% 
  rename(station_name = stationame) %>% 
  filter(!station_name %in% c("Randolph/Wabash", "Madison/Wabash", "Washington/State", "Homan")) %>% 
  mutate(station_name = str_replace(station_name, "East 63rd-Cottage Grove", "Cottage Grove")) %>% 
  mutate(station_name = str_replace(station_name, "Dempster-Skokie", "Dempster-Sk")) %>% 
  mutate(station_name = str_replace(station_name, "Oakton-Skokie", "Oakton-Sk")) %>%
  mutate(station_name = str_replace(station_name, "Skokie", "Dempster-Sk")) %>% 
  mutate(station_name = str_replace(station_name, "Dempster-Sk", "Dempster-Skokie")) %>% 
  mutate(station_name = str_replace(station_name, "Oakton-Sk", "Oakton-Skokie"))

# another issue we run into is that stations_sf uses station names that overlap
# so we want to pull the correct station names from monthly_riders_clean
# which we'll then force over the stations_sf
correct_station_names <- as_tibble(unique(monthly_riders_clean$station_name)) %>% 
  transmute(station_name = value) %>% 
  arrange(station_name) %>% 
  mutate(station_id = seq(1:nrow(.)), .before = station_name)
```

``` r
# the L stations geodata has a roosevelt for both the green/orange line stop,
# and the red line stop, but the ridership data doesn't, so I have to purge it
stations_sf_clean <- stations_sf %>%
  rename(station_name = Name) %>% 
  arrange(station_name) %>% 
  mutate(station_id = seq(1:nrow(stations_sf))) %>% 
  filter(station_id != 125)
```

``` r
stations_sf_final <- stations_sf_clean %>% 
  mutate(station_name = correct_station_names$station_name)

stations_sf_riders_2022 <- monthly_riders_clean %>% 
  filter(month_beginning == "2022-10-01") %>% 
  distinct(station_name, .keep_all = TRUE) %>% 
  arrange(station_name) %>% 
  left_join(stations_sf_final, ., by = c("station_name" = "station_name"))
```

``` r
plot_1 <- ggplot(stations_sf_riders_2022) + 
  geom_sf(
    pch = 21,
    color = "black",
    aes(size = monthtotal,
        fill = monthtotal)
  ) +
  geom_sf(
    data = lines_sf,
    color = "white",
    inherit.aes = FALSE
  ) +
  labs(
    title = "CTA 'L' Ridership by Station",
    subtitle = "October 2022",
  ) +
  scale_size_continuous(
    name = "Monthly Riders",
    breaks = c(25000, 50000, 100000, 250000),
    labels = c("25k", "50k", "100k", "250k")
  ) +
  scale_fill_viridis_c(
    name = "Monthly Riders",
    breaks = c(25000, 50000, 100000, 250000),
    labels = c("25k", "50k", "100k", "250k"),
    option = "plasma"
  ) +
  guides(
    fill = "legend",
    size = "legend"
  ) +
  theme_void() +
  theme(
    plot.title.position = "plot", # makes title and subtitle centered relative to plot, not panel
    plot.title = element_text(family = "Space Grotesk Bold",
                              color = "white",
                              size = 25,
                              hjust = 0.5),
    plot.subtitle = element_text(family = "Space Grotesk Bold",
                                 color = "gray50",
                                 size = 20,
                                 hjust = 0.5),
    plot.background = element_rect(fill = "gray10",
                                   color = NA),
    legend.title = element_text(family = "Space Grotesk",
                                color = "white",
                                size = 15),
    legend.text = element_text(family = "Space Grotesk",
                               color = "white",
                               size = 15),
    legend.key.size = unit(0.75, "cm"),
    plot.margin = margin(10, 10, 10, 10)
  )
ggdraw(plot_1) +
  theme(panel.background = element_rect(fill = "gray10", color = NA))
```

![](cta-ridership_viz_files/figure-commonmark/Plotting-1.png)
