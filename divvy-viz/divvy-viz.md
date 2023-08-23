# Divvy Viz

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ✔ purrr     1.0.1     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(here)
```

    here() starts at /Users/jacksanderson/Documents/R/chicago-transit-project

``` r
library(zoo)
```


    Attaching package: 'zoo'

    The following objects are masked from 'package:base':

        as.Date, as.Date.numeric

``` r
library(sf)
```

    Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE

``` r
set.seed(20040808)
```

``` r
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = here("data", "duckdb", "divvy.db"))
```

``` r
new_dates <- tbl(con, "new_divvy_trips") %>% 
  select(started_at) %>% 
  group_by(year(started_at), month(started_at), day(started_at)) %>% 
  count() %>% 
  collect()

old_dates <- tbl(con, "old_divvy_trips") %>% 
  select(start_datetime) %>% 
  group_by(year(start_datetime), month(start_datetime), day(start_datetime)) %>% 
  count() %>% 
  collect()


old_clean <- old_dates %>% 
  rename(
    year = "year(start_datetime)",
    month = "month(start_datetime)",
    day = "day(start_datetime)",
    rides = n
  ) %>% 
  mutate(date = make_date(year, month, day)) %>% 
  ungroup() %>% 
  select(date, rides)

new_clean <- new_dates %>% 
  rename(
    year = "year(started_at)",
    month = "month(started_at)",
    day = "day(started_at)",
    rides = n
  ) %>% 
  mutate(date = make_date(year, month, day)) %>% 
  ungroup() %>% 
  select(date, rides)

divvy_rides <- bind_rows(old_clean, new_clean) %>% 
  arrange(date)

divvy_rolling_rides <- divvy_rides %>% 
  mutate(
    roll_rides = rollmean(rides, 14, fill = NA, align = c("right"))
  ) %>% 
  filter(!is.na(roll_rides))

# clean
rm(old_clean, new_clean)
```

``` r
theme_set(
  theme_minimal() +
  theme(
    panel.background = element_rect(color = "gray10",
                                    fill = "gray10"),
    plot.background = element_rect(color = "gray10",
                                   fill = "gray10"),
    
    plot.title = element_text(family = "Space Grotesk",
                              face = "bold",
                              color = "white",
                              size = 18),
    plot.title.position = "plot",
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.title = element_blank(),
    axis.text = element_blank(),
    
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    
    plot.margin = margin(5, 5, 5, 5)
  )
)
```

``` r
ggplot(divvy_rolling_rides, 
       aes(x = date, y = rides)) + 
  # daily riders line
  geom_line(
    aes(color = "white", 
        alpha = 0.2)
  ) +
  # daily riders area
  geom_area(
    fill = "white",
    alpha = 0.1
  ) +
  # rolling riders line
  geom_line(
    aes(x = date, y = roll_rides, 
        color = "#41b6e6",
        alpha = 1),
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  
  # legend
  scale_color_identity(
    guide = "legend",
    name = "",
    breaks = c("white", "#41b6e6"),
    labels = c("Daily Riders", "Two-Week Moving Average")
  ) +
  scale_alpha_identity(
    guide = "legend",
    name = "",
    breaks = c(0.2, 1),
    labels = c("Daily Riders", "Two-Week Moving Average")
  ) +
  
  labs(
    title = "Divvy Ridership Exploded Following Expansions and the\nIntroduction of E-Bikes",
    x = "Date",
    y = "Daily Riders"
  ) +
  
  coord_cartesian(
    xlim = c(min(divvy_rolling_rides$date), max(divvy_rolling_rides$date)),
    ylim = c(0, max(divvy_rolling_rides$rides) + 2450)
  ) +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = c(0,0)
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale()), # turns into "1M", "2M", etc
    expand = c(0,0) # forces start at origin
  ) +
  scale_fill_viridis_c() +
  
  theme(
    panel.background = element_rect(color = "gray90",
                                    linewidth = 0.5,
                                    fill = "gray10"),
    
    panel.grid.major = element_line(color = "gray90",
                              linewidth = 0.25),
    
    axis.title = element_text(family = "Space Grotesk",
                              face = "bold",
                              color = "white"),
    axis.text = element_text(family = "Space Grotesk",
                             color = "gray90"),
    
    legend.background = element_blank(),
    legend.text = element_text(family = "Space Grotesk",
                               color = "white"),
    legend.key = element_blank(),
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, 0, 0, 0),
  ) +
  
  # phase 1 expansion / ebikes
  geom_vline(
    xintercept = as_date("2020-07-29"),
    linetype = "dashed",
    color = "#FFB07F",
  ) +
  annotate(
    geom = "curve",
    x = as_date("2019-02-01"),
    xend = as_date("2020-07-01"),
    y = 26000,
    yend = 31000,
    arrow = arrow(length = unit(0.08, "inch")),
    curvature = 0.1,
    color = "#FFB07F"
  ) +
  annotate(
    geom = "label",
    x = as_date("2017-07-05"),
    y = 26000,
    family = "Space Grotesk",
    label = "South Side Expansion Begins,\nE-Bikes Introduced",
    size = 3.2,
    fill = "gray10",
    color = "#FFB07F",
    label.r = unit(0, "lines")
  ) +
  
  # phase 3 expansion / whole city
  geom_vline(
    xintercept = as_date("2023-05-02"),
    linetype = "dashed",
    color = "#F11A7B"
  ) +
  annotate(
    geom = "curve",
    x = as_date("2019-06-15"),
    xend = as_date("2023-04-05"),
    y = 36000,
    yend = 38000,
    arrow = arrow(length = unit(0.08, "inch")),
    curvature = 0.08,
    color = "#F11A7B"
  ) +
  annotate(
    geom = "label",
    x = as_date("2018-01-01"),
    y = 36000,
    family = "Space Grotesk",
    label = "Divvy Service Introduced to\nEntire City",
    size = 3.2,
    fill = "gray10",
    color = "#F11A7B",
    label.r = unit(0, "lines")
  )
```

![](divvy-viz_files/figure-commonmark/Rolling%20Ridership%20Plot-1.png)

``` r
# ggsave(
#   here("divvy-viz", "plots", "divvy-ridership-timeseries.png"),
#   width = 8,
#   height = 5,
#   units = "in",
#   dpi = 300
#  )
```

``` r
coordinate_counts <- tbl(con, "new_divvy_trips") %>% 
  filter(
    rideable_type == "electric_bike",
     start_lng < -80
  ) %>% 
  select(started_at, start_lng, start_lat) %>% 
  slice_sample(n = 100000) %>% 
  collect()

coordinate_sf <- st_as_sf(coordinate_counts, coords = c("start_lng", "start_lat"), crs = "longlat")

DBI::dbDisconnect(con, shutdown = TRUE)
rm(con)
```

``` r
chicago_communities <- read_sf(here::here("data", "SHP_chicago-communities", "geo_export_e07d67fa-91ce-4d30-9da3-eb903021731c.shp"))

st_crs(chicago_communities) <- st_crs(coordinate_sf)

ebike_locations <- ggplot(chicago_communities) + 
  geom_sf(
    color = "black",
    linewidth = 0.5,
    fill = "gray35"
  ) +
  geom_sf(
    data = coordinate_sf,
    color = "#41b6e6",
    size = 0.5,
    alpha = 0.025
  ) + 
  labs(
    title = "E-Bike Ridership is Largely Concentrated\non the North Side and in Downtown"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

cowplot::ggdraw(ebike_locations) +
  theme(plot.background = element_rect(fill = "gray10", color = "gray10"))
```

![](divvy-viz_files/figure-commonmark/E-Bike%20Start%20Points%20Graph-1.png)
