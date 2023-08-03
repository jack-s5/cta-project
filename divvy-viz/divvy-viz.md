# Divvy vs. Taxi

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
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = here("data", "duckdb", "divvy.db"))

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
    roll_rides = rollmean(rides, 15, fill = NA, align = c("right"))
  ) %>% 
  filter(!is.na(roll_rides))

# clean
rm(old_clean, new_clean)
DBI::dbDisconnect(con)
```

``` r
ggplot(divvy_rolling_rides, 
       aes(x = date, y = rides)) + 
  # daily riders line
  geom_line(
    color = "white",
    alpha = 0.2
  ) +
  # daily riders area
  geom_area(
    fill = "white",
    alpha = 0.1
  ) +
  # rolling riders line
  geom_line(
    aes(x = date, y = roll_rides),
    color = "#41b6e6",
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  
  labs(
    title = "Divvy Ridership Exploded Following Expansions and the\nIntroduction of E-Bikes",
    x = "Date",
    y = "Riders"
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
    plot.background = element_rect(color = "gray10",
                                   fill = "gray10"),
    
    panel.grid.major = element_line(color = "gray90",
                              linewidth = 0.25),
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(family = "Space Grotesk",
                              face = "bold",
                              color = "white",
                              size = 18),
    plot.title.position = "plot",
    axis.title = element_text(family = "Space Grotesk",
                              face = "bold",
                              color = "white"),
    axis.text = element_text(family = "Space Grotesk",
                             color = "gray90"),
    
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(5, 5, 5, 5)
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

![](divvy-viz_files/figure-commonmark/Plot%20Monthly%20Ridership-1.png)

``` r
# ggsave(
#   here("divvy-viz", "plots", "divvy-ridership-timeseries.png"), 
#   width = 8, 
#   height = 5, 
#   units = "in", 
#   dpi = 300
#  )
```
