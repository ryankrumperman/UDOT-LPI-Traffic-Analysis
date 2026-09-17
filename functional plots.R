library(tidyverse)

setwd("C:/Users/rkrum/Documents/Summer 2026/Consulting Center/UDOT/Averaged Intersections")

source("read_data.R")

for (dataset_name in dataset_names) {
  
  dataset <- get(dataset_name)
  
  # Wait-time columns, excluding wait-time-volume columns
  wait_time_cols <- names(dataset)[
    grepl("^wait_time_", names(dataset)) &
      !grepl("^wait_time_volume_", names(dataset))
  ]
  
  # Wait-time-volume columns
  volume_cols <- names(dataset)[
    grepl("^wait_time_volume_", names(dataset))
  ]
  
  dataset <- dataset |>
    mutate(
      average_wait_time = rowMeans(
        across(all_of(wait_time_cols)),
        na.rm = TRUE
      ),
      average_wait_time_volume = rowMeans(
        across(all_of(volume_cols)),
        na.rm = TRUE
      )
    )
  
  # Replace NaN with 0 if an entire row contains missing values
  dataset$average_wait_time[
    is.nan(dataset$average_wait_time)
  ] <- 0
  
  dataset$average_wait_time_volume[
    is.nan(dataset$average_wait_time_volume)
  ] <- 0
  
  assign(dataset_name, dataset, envir = .GlobalEnv)
}



library(dplyr)
library(purrr)
library(ggplot2)
library(lubridate)

dataset_names <- ls(pattern = "^int_\\d{4}$")

functional_data <- map_dfr(dataset_names, function(dataset_name) {
  
  dat <- get(dataset_name)
  
  wait_time_cols <- names(dat)[
    grepl("^wait_time_", names(dat)) &
      !grepl("^wait_time_volume_", names(dat))
  ]
  
  volume_cols <- names(dat)[
    grepl("^wait_time_volume_", names(dat))
  ]
  
  dat |>
    mutate(
      intersection = sub("^int_", "", dataset_name),
      
      # Average across movements within each 15-minute row
      average_wait_time = rowMeans(
        across(all_of(wait_time_cols)),
        na.rm = TRUE
      ),
      
      average_wait_time_volume = rowMeans(
        across(all_of(volume_cols)),
        na.rm = TRUE
      )
    ) |>
    mutate(
      average_wait_time = if_else(
        is.nan(average_wait_time),
        0,
        average_wait_time
      ),
      average_wait_time_volume = if_else(
        is.nan(average_wait_time_volume),
        0,
        average_wait_time_volume
      ),
      
      # Numeric time-of-day for plotting
      time_decimal =
        hour(timestamp) +
        minute(timestamp) / 60
    ) |>
    select(
      intersection,
      timestamp,
      date,
      time,
      study_day,
      period,
      day_number,
      time_decimal,
      average_wait_time,
      average_wait_time_volume, 
      pedestrian_crossing
    )
})

wait_time_plot <- ggplot(
  functional_data,
  aes(
    x = time_decimal,
    y = average_wait_time,
    group = factor(date),
    color = factor(period)
  )
) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ intersection, scales = "free_y") +
  scale_x_continuous(
    breaks = seq(0, 24, by = 3),
    limits = c(0, 24)
  ) +
  labs(
    title = "Averaged Wait-Time Functional Curves",
    x = "Time of day",
    y = "Average wait time",
    color = "Date"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

wait_time_plot


volume_plot <- ggplot(
  functional_data,
  aes(
    x = time_decimal,
    y = average_wait_time_volume,
    group = factor(date),
    color = factor(period)
  )
) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ intersection, scales = "free_y") +
  scale_x_continuous(
    breaks = seq(0, 24, by = 3),
    limits = c(0, 24)
  ) +
  labs(
    title = "Averaged Wait-Time Volume Functional Curves",
    x = "Time of day",
    y = "Average wait-time volume",
    color = "Date"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

volume_plot


library(dplyr)
library(ggplot2)

pedestrian_by_time <- model_data |>
  group_by(
    intersection,
    time_decimal
  ) |>
  summarise(
    pedestrian_rate = mean(
      pedestrian_crossing,
      na.rm = TRUE
    ),
    pedestrian_days = sum(
      pedestrian_crossing,
      na.rm = TRUE
    ),
    total_days = n_distinct(date),
    .groups = "drop"
  )

ggplot(
  pedestrian_by_time,
  aes(
    x = time_decimal,
    y = pedestrian_rate
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1) +
  facet_wrap(
    ~ intersection,
    ncol = 3
  ) +
  scale_x_continuous(
    breaks = seq(0, 24, by = 3),
    limits = c(0, 24),
    labels = seq(0, 24, by = 3)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent
  ) +
  labs(
    title = "Pedestrian Crossings by Intersection and Time",
    subtitle = "Percentage of study days with a pedestrian crossing in each 15-minute interval",
    x = "Time of day",
    y = "Study days with pedestrian activity"
  ) +
  theme_minimal()



library(dplyr)
library(tidyr)
library(ggplot2)

intersection_change <- model_data |>
  group_by(
    intersection,
    period
  ) |>
  summarise(
    mean_wait_time = mean(
      average_wait_time,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = period,
    values_from = mean_wait_time
  ) |>
  mutate(
    difference = After - Before,
    absolute_difference = abs(difference)
  ) |>
  arrange(desc(absolute_difference))

intersection_change
ggplot(
  intersection_change,
  aes(
    x = reorder(intersection, difference),
    y = difference
  )
) +
  geom_col() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  coord_flip() +
  labs(
    title = "Average Before-to-After Change by Intersection",
    subtitle = "After minus Before average wait time",
    x = "Intersection",
    y = "Change in average wait time"
  ) +
  theme_minimal()





library(dplyr)
library(ggplot2)

model_data |>
  filter(intersection == "7102") |>
  ggplot(
    aes(
      x = time_decimal,
      y = average_wait_time,
      group = date,
      color = factor(study_day)
    )
  ) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(
    breaks = seq(0, 24, by = 3),
    limits = c(0, 24)
  ) +
  labs(
    title = "Average Wait Time by Study Day: Intersection 7102",
    x = "Time of day",
    y = "Average wait time",
    color = "Study date"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )
