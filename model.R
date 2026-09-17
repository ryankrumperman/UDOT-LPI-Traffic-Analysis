################# MODELING SCRIPT #####################


library(dplyr)
library(lubridate)


functional_data <- functional_data |>
  filter(
    !(
      intersection == "7102" &
        period == "Before" &
        day_number == 2
    )
  )

model_data <- functional_data |>
  mutate(
    intersection = factor(intersection),
    
    date = as.Date(date),
    
    period = factor(
      period,
      levels = c("Before", "After")
    ),
    
    after = as.integer(period == "After"),
    
    pedestrian_crossing = as.integer(pedestrian_crossing),
    
    # LiDAR detection indicator
    lidar = case_when(
      intersection == "7104" ~ 1L,
      intersection == "7102" & period == "After" ~ 1L,
      TRUE ~ 0L
    ),
    
    curve_id = interaction(
      intersection,
      date,
      drop = TRUE
    ),
    
    time_decimal =
      hour(timestamp) +
      minute(timestamp) / 60
  ) |>
  arrange(intersection, date, time_decimal)


library(mgcv)

functional_model_lidar <- bam(
  average_wait_time ~
    
    after +
    
    s(
      time_decimal,
      bs = "cc",
      k = 16
    ) +
    
    s(
      time_decimal,
      by = after,
      bs = "cc",
      k = 16
    ) +
    
    # average_wait_time_volume +
    # 
    # s(
    #   time_decimal,
    #   by = average_wait_time_volume,
    #   bs = "cc",
    #   k = 12
    # ) +
    
    pedestrian_crossing +
    
    s(
      time_decimal,
      by = pedestrian_crossing,
      bs = "cc",
      k = 12
    ) +
    
    # Allow LiDAR effect to vary over time of day
    s(
      time_decimal,
      by = lidar,
      bs = "cc",
      k = 12
    ) +
    
    s(
      intersection,
      bs = "re"
    ) +
    
    s(
      time_decimal,
      intersection,
      bs = "fs",
      k = 8,
      m = 1
    ) +
    
    s(
      curve_id,
      bs = "re"
    ),
  
  data = model_data,
  method = "fREML",
  discrete = TRUE,
  knots = list(
    time_decimal = c(0, 24)
  )
)
summary(functional_model_lidar)


model_with_day <- functional_model

model_without_day <- update(
  functional_model,
  . ~ . - s(curve_id, bs = "re")
)

AIC(model_with_day, model_without_day)

summary(model_without_day)
gam.check(model_without_day)


gam.vcomp(model_with_day)




functional_model_intersection_effect <- bam(
  average_wait_time ~
    
    # ========================================================
  # POPULATION-LEVEL BEFORE / AFTER EFFECT
  # ========================================================
  
  # Overall constant Before-versus-After shift
  after +
    
    # Baseline Before curve
    s(
      time_decimal,
      bs = "cc",
      k = 16
    ) +
    
    # Overall time-varying After effect
    s(
      time_decimal,
      by = after,
      bs = "cc",
      k = 16
    ) +
    
    
    # ========================================================
  # TRAFFIC VOLUME
  # ========================================================
  
  average_wait_time_volume +
    
    s(
      time_decimal,
      by = average_wait_time_volume,
      bs = "cc",
      k = 12
    ) +
    
    
    # ========================================================
  # PEDESTRIAN ACTIVITY
  # ========================================================
  
  pedestrian_crossing +
    
    s(
      time_decimal,
      by = pedestrian_crossing,
      bs = "cc",
      k = 12
    ) +
    
    
    # ========================================================
  # LIDAR
  # ========================================================
  
  lidar +
    
    s(
      time_decimal,
      by = lidar,
      bs = "cc",
      k = 12
    ) +
    
    
    # ========================================================
  # EXISTING INTERSECTION EFFECTS
  # ========================================================
  
  # Baseline difference in level between intersections
  s(
    intersection,
    bs = "re"
  ) +
    
    # Baseline intersection-specific daily shape
    s(
      time_decimal,
      intersection,
      bs = "fs",
      k = 8,
      m = 1
    ) +
    
    
    # ========================================================
  # NEW: INTERSECTION-SPECIFIC IMPLEMENTATION EFFECT
  # ========================================================
  
  # Allows the overall After shift to differ by intersection
  s(
    intersection,
    by = after,
    bs = "re"
  ) +
    
    # Allows the After-minus-Before curve to differ
    # over time for each intersection
    s(
      time_decimal,
      intersection,
      by = after,
      bs = "fs",
      k = 8,
      m = 1
    ) +
    
    
    # ========================================================
  # DAY-LEVEL RANDOM EFFECT
  # ========================================================
  
  s(
    curve_id,
    bs = "re"
  ),
  
  data = model_data,
  method = "fREML",
  discrete = TRUE,
  
  knots = list(
    time_decimal = c(0, 24)
  )
)

summary(functional_model_intersection_effect)



gam.check(functional_model_intersection_effect)







library(tidyverse)
library(mgcv)

# ============================================================
# INTERSECTION-SPECIFIC AFTER - BEFORE EFFECT CURVES
# ============================================================

# Time grid
time_grid <- seq(0, 23.75, by = 0.25)

# Intersections
intersection_levels <- levels(model_data$intersection)

# Create prediction grid
intersection_prediction_data <- expand.grid(
  time_decimal = time_grid,
  intersection = intersection_levels,
  after = c(0, 1)
) |>
  mutate(
    intersection = factor(
      intersection,
      levels = levels(model_data$intersection)
    ),
    
    average_wait_time_volume =
      mean(
        model_data$average_wait_time_volume,
        na.rm = TRUE
      ),
    
    pedestrian_crossing = 0L,
    
    # Hold LiDAR constant so the Before/After contrast is
    # not also changing LiDAR status
    lidar = 0L,
    
    period = factor(
      if_else(after == 1, "After", "Before"),
      levels = c("Before", "After")
    ),
    
    # Required for prediction, but day effect will be excluded
    curve_id = model_data$curve_id[1]
  )


# ============================================================
# SPLIT INTO BEFORE AND AFTER DATA
# ============================================================

before_intersection <- intersection_prediction_data |>
  filter(after == 0) |>
  arrange(intersection, time_decimal)

after_intersection <- intersection_prediction_data |>
  filter(after == 1) |>
  arrange(intersection, time_decimal)


# ============================================================
# LINEAR PREDICTOR MATRICES
# ============================================================

# IMPORTANT:
# We exclude the daily random intercept,
# but KEEP the intersection-specific effects.

X_before_intersection <- predict(
  functional_model_intersection_effect,
  newdata = before_intersection,
  type = "lpmatrix",
  exclude = c(
    "s(curve_id)"
  )
)

X_after_intersection <- predict(
  functional_model_intersection_effect,
  newdata = after_intersection,
  type = "lpmatrix",
  exclude = c(
    "s(curve_id)"
  )
)


# ============================================================
# AFTER - BEFORE CONTRAST
# ============================================================

X_difference_intersection <-
  X_after_intersection - X_before_intersection

beta_hat_intersection <-
  coef(functional_model_intersection_effect)

variance_matrix_intersection <-
  vcov(functional_model_intersection_effect)


# Estimated effect
difference_estimate_intersection <- as.vector(
  X_difference_intersection %*%
    beta_hat_intersection
)


# Standard error
difference_se_intersection <- sqrt(
  rowSums(
    (
      X_difference_intersection %*%
        variance_matrix_intersection
    ) *
      X_difference_intersection
  )
)


# ============================================================
# RESULTS DATASET
# ============================================================

intersection_effect <- tibble(
  intersection = before_intersection$intersection,
  time_decimal = before_intersection$time_decimal,
  
  difference = difference_estimate_intersection,
  
  se = difference_se_intersection,
  
  lower = difference - 1.96 * se,
  
  upper = difference + 1.96 * se
)


# ============================================================
# FACETED PLOT
# ============================================================

intersection_effect_plot <- ggplot(
  intersection_effect,
  aes(
    x = time_decimal,
    y = difference
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.20
  ) +
  geom_line(
    linewidth = 0.9
  ) +
  facet_wrap(
    ~intersection,
    ncol = 3
  ) +
  scale_x_continuous(
    breaks = seq(0, 24, by = 6),
    limits = c(0, 24)
  ) +
  labs(
    title = "Estimated Traffic-Pattern Effect by Intersection",
    subtitle = "After minus Before predicted average wait time",
    x = "Time of day",
    y = "Difference in average wait time (seconds)"
  ) +
  theme_minimal()

print(intersection_effect_plot)


# Optional save
ggsave(
  "Intersection-Specific After Minus Before Wait Time.png",
  plot = intersection_effect_plot,
  width = 10,
  height = 8,
  dpi = 300
)