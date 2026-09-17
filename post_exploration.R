#### EXPLORING POST #################

library(tidyverse)
library(ggplot2)

# ============================================================
# CREATE PREDICTION DATA
# ============================================================

prediction_data <- expand.grid(
  time_decimal = seq(0, 23.75, by = 0.25),
  after = c(0, 1),
  lidar = c(0, 1)
) |>
  mutate(
    average_wait_time_volume =
      mean(
        model_data$average_wait_time_volume,
        na.rm = TRUE
      ),
    
    pedestrian_crossing = 0L,
    
    # Required values; excluded from population predictions
    intersection = model_data$intersection[1],
    curve_id = model_data$curve_id[1],
    
    period = factor(
      if_else(after == 1, "After", "Before"),
      levels = c("Before", "After")
    ),
    
    lidar_status = factor(
      if_else(lidar == 1, "LiDAR", "No LiDAR"),
      levels = c("No LiDAR", "LiDAR")
    )
  )


# ============================================================
# PREDICT BEFORE / AFTER CURVES
# ============================================================

pred <- predict(
  functional_model_lidar,
  newdata = prediction_data,
  type = "link",
  se.fit = TRUE,
  exclude = c(
    "s(intersection)",
    "s(time_decimal,intersection)",
    "s(curve_id)"
  )
)

prediction_data <- prediction_data |>
  mutate(
    fitted = as.numeric(pred$fit),
    se = as.numeric(pred$se.fit),
    lower = fitted - 1.96 * se,
    upper = fitted + 1.96 * se
  )


# ============================================================
# PLOT ADJUSTED WAIT-TIME CURVES
# ============================================================

ggplot(
  prediction_data,
  aes(
    x = time_decimal,
    y = fitted,
    color = period,
    fill = period
  )
) +
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  scale_x_continuous(
    breaks = seq(0, 24, by = 3),
    limits = c(0, 24)
  ) +
  labs(
    title = "Adjusted Functional Wait-Time Curves",
    subtitle = paste(
      "Before vs. After under LiDAR and non-LiDAR conditions"
    ),
    x = "Time of day",
    y = "Predicted average wait time",
    color = "Period",
    fill = "Period"
  ) +
  theme_minimal()


# ============================================================
# CALCULATE AFTER - BEFORE EFFECT
# SEPARATELY FOR LIDAR = 0 AND LIDAR = 1
# ============================================================

before_data <- prediction_data |>
  filter(after == 0) |>
  select(-fitted, -se, -lower, -upper)

after_data <- prediction_data |>
  filter(after == 1) |>
  select(-fitted, -se, -lower, -upper)

# Make sure rows line up exactly
before_data <- before_data |>
  arrange(lidar, time_decimal)

after_data <- after_data |>
  arrange(lidar, time_decimal)


X_before <- predict(
  functional_model_lidar,
  newdata = before_data,
  type = "lpmatrix",
  exclude = c(
    "s(intersection)",
    "s(time_decimal,intersection)",
    "s(curve_id)"
  )
)

X_after <- predict(
  functional_model_lidar,
  newdata = after_data,
  type = "lpmatrix",
  exclude = c(
    "s(intersection)",
    "s(time_decimal,intersection)",
    "s(curve_id)"
  )
)

X_difference <- X_after - X_before

beta_hat <- coef(functional_model_lidar)
variance_matrix <- vcov(functional_model_lidar)

difference_estimate <- as.vector(
  X_difference %*% beta_hat
)

difference_se <- sqrt(
  rowSums(
    (X_difference %*% variance_matrix) *
      X_difference
  )
)

implementation_effect <- tibble(
  time_decimal = before_data$time_decimal,
  lidar = before_data$lidar,
  lidar_status = before_data$lidar_status,
  difference = difference_estimate,
  se = difference_se,
  lower = difference - 1.96 * se,
  upper = difference + 1.96 * se
)


# ============================================================
# PLOT IMPLEMENTATION EFFECT
# ============================================================

ggplot(
  implementation_effect,
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
    alpha = 0.2
  ) +
  geom_line(linewidth = 1)+
  scale_x_continuous(
    breaks = seq(0, 24, by = 3),
    limits = c(0, 24)
  ) +
  labs(
    title = "Estimated Effect of Traffic-Pattern Implementation",
    subtitle = "After minus Before wait time, by LiDAR status",
    x = "Time of day",
    y = "Difference in average wait time (seconds)"
  ) +
  theme_minimal()

ggsave(
  "Intersection After Minus Before Wait Time_No_Volume.png"
)
library(tidyverse)
library(mgcv)

# ============================================================
# INTERSECTION-LEVEL BETA CURVES:
# VOLUME AND PEDESTRIAN CROSSING
# ============================================================

time_grid <- seq(0, 23.75, by = 0.25)

# Start with valid rows so all factor levels are valid
reference_data <- model_data[
  rep(1, length(time_grid)),
  ,
  drop = FALSE
]

reference_data$time_decimal <- time_grid

# Hold Before/After at Before
reference_data$after <- 0

# Required random-effect variables
reference_data$intersection <- factor(
  reference_data$intersection,
  levels = levels(model_data$intersection)
)

reference_data$curve_id <- factor(
  reference_data$curve_id,
  levels = levels(model_data$curve_id)
)

# Random effects excluded to obtain population-level beta curves
random_terms <- c(
  "s(intersection)",
  "s(time_decimal,intersection)",
  "s(curve_id)"
)

beta_hat <- coef(functional_model)
V_beta <- vcov(functional_model)


# ============================================================
# HELPER FUNCTION FOR BETA CURVES
# ============================================================

make_beta_curve <- function(
    high_data,
    low_data,
    curve_name,
    divisor = 1
) {
  
  X_high <- predict(
    functional_model,
    newdata = high_data,
    type = "lpmatrix",
    exclude = random_terms
  )
  
  X_low <- predict(
    functional_model,
    newdata = low_data,
    type = "lpmatrix",
    exclude = random_terms
  )
  
  X_diff <- (X_high - X_low) / divisor
  
  estimate <- as.vector(
    X_diff %*% beta_hat
  )
  
  se <- sqrt(
    rowSums(
      (X_diff %*% V_beta) *
        X_diff
    )
  )
  
  tibble(
    time_decimal = high_data$time_decimal,
    curve = curve_name,
    estimate = estimate,
    se = se,
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se
  )
}


# ============================================================
# VOLUME BETA CURVE
# ============================================================

# Compare volume = 100 to volume = 0
# Then divide by 100 to get effect PER ONE UNIT of volume
volume_low <- reference_data
volume_high <- reference_data

volume_low$average_wait_time_volume <- 0
volume_high$average_wait_time_volume <- 100

# Hold pedestrian activity constant
volume_low$pedestrian_crossing <- 0
volume_high$pedestrian_crossing <- 0

beta_volume <- make_beta_curve(
  high_data = volume_high,
  low_data = volume_low,
  curve_name = "Wait-Time Volume",
  divisor = 100
)

# Plot volume beta curve
volume_beta_plot <- ggplot(
  beta_volume,
  aes(
    x = time_decimal,
    y = estimate
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
    alpha = 0.2
  ) +
  geom_line(linewidth = 1) +
  scale_x_continuous(
    breaks = seq(0, 24, by = 3),
    limits = c(0, 24)
  ) +
  labs(
    title = "Time-Varying Effect of Traffic Volume",
    subtitle = "Effect of a one-unit increase in average wait-time volume",
    x = "Time of day",
    y = expression(beta[Volume](t))
  ) +
  theme_minimal()

print(volume_beta_plot)


# ============================================================
# PEDESTRIAN BETA CURVE
# ============================================================

pedestrian_absent <- reference_data
pedestrian_present <- reference_data

# Hold volume constant at its overall mean
mean_volume <- mean(
  model_data$average_wait_time_volume,
  na.rm = TRUE
)

pedestrian_absent$average_wait_time_volume <- mean_volume
pedestrian_present$average_wait_time_volume <- mean_volume

pedestrian_absent$pedestrian_crossing <- 0
pedestrian_present$pedestrian_crossing <- 1

beta_pedestrian <- make_beta_curve(
  high_data = pedestrian_present,
  low_data = pedestrian_absent,
  curve_name = "Pedestrian Crossing"
)

# Plot pedestrian beta curve
pedestrian_beta_plot <- ggplot(
  beta_pedestrian,
  aes(
    x = time_decimal,
    y = estimate
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
    alpha = 0.2
  ) +
  geom_line(linewidth = 1) +
  scale_x_continuous(
    breaks = seq(0, 24, by = 3),
    limits = c(0, 24)
  ) +
  labs(
    title = "Time-Varying Effect of Pedestrian Activity",
    subtitle = "Pedestrian crossing present minus absent",
    x = "Time of day",
    y = expression(beta[Pedestrian](t))
  ) +
  theme_minimal()

print(pedestrian_beta_plot)

summary()