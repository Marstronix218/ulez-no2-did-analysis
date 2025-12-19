# ===============================
# Package installation & loading
# ===============================

if (!require("dplyr", character.only = TRUE)) install.packages("dplyr")
if (!require("ggplot2", character.only = TRUE)) install.packages("ggplot2")
if (!require("lubridate", character.only = TRUE)) install.packages("lubridate")
if (!require("zoo", character.only = TRUE)) install.packages("zoo")
if (!require("fixest", character.only = TRUE)) install.packages("fixest")
if (!require("tidyr", character.only = TRUE)) install.packages("tidyr")
if (!require("flextable", character.only = TRUE)) install.packages("flextable")
if (!require("officer", character.only = TRUE)) install.packages("officer")

library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(fixest)
library(tidyr)
library(flextable)
library(officer)

# ===============================
# Analysis code
# ===============================

data_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRVmNQhawaVD2smh4nvQ1rExpUFDH4VHjW1-4KH3zBbvL1YCmOwZKLpoFrZ1xt7xUc3njjCuz8h3UFb/pub?output=csv"
data_did <- read.csv(data_url)
head (data_did)


# ------------------------------------------------------------------------------
# Summary Statistics Table
# ------------------------------------------------------------------------------


# 1. Construct summary statistics for the final analysis sample
#    We summarize the panel structure (station–day observations),
#    the number of monitoring stations by treatment status,
#    the temporal coverage, and the distribution of NO₂ concentrations.

summary_table <- data_did %>%
  summarise(
    `Station–day observations` = n(),
    `Stations` = n_distinct(site_id),
    `Treated stations` = n_distinct(site_id[treatment_indicator == 1]),
    `Control stations` = n_distinct(site_id[treatment_indicator == 0]),
    `Start date` = min(measurement_date, na.rm = TRUE),
    `End date` = max(measurement_date, na.rm = TRUE),
    `Mean NO₂ (µg/m³)` = mean(no2_concentration, na.rm = TRUE),
    `SD NO₂` = sd(no2_concentration, na.rm = TRUE),
    `Min NO₂` = min(no2_concentration, na.rm = TRUE),
    `25th percentile` = quantile(no2_concentration, 0.25, na.rm = TRUE),
    `Median NO₂` = median(no2_concentration, na.rm = TRUE),
    `75th percentile` = quantile(no2_concentration, 0.75, na.rm = TRUE),
    `Max NO₂` = max(no2_concentration, na.rm = TRUE)
  ) %>%
  # Convert all values to character to allow stacking into a single column
  mutate(across(everything(), as.character)) %>%
  # Reshape to a table format with statistics as rows
  pivot_longer(
    cols = everything(),
    names_to = "Statistic",
    values_to = "Full sample"
  )


# 2. Format the table for inclusion in the paper

ft <- flextable(summary_table) %>%
  autofit() %>%
  align(align = "left", part = "body") %>%
  align(align = "center", part = "header") %>%
  set_caption("Table 1. Summary statistics for the final analysis sample")


# 3. Export the table to Word for use in Google Docs

doc <- read_docx()
doc <- body_add_flextable(doc, ft)
print(doc, target = "summary_statistics.docx")



# ------------------------------------------------------------------------------
# Ensuring that the stations all have at least 75% coverage.
# ------------------------------------------------------------------------------


# Calculate data coverage for each monitoring station
coverage_table <- data_did %>%
  group_by(site_id) %>%                       # group the dataset by station ID
  summarise(
    days_with_data = sum(!is.na(no2_concentration)),   # count days with valid NO₂ values
    total_rows = n(),                                  # total number of days recorded for that station
    coverage_pct = days_with_data / total_rows * 100   # percentage of days with valid data
  ) %>%
  arrange(coverage_pct)                        # sort stations from lowest to highest coverage
coverage_table


# ------------------------------------------------------------------------------
# Counting the treated and control stations
# ------------------------------------------------------------------------------


# ---- Create a station-level table from data_did ----
station_status <- data_did %>%
  group_by(site_id) %>%                      # one row per station
  summarise(
    treated = first(treatment_indicator),    # extract its treatment assignment
    .groups = "drop"
  )

# ---- Count total stations ----
nrow(station_status)

# ---- Count treated vs control ----
station_status %>% count(treated)


# ------------------------------------------------------------------------------
# Where does the missing data in the dataset occur? How random is the missing data?
# ------------------------------------------------------------------------------


# Convert measurement_date to a real Date

data_did <- data_did %>%
  mutate(
    measurement_date = as.Date(measurement_date)
  )



# Define the ULEZ treatment start date
ulez_start <- as.Date("2019-04-08")

# ---- Visualize when NO₂ readings are missing in data_did ----
data_did %>%
  # Create an indicator for missing NO₂ observations
  mutate(
    missing = is.na(no2_concentration)        # TRUE if NO₂ is missing on that row
  ) %>%
  # Aggregate missingness at the daily level
  group_by(measurement_date) %>%              # one row per calendar day
  summarise(
    pct_missing = mean(missing),              # share of stations missing data that day
    .groups = "drop"
  ) %>%
  # Plot daily missingness over time
  ggplot(aes(x = measurement_date, y = pct_missing)) +
  geom_line(color = "black", linewidth = 0.7) +
  # Add vertical line at ULEZ introduction date
  geom_vline(
    xintercept = ulez_start,
    linetype = "dashed",
    color = "black",
    linewidth = 0.7
  ) +
  # Label axes and title
  labs(
    title = "Daily Share of Missing NO2 Readings",
    x = "Date",
    y = "Share Missing"
  ) +
  theme_minimal(base_size = 13)


# ------------------------------------------------------------------------------
# Daily percent of stations missing for treatment and control group
# ------------------------------------------------------------------------------


# ---- Step 1: Count how many stations belong to each group ----
# This is needed to compute missingness as a percentage rather than a raw count

group_sizes <- data_did %>%

  # Keep one row per station and treatment status
  distinct(site_id, treatment_indicator) %>%

  # Create a readable group label
  mutate(group = ifelse(treatment_indicator == 1, "Treated", "Control")) %>%

  # Count how many stations are in each group
  count(group, name = "total_stations")

# ---- Step 2: Compute daily missingness by treatment group ----
daily_pct_missing <- data_did %>%

  # Flag whether the NO₂ value is missing and assign group labels
  mutate(
    missing = is.na(no2_concentration),        # TRUE if NO₂ is missing
    group = ifelse(treatment_indicator == 1, "Treated", "Control")
  ) %>%

  # Aggregate at the day × group level
  group_by(measurement_date, group) %>%

  # Count how many stations are missing data on each day in each group
  summarise(
    stations_missing = sum(missing),
    .groups = "drop"
  ) %>%

  # Merge in the total number of stations per group
  # so that missingness can be expressed as a share
  left_join(group_sizes, by = "group") %>%

  # Compute the percentage of stations missing data each day
  mutate(
    pct_missing = stations_missing / total_stations
  )

# ---- Step 3: Plot treated vs control missingness over time ----
ggplot(
  daily_pct_missing,
  aes(x = measurement_date, y = pct_missing, color = group)
) +
  geom_line(alpha = 0.8, size = 0.7) +
  labs(
    title = "Daily Percentage of Missing NO₂ Readings (Treated vs Control)",
    x = "Date",
    y = "Share Missing",
    color = "Group"
  ) +
  theme_minimal(base_size = 13)


# Compute a 14-day rolling average of daily missingness,
# separately for treated and control stations
daily_pct_missing_smooth <- daily_pct_missing %>%

  # Ensure observations are ordered correctly within each group
  # (rolling averages depend on time ordering)
  arrange(group, measurement_date) %>%

  # Compute rolling averages separately for treated and control groups
  # so that smoothing does not mix information across groups
  group_by(group) %>%

  # Create a smoothed version of the daily missingness rate
  mutate(
    pct_missing_14d = rollmean(
      pct_missing,   # daily share of missing NO₂ observations
      k = 14,         # window size: 14 days
      fill = NA,      # return NA at the beginning and end where the window is incomplete
      align = "center" # center the window so each value reflects surrounding days
    )
  ) %>%

  # Remove grouping to avoid unintended behavior in later operations
  ungroup()


# Plot both raw and smoothed missingness by treatment status
ggplot(
  daily_pct_missing_smooth,
  aes(x = measurement_date, y = pct_missing, color = group)
) +

  # Plot raw daily missingness with high transparency
  # This shows day-to-day volatility and short-lived outages
  geom_line(alpha = 0.25, size = 0.5) +

  # Overlay the 14-day rolling average for each group
  # This highlights underlying trends while keeping the raw data visible
  geom_line(
    aes(y = pct_missing_14d),
    size = 0.9
  ) +

  # Add a vertical line at the ULEZ introduction date
  # to assess whether missingness shifts at treatment
  geom_vline(
    xintercept = ulez_start,
    linetype = "dashed",
    color = "black",
    linewidth = 0.7
  ) +

  # Clear labeling for interpretation
  labs(
    title = "Daily Percentage of Missing NO₂ Readings (Raw and 14-day Smoothed)",
    x = "Date",
    y = "Share Missing",
    color = "Group"
  ) +

  # Minimal theme for clarity and consistency with other figures
  theme_minimal(base_size = 13)



# ------------------------------------------------------------------------------
# Station-level missingness over time
# ------------------------------------------------------------------------------


# Identify whether each station has any missing NO₂ data within a given month
missing_station_month <- data_did %>%

  # Create a monthly time variable and flag missing NO₂ observations
  mutate(
    month = floor_date(measurement_date, "month"),  # collapse dates to calendar months
    missing = is.na(no2_concentration)               # TRUE if NO₂ is missing on that day
  ) %>%

  # Aggregate at the station × month level
  group_by(site_id, month) %>%

  # Indicate whether the station had at least one missing observation in that month
  summarise(
    any_missing = any(missing),   # TRUE if at least one day is missing in that month
    .groups = "drop"
  )


# Compute station-level data coverage and order stations by coverage rate
station_order <- data_did %>%

  # Group observations by monitoring station
  group_by(site_id) %>%

  # Calculate the share of days with observed (non-missing) NO₂ values per station
  summarise(
    coverage_rate = mean(!is.na(no2_concentration)),  # fraction of days with data
    .groups = "drop"
  ) %>%

  # Sort stations from lowest to highest coverage
  arrange(coverage_rate) %>%

  # Convert site_id to a factor ordered by coverage rate
  # (useful for plotting stations in coverage order)
  mutate(site_id = factor(site_id, levels = site_id))


# Attach station-level coverage information to the station–month missingness data
missing_station_month <- missing_station_month %>%

  # Join the ordered station coverage rates to each station–month observation
  # This allows stations to be plotted or analyzed in order of overall data coverage
  left_join(station_order, by = "site_id")



# Define the ULEZ introduction date
ulez_start <- as.Date("2019-04-08")

# Plot station-level missingness by month as a heatmap
ggplot(
  missing_station_month,
  aes(x = month, y = site_id, fill = any_missing)
) +

  # Draw one tile per station–month combination
  # Color indicates whether the station had any missing data in that month
  geom_tile() +

  # Manually define colors and labels for missing vs observed data
  scale_fill_manual(
    values = c("FALSE" = "grey90", "TRUE" = "steelblue"),
    labels = c("Observed", "Missing"),
    name = "Data status"
  ) +

  # Add a vertical line at the ULEZ introduction date
  geom_vline(
    xintercept = as.numeric(ulez_start),
    linetype = "dashed",
    color = "black",
    linewidth = 0.6
  ) +

  # Label axes and title
  labs(
    title = "Missing NO2 Observations by Station and Month",
    x = "Month",
    y = "Monitoring Station"
  ) +

  # Use a clean minimal theme
  theme_minimal(base_size = 12) +

  # Remove station labels to reduce visual clutter
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


# ------------------------------------------------------------------------------
# Day-level missingness
# ------------------------------------------------------------------------------



# Create a day-level indicator for missing NO₂ observations
df_missing_day <- data_did %>%

  # Flag whether the NO₂ concentration is missing for each station–day observation
  mutate(
    missing = is.na(no2_concentration)
  )


# Order stations by overall data coverage (from lowest to highest)
station_order <- df_missing_day %>%

  # Group observations by monitoring station
  group_by(site_id) %>%

  # Compute the share of days with observed (non-missing) NO₂ data per station
  summarise(
    coverage = mean(!missing)
  ) %>%

  # Sort stations by coverage rate
  arrange(coverage) %>%

  # Extract the ordered station IDs as a vector
  pull(site_id)




# Define the ULEZ introduction date
ulez_start <- as.Date("2019-04-08")

# Plot daily missingness by station as a heatmap
ggplot(
  df_missing_day,
  aes(
    x = measurement_date,                         # calendar day
    y = factor(site_id, levels = station_order),  # stations ordered by coverage
    fill = missing                                # missing vs observed NO₂
  )
) +

  # Draw one tile per station–day observation
  geom_tile(height = 0.9) +

  # Define colors and labels for missing and observed data
  scale_fill_manual(
    values = c(
      "FALSE" = "grey90",    # observed NO₂
      "TRUE"  = "steelblue"  # missing NO₂
    ),
    labels = c("Observed", "Missing"),
    name = "Data status"
  ) +

  # Add a vertical line at the ULEZ introduction date
  geom_vline(
    xintercept = ulez_start,
    linetype = "dashed",
    color = "black",
    linewidth = 0.6
  ) +

  # Label axes and title
  labs(
    title = "Daily Missing NO2 Observations by Monitoring Station",
    x = "Date",
    y = "Monitoring station"
  ) +

  # Use a clean minimal theme
  theme_minimal(base_size = 12) +

  # Remove station labels to reduce visual clutter
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )



# Compute station-level data coverage statistics
station_coverage <- data_did %>%

  # Group observations by monitoring station
  group_by(site_id) %>%

  # Summarise data availability for each station
  summarise(
    total_days = n(),                               # total number of station–day observations
    observed_days = sum(!is.na(no2_concentration)), # number of days with observed NO₂ values
    coverage_rate = observed_days / total_days      # share of days with available data
  ) %>%

  # Remove grouping to avoid unintended behavior in later operations
  ungroup()



# ------------------------------------------------------------------------------
# Parallel Trends Plot (Pre-policy only)
# ------------------------------------------------------------------------------

# Ensure measurement_date is Date (safe guard)
data_did <- data_did %>%
  mutate(measurement_date = as.Date(measurement_date))

# Define the ULEZ start date
ulez_start <- as.Date("2019-04-08")

# Keep ONLY pre-policy observations (parallel trends check must be pre-treatment)
pre_trends <- data_did %>%
  filter(measurement_date < ulez_start) %>%
  filter(!is.na(no2_concentration)) %>%
  group_by(measurement_date, treatment_indicator) %>%
  summarise(
    avg_NO2 = mean(no2_concentration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(treatment_indicator == 1, "Treated", "Control")
  )

# Raw pre-policy trends plot
ggplot(pre_trends, aes(x = measurement_date, y = avg_NO2, color = group)) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Parallel Trends Check (Pre-ULEZ)",
    x     = "Date",
    y     = "Average NO2 Concentration (µg/m³)",
    color = "Group"
  ) +
  theme_minimal(base_size = 13)

# Smoothed pre-policy trends (30-day rolling mean)
pre_trends_smooth <- pre_trends %>%
  group_by(group) %>%
  arrange(measurement_date, .by_group = TRUE) %>%
  mutate(
    avg_NO2_roll30 = rollmean(avg_NO2, k = 30, fill = NA, align = "right")
  ) %>%
  ungroup()

ggplot(pre_trends_smooth, aes(x = measurement_date, y = avg_NO2_roll30, color = group)) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Parallel Trends Check (Pre-ULEZ, 30-day Rolling Mean)",
    x     = "Date",
    y     = "Average NO2 (30-day rolling mean, µg/m³)",
    color = "Group"
  ) +
  theme_minimal(base_size = 13)


# ------------------------------------------------------------
# Create event time (days relative to ULEZ introduction)
# ------------------------------------------------------------

data_did <- data_did %>%
  mutate(
    event_time = as.numeric(measurement_date - ulez_start)
  )


# ------------------------------------------------------------
# Event-study DiD specification
# ------------------------------------------------------------

did_event <- feols(
  no2_concentration ~ 
    i(event_time, treatment_indicator, ref = -1) |
    site_id + measurement_date,
  data = data_did,
  cluster = ~site_id
)


# ------------------------------------------------------------
# Event-study plot
# ------------------------------------------------------------

iplot(
  did_event,
  ref.line = 0,
  xlab = "Days relative to ULEZ introduction",
  ylab = "Effect on daily NO2 concentration (µg/m³)",
  main = "Event-study estimates of the ULEZ effect on NO2"
)


# ------------------------------------------------------------
# Bin event time into weeks
# ------------------------------------------------------------

data_did <- data_did %>%
  mutate(
    event_week = floor(event_time / 7)
  )

did_event_week <- feols(
  no2_concentration ~ 
    i(event_week, treatment_indicator, ref = -1) |
    site_id + measurement_date,
  data = data_did,
  cluster = ~site_id
)

iplot(
  did_event_week,
  ref.line = 0,
  xlab = "Weeks relative to ULEZ introduction",
  ylab = "Effect on daily NO2 concentration (µg/m³)",
  main = "Event-study estimates of the ULEZ effect on NO2"
)


# ------------------------------------------------------------------------------
# Main DiD Model with fixed effects
# ------------------------------------------------------------------------------


# Main DiD model
did_main <- feols(
  no2_concentration ~ treatment_indicator * pre_post_indicator |
    site_id + measurement_date,
  data = data_did,
  cluster = "site_id"
)

# View results
summary(did_main)


# ------------------------------------------------------------------------------
# Simpler DidD model for comparison
# ------------------------------------------------------------------------------


# Naive DiD model without fixed effects (for comparison only)
did_naive <- feols(
  no2_concentration ~ treatment_indicator * pre_post_indicator,
  data = data_did,
  cluster = "site_id"
)

# View results
summary(did_naive)


# ------------------------------------------------------------------------------
# Placebo-in-time test
# ------------------------------------------------------------------------------



# 1. Keep only observations before the actual ULEZ start date (8 April 2019)
#    This avoids overlap between the placebo post-period and the real treatment.
data_placebo <- subset(
  data_did,
  measurement_date < as.Date("2019-04-08")
)

# 2. Define a placebo post indicator using a fake intervention date
#    (one year before the real policy).
data_placebo$post_placebo_2018 <- ifelse(
  data_placebo$measurement_date >= as.Date("2018-04-08"), 1, 0
)

# 3. Re-estimate the same fixed-effects DiD model on the restricted sample.
#    Standard errors are clustered at the station level.
did_placebo_2018 <- feols(
  no2_concentration ~ treatment_indicator * post_placebo_2018 |
    site_id + measurement_date,
  data = data_placebo,
  cluster = "site_id"
)

# 4. Inspect the placebo interaction coefficient
summary(did_placebo_2018)


# ------------------------------------------------------------------------------
# Increasing the coverage cutoff to 80%
# ------------------------------------------------------------------------------



# 1. Calculate data coverage for each station
#    Coverage is defined as the share of days with a valid NO₂ observation.
coverage_by_station <- data_did %>%
  group_by(site_id) %>%
  summarise(
    total_days = n(),
    observed_days = sum(!is.na(no2_concentration)),
    coverage_share = observed_days / total_days
  )

# 2. Keep only stations with at least 80% coverage
stations_80 <- coverage_by_station %>%
  filter(coverage_share >= 0.80) %>%
  pull(site_id)

# 3. Restrict the dataset to high-coverage stations
data_80 <- data_did %>%
  filter(site_id %in% stations_80)

# 4. Re-estimate the main fixed-effects DiD model on the restricted sample
did_80 <- feols(
  no2_concentration ~ treatment_indicator * pre_post_indicator |
    site_id + measurement_date,
  data = data_80,
  cluster = "site_id"
)

# 5. View the results
summary(did_80)


# ------------------------------------------------------------------------------
# Removing control stations closest to the ULEZ boundary
# ------------------------------------------------------------------------------


# 1. Define the set of control stations located closest to the ULEZ boundary
#    These stations are within 1 km of the boundary and may be affected by
#    spillover effects such as traffic rerouting.
boundary_controls <- c("WMA", "WAA", "LB5")

# 2. Remove these control stations from the dataset
data_no_boundary_controls <- data_did %>%
  filter(!(site_id %in% boundary_controls))

# 3. Re-estimate the main fixed-effects DiD model on the restricted sample
did_no_boundary_controls <- feols(
  no2_concentration ~ treatment_indicator * pre_post_indicator |
    site_id + measurement_date,
  data = data_no_boundary_controls,
  cluster = "site_id"
)

# 4. Inspect the results
summary(did_no_boundary_controls)


# ------------------------------------------------------------------------------
# Placebo-in-space test
# ------------------------------------------------------------------------------


# 1. Set a random seed so the placebo assignment is reproducible
#    This ensures that the same stations are selected each time the code is run.
set.seed(123)

# 2. Identify all stations that were never exposed to the ULEZ
#    Only these stations are eligible for placebo treatment.
control_stations <- data_did %>%
  filter(treatment_indicator == 0) %>%
  distinct(site_id) %>%
  pull(site_id)

# 3. Determine how many stations were actually treated
#    We match the size of the placebo-treated group to the real treated group.
n_treated <- data_did %>%
  filter(treatment_indicator == 1) %>%
  distinct(site_id) %>%
  nrow()

# 4. Randomly select control stations to receive placebo treatment
placebo_treated_stations <- sample(control_stations, n_treated)

# 5. Create a placebo treatment indicator
#    This indicator equals 1 for randomly selected control stations
#    and 0 for all other stations, including those truly treated by the ULEZ.
data_placebo_space <- data_did %>%
  mutate(placebo_treatment = ifelse(site_id %in% placebo_treated_stations, 1, 0))

# 6. Estimate the placebo-in-space DiD model
#    The specification mirrors the main model, replacing the true treatment
#    indicator with the placebo treatment definition.
did_placebo_space <- feols(
  no2_concentration ~ placebo_treatment * pre_post_indicator |
    site_id + measurement_date,
  data = data_placebo_space,
  cluster = "site_id"
)

# 7. Inspect the placebo interaction coefficient
summary(did_placebo_space)



# ------------------------------------------------------------------------------
# Placebo-in-space simulation
# ------------------------------------------------------------------------------


# 1. Set parameters for the placebo-in-space simulation
# We specify the number of random assignments used to construct
# the placebo distribution. Several hundred repetitions are
# sufficient to characterize the behavior of the estimator
# under arbitrary spatial treatment assignment.
n_draws <- 500

# We fix the random seed to ensure full reproducibility
set.seed(123)


# 2. Identify control stations eligible for placebo treatment
# Placebo treatment is assigned only among stations that were
# never exposed to the ULEZ. This ensures that the placebo
# intervention does not overlap with the true treatment.
control_stations <- data_did %>%
  filter(treatment_indicator == 0) %>%
  distinct(site_id) %>%
  pull(site_id)

# We match the number of placebo-treated stations to the size
# of the actual treated group.
n_treated <- data_did %>%
  filter(treatment_indicator == 1) %>%
  distinct(site_id) %>%
  nrow()


# 3. Repeated random placebo assignments and estimation
# For each draw, we randomly select a set of control stations
# to receive placebo treatment, re-estimate the fixed-effects
# DiD model, and store the coefficient on the interaction term.
placebo_estimates <- numeric(n_draws)

for (i in 1:n_draws) {
  
  placebo_treated <- sample(control_stations, n_treated)
  
  data_placebo <- data_did %>%
    mutate(placebo_treatment = ifelse(site_id %in% placebo_treated, 1, 0))
  
  placebo_model <- feols(
    no2_concentration ~ placebo_treatment * pre_post_indicator |
      site_id + measurement_date,
    data = data_placebo,
    cluster = "site_id"
  )
  
  placebo_estimates[i] <-
    coef(placebo_model)["placebo_treatment:pre_post_indicator"]
}

# We collect the placebo estimates in a data frame for visualization
placebo_df <- data.frame(placebo_effect = placebo_estimates)


# 4. Visualize the placebo-in-space distribution
# The histogram shows the distribution of DiD estimates obtained
# under random spatial assignment. The dashed vertical line marks
# the estimated treatment effect from the actual ULEZ policy.
true_effect <- -6.3  # main DiD estimate from the baseline specification

ggplot(placebo_df, aes(x = placebo_effect)) +
  geom_histogram(bins = 40, color = "white") +
  geom_vline(xintercept = true_effect, linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribution of placebo-in-space DiD estimates",
    x = "Placebo treatment effect (µg/m³)",
    y = "Frequency"
  ) +
  theme_minimal()



# ------------------------------------------------------------
# Compute Cohen's d for the DiD estimate
# (using pre-treatment variation as the reference)
# ------------------------------------------------------------

# DiD coefficient: average post-ULEZ change at treated stations
did_effect <- coef(did_main)["treatment_indicator:pre_post_indicator"]

# Standard deviation of NO2 in the pre-treatment period
sd_pre <- data_did %>%
  filter(pre_post_indicator == 0) %>%
  summarise(sd_no2 = sd(no2_concentration, na.rm = TRUE)) %>%
  pull(sd_no2)

# Cohen's d: effect size in standard deviation units
cohens_d <- did_effect / sd_pre

cohens_d
