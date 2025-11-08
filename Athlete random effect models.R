# athletedates_analysis.R

# Load required packages
library(readr)      # read_csv()
library(dplyr)      # data manipulation
library(tidyr)      # reshaping
library(lubridate)  # date parsing
library(ggplot2)    # plotting
library(GGally)     # advanced pair plots
library(lme4)       # mixed effects models
library(caret)      # cross-validation framework

# 1) Import each CSV into its own data frame
file_paths <- list.files(path = ".", pattern = "\\.csv$", full.names = TRUE)
clean_names <- tools::file_path_sans_ext(basename(file_paths)) %>% make.names()
df_list <- setNames(lapply(file_paths, read_csv), clean_names)

# 2) Parse Date column into Date class for each data frame
#    Dynamically detect date column and handle parsing warnings/errors
df_list <- Map(function(df, nm) {
  date_col <- names(df)[tolower(names(df)) %in% c("date", "testdate")] %>% head(1)
  if (length(date_col) == 0) {
    message(paste0("Skipping dataset '", nm, "': no date column found."))
    return(NULL)
  }
  parsed_dates <- tryCatch(
    suppressWarnings(parse_date_time(df[[date_col]], orders = c("ymd", "mdy", "dmy"))),
    warning = function(w) {
      message(paste0("Warning parsing dates in '", nm, "': ", w$message))
      rep(NA, nrow(df))
    },
    error = function(e) {
      message(paste0("Error parsing dates in '", nm, "': ", e$message))
      rep(NA, nrow(df))
    }
  )
  df[[date_col]] <- as_date(parsed_dates)
  if (date_col != "Date") names(df)[names(df) == date_col] <- "Date"
  if (all(is.na(df$Date))) {
    message(paste0("Dropping dataset '", nm, "' because all dates failed to parse."))
    return(NULL)
  }
  return(df)
}, df_list, names(df_list))
# Remove datasets that failed parsing
df_list <- Filter(Negate(is.null), df_list)

# 3) Combine all tests into one long-format data frame
all_tests <- bind_rows(df_list, .id = "TestGroup") %>%
  select(Athlete, Date, TestGroup, everything()) %>%
  arrange(Athlete, Date)

# 4) Export the combined all_tests data frame to CSV
#write_csv(all_tests, "all_tests.csv")

# 5) Merge tests within one day clusters per athlete
metric_cols <- setdiff(names(all_tests), c("Athlete", "Date", "TestGroup"))
all_tests_clustered <- all_tests %>%
  arrange(Athlete, Date) %>%
  group_by(Athlete) %>%
  dplyr::mutate(
    lag_date = lag(Date),
    diff_days = as.numeric(Date - lag_date),
    new_cluster = ifelse(is.na(diff_days) | diff_days > 1, 1, 0),
    cluster_id = cumsum(new_cluster)
  ) %>%
  ungroup() %>%
  select(-lag_date, -diff_days, -new_cluster)

merged_tests <- all_tests_clustered %>%
  group_by(Athlete, cluster_id) %>%
  summarise(
    ClusterDate = min(Date),
    across(all_of(metric_cols), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(Athlete, ClusterDate)


# Drop any rows with missing data
merged_tests_clean <- merged_tests %>% drop_na()
merged_tests_clean <- merged_tests_clean %>%
  mutate(
    `L Max Force (N)` = as.double(`L Max Force (N)`),
    `R Max Force (N)` = as.double(`R Max Force (N)`),
    diff_N = `L Max Force (N)` - `R Max Force (N)`
  )
#write_csv(merged_tests_clean, "merged_tests_clean.csv")

# 6) Mixed Effects Model with 10-fold Cross-Validation (manual implementation)
# Ensure ClusterDateNum exists (numeric days since earliest date)
if (!"ClusterDateNum" %in% colnames(merged_tests_clean)) {
  merged_tests_clean <- merged_tests_clean %>%
    mutate(ClusterDateNum = as.numeric(ClusterDate - min(ClusterDate, na.rm = TRUE)))
}

# Create 10-fold CV folds
set.seed(710)
folds <- caret::createFolds(merged_tests_clean$Total, k = 10, list = TRUE, returnTrain = FALSE)

# Perform manual CV
cv_results_list <- lapply(seq_along(folds), function(i) {
  test_idx <- folds[[i]]
  train_data <- merged_tests_clean[-test_idx, ]
  test_data  <- merged_tests_clean[test_idx, ]
  
  # Fit mixed-effects model on training fold
  cv_model <- lme4::lmer(
    Total ~ diff_N + (1 + ClusterDateNum | Athlete),
    data = train_data,
    REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
  )
  # Predict on test fold (allow new levels)
  preds <- predict(cv_model, newdata = test_data, allow.new.levels = TRUE)
  data.frame(
    Fold = i,
    obs  = test_data$Total,
    pred = preds
  )
})
cv_results <- bind_rows(cv_results_list)

# Compute CV performance metrics
cv_perf <- caret::postResample(pred = cv_results$pred, obs = cv_results$obs)
print("Cross-validation performance:")
print(cv_perf)

# 7) Final Mixed-Effects Model on full data
final_model <- lme4::lmer(
  Total ~ diff_N + (1 + ClusterDateNum | Athlete),
  data = merged_tests_clean,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

print("Final model summary:")
print(summary(final_model))
anova(final_model)


##################################################
# 8) Bootstrap CIs and infer fixed/random effects
##################################################
# Bootstrap fixed-effects coefficients (nsim=1000)
set.seed(710)
boot_results <- bootMer(
  final_model,
  FUN = function(x) fixef(x),
  nsim = 1000,
  use.u = FALSE,
  type = "parametric"
)
# Compute percentile-based 95% CIs
ci_fixed <- confint(boot_results, method = "perc")
print("Bootstrap 95% CI for fixed effects:")
print(ci_fixed)

# Inferential statistics for fixed effects using lmerTest
library(lmerTest)
# Refit model to incorporate p-values
final_model_p <- update(
  final_model,
  REML = FALSE
)
print("Fixed effects with p-values and F-tests:")
print(anova(final_model_p, type = 3))

# Print random effects estimates
print("Random effects: (BLUPs by Athlete)")
print(ranef(final_model))

summary(final_model_p)

merged_tests_clean <- merged_tests_clean %>%
  mutate(predicted_Total = predict(final_model_p, newdata = merged_tests_clean))

ggplot(merged_tests_clean, aes(x = diff_N, y = Total, color = ClusterDateNum)) +
  geom_point() +
  geom_line(aes(y = predicted_Total), color = "black")
