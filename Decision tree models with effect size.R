# athletedates_analysis_refactored.R

# ─────────────────────────────────────────────────────────────────────────────
# 0) Load required packages
# ─────────────────────────────────────────────────────────────────────────────
library(readr)      # read_csv()
library(dplyr)      # data manipulation
library(lubridate)  # date parsing
library(tidyr)      # drop_na()
library(caret)      # train workflow
library(rpart)      # decision trees (used by caret)
library(effsize)    # for later effect‐size calculations (Cohen’s d)

# ─────────────────────────────────────────────────────────────────────────────
# 1) Import all CSVs, parse dates, and drop any data frames without valid Date
# ─────────────────────────────────────────────────────────────────────────────
file_paths <- list.files(path = ".", pattern = "\\.csv$", full.names = TRUE)
clean_names <- tools::file_path_sans_ext(basename(file_paths)) %>% make.names()

df_list <- setNames(lapply(seq_along(file_paths), function(i) {
  path <- file_paths[i]
  nm   <- clean_names[i]
  df   <- read_csv(path, show_col_types = FALSE)
  date_col <- names(df)[tolower(names(df)) %in% c("date", "testdate")] %>% head(1)
  if (length(date_col) == 0) {
    message("Skipping '", nm, "': no Date column found.")
    return(NULL)
  }
  parsed <- tryCatch(
    parse_date_time(df[[date_col]], orders = c("ymd", "mdy", "dmy")),
    warning = function(w) { message("Warning parsing dates in '", nm, "': ", w$message); rep(NA, nrow(df)) },
    error   = function(e) { message("Error parsing dates in '", nm, "': ", e$message); rep(NA, nrow(df)) }
  )
  df[[date_col]] <- as_date(parsed)
  if (all(is.na(df[[date_col]]))) {
    message("Dropping '", nm, "': all Date values failed to parse.")
    return(NULL)
  }
  if (date_col != "Date") names(df)[names(df) == date_col] <- "Date"
  df
}), clean_names)

# Remove NULL entries
df_list <- Filter(Negate(is.null), df_list)

# ─────────────────────────────────────────────────────────────────────────────
# 2) Combine all tests into one long data frame, sorted by Athlete & Date
# ─────────────────────────────────────────────────────────────────────────────
all_tests <- bind_rows(df_list, .id = "TestGroup") %>%
  select(Athlete, Date, TestGroup, everything()) %>%
  arrange(Athlete, Date)

# ─────────────────────────────────────────────────────────────────────────────
# 3) Cluster same‐day tests per Athlete, then compute mean of metrics per cluster
# ─────────────────────────────────────────────────────────────────────────────
metric_cols <- setdiff(names(all_tests), c("Athlete", "Date", "TestGroup"))

merged_tests_clean <- all_tests %>%
  arrange(Athlete, Date) %>%
  group_by(Athlete) %>%
  mutate(
    diff_days   = as.numeric(Date - lag(Date, default = first(Date))),
    new_cluster = if_else(row_number() == 1 | diff_days > 1, 1L, 0L),
    cluster_id  = cumsum(new_cluster)
  ) %>%
  group_by(Athlete, cluster_id) %>%
  summarise(
    ClusterDate = min(Date),
    across(all_of(metric_cols), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  drop_na()  # Drop any rows with missing metrics

# ─────────────────────────────────────────────────────────────────────────────
# 4) Convert left/right measures to numeric and compute diffs in one mutate
# ─────────────────────────────────────────────────────────────────────────────
merged_tests_clean <- merged_tests_clean %>%
  mutate(
    across(
      c(`L Max Force (N)`, `R Max Force (N)`,
        `L Max Torque (Nm)`, `R Max Torque (Nm)`,
        `L Max Impulse (Ns)`, `R Max Impulse (Ns)`),
      as.double
    ),
    force_diff    = `L Max Force (N)`   - `R Max Force (N)`,
    torque_diff   = `L Max Torque (Nm)`  - `R Max Torque (Nm)`,
    impulse_diff  = `L Max Impulse (Ns)` - `R Max Impulse (Ns)`,
    max_imb       = `Max Imbalance (%)`,
    avg_imb       = `Avg Imbalance (%)`,
    impulse_imb   = `Impulse Imbalance (%)`
  ) %>%
  drop_na(Total, force_diff, torque_diff, impulse_diff)

# ─────────────────────────────────────────────────────────────────────────────
# 5) Decision Tree with 10‐Fold CV via caret
# ─────────────────────────────────────────────────────────────────────────────
set.seed(710)
cp_values <- seq(0.001, 0.05, length.out = 10)
tune_grid <- expand.grid(cp = cp_values)

tc <- trainControl(
  method      = "cv",
  number      = 10,
  verboseIter = TRUE
)

tree_formula <- Total ~ force_diff + torque_diff + impulse_diff

tree_fit <- train(
  tree_formula,
  data      = merged_tests_clean,
  method    = "rpart",
  trControl = tc,
  tuneGrid  = tune_grid
)

print(tree_fit)
final_tree <- tree_fit$finalModel
print(final_tree)

foldwise_rmse <- tree_fit$resample %>%
  select(Resample, RMSE)
print(foldwise_rmse)

# ─────────────────────────────────────────────────────────────────────────────
# 6) Per‐Split Cohen’s d Helper and Computation
# ─────────────────────────────────────────────────────────────────────────────
compute_cohens_d_fixed <- function(data, split_var, split_value,
                                   direction = c("lt", "ge"), outcome = "Total",
                                   zero_replace = FALSE) {
  direction <- match.arg(direction)
  idx_left  <- if (direction == "lt") {
    data[[split_var]] < split_value
  } else {
    data[[split_var]] >= split_value
  }
  grpL <- data[idx_left,    , drop = FALSE]
  grpR <- data[!idx_left,   , drop = FALSE]
  
  nL   <- nrow(grpL);   nR   <- nrow(grpR)
  varL <- if (nL >= 2) var(grpL[[outcome]]) else NA_real_
  varR <- if (nR >= 2) var(grpR[[outcome]]) else NA_real_
  
  message(sprintf(
    "[%s %s %.4f]  n_left=%d  var_left=%s  |  n_right=%d  var_right=%s",
    split_var,
    ifelse(direction == "lt", "<", ">="),
    split_value,
    nL,
    ifelse(is.na(varL), "NA", sprintf("%.4f", varL)),
    nR,
    ifelse(is.na(varR), "NA", sprintf("%.4f", varR))
  ))
  
  if (nL < 2 || nR < 2) {
    warning(" → One side has fewer than 2 observations. Returning NA.")
    return(list(estimate = NA_real_, conf.int = c(NA_real_, NA_real_)))
  }
  if (varL == 0 || varR == 0) {
    warning(" → Zero variance in outcome for one side. Returning NA.")
    if (!zero_replace) {
      return(list(estimate = NA_real_, conf.int = c(NA_real_, NA_real_)))
    } else {
      return(list(estimate = Inf, conf.int = c(Inf, Inf)))
    }
  }
  
  cd_out <- tryCatch(
    cohen.d(grpL[[outcome]], grpR[[outcome]],
            hedges.correction = TRUE, pooled = TRUE),
    error = function(e) {
      warning(" → cohen.d() error: ", e$message); NA_real_
    }
  )
  if (!is.list(cd_out)) {
    return(list(estimate = NA_real_, conf.int = c(NA_real_, NA_real_)))
  }
  list(estimate = cd_out$estimate, conf.int = cd_out$conf.int)
}

print(final_tree)
# Identify splits from final_tree output, for example:
#   1) root ...
#      2) force_diff < 3.125 ...
#         4) torque_diff < -23.99 ...
#         5) torque_diff >= -23.99 ...
#            10) impulse_diff < -71.82 ...
#            11) impulse_diff >= -71.82 ...
#               22) impulse_diff < -453.2 ...
#               23) impulse_diff >= -453.2 ...
#      3) force_diff >= 3.125 ...
#         6) impulse_diff < 129.1 ...
#         7) impulse_diff >= 129.1 ...

subset_A <- merged_tests_clean
subset_B <- filter(merged_tests_clean, force_diff < 3.125)
subset_C <- filter(merged_tests_clean, force_diff < 3.125, torque_diff >= -23.99)
subset_D <- filter(merged_tests_clean, force_diff < 3.125, torque_diff >= -23.99, impulse_diff >= -71.82)
subset_E <- filter(merged_tests_clean, force_diff >= 3.125)

dA <- compute_cohens_d_fixed(subset_A, "force_diff",  3.125,  "lt", "Total")
dB <- compute_cohens_d_fixed(subset_B, "torque_diff", -23.99, "lt", "Total")
dC <- compute_cohens_d_fixed(subset_C, "impulse_diff", -71.82, "lt", "Total")
dD <- compute_cohens_d_fixed(subset_D, "impulse_diff", -453.2, "lt", "Total")
dE <- compute_cohens_d_fixed(subset_E, "impulse_diff", 129.1,  "lt", "Total")

results_effect_sizes <- tibble(
  split_name = c(
    "A: force_diff < 3.125",
    "B: torque_diff < -23.99 (given A)",
    "C: impulse_diff < -71.82 (given A & B-right)",
    "D: impulse_diff < -453.2 (given A & B-right & C-right)",
    "E: impulse_diff < 129.1 (given force_diff ≥ 3.125)"
  ),
  n_left  = c(
    sum(merged_tests_clean$force_diff   < 3.125),
    sum(subset_B$torque_diff            < -23.99),
    sum(subset_C$impulse_diff           < -71.82),
    sum(subset_D$impulse_diff           < -453.2),
    sum(subset_E$impulse_diff           < 129.1)
  ),
  n_right = c(
    sum(merged_tests_clean$force_diff   >= 3.125),
    sum(subset_B$torque_diff            >= -23.99),
    sum(subset_C$impulse_diff           >= -71.82),
    sum(subset_D$impulse_diff           >= -453.2),
    sum(subset_E$impulse_diff           >= 129.1)
  ),
  cohen_d  = c(dA$estimate, dB$estimate, dC$estimate, dD$estimate, dE$estimate),
  lower_CI = c(dA$conf.int[1], dB$conf.int[1], dC$conf.int[1], dD$conf.int[1], dE$conf.int[1]),
  upper_CI = c(dA$conf.int[2], dB$conf.int[2], dC$conf.int[2], dD$conf.int[2], dE$conf.int[2])
)

print(results_effect_sizes)
# ─────────────────────────────────────────────────────────────────────────────
# End of refactored script
# ─────────────────────────────────────────────────────────────────────────────
print(tree_fit)
final_tree <- tree_fit$finalModel

# 1) Base‐R plot + text (will open a graphics window or render in your IDE):
plot(final_tree, uniform = TRUE, margin = 0.1)
text(final_tree, use.n = TRUE, all = TRUE, cex = 0.8)

# 2) If you want a fancier, more readable plot, install/load rpart.plot:
if (!requireNamespace("rpart.plot", quietly = TRUE)) {
  install.packages("rpart.plot")
}
library(rpart.plot)
rpart.plot::rpart.plot(final_tree, 
                       type = 3,        # draw split labels below nodes
                       extra = 101,     # display n, percentage, and predicted value
                       fallen.leaves = TRUE,
                       cex = 0.8)
