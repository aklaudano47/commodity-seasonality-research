# ==============================================================================
# PROJECT: Systematic Seasonal Alpha in Physical Commodities
# PURPOSE: Audited replication script for portfolio / interview presentation
# AUTHOR:  Alexander K. Laudano
# NOTE:    This revision preserves the original research idea while making
#          transaction-cost, sample, and statistical assumptions explicit.
# ==============================================================================

# 1. LOAD REQUIRED LIBRARIES ---------------------------------------------------
required_packages <- c("readr", "dplyr", "lubridate", "tidyr", "ggplot2", "knitr", "purrr", "scales")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Please install required packages before running: ", paste(missing_packages, collapse = ", "))
}

library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(knitr)
library(purrr)
library(scales)

# 2. LOAD AND VALIDATE DATA ----------------------------------------------------
# Edit this path if the CSV is stored elsewhere.
candidate_files <- c("alex_finaldata.csv", "alex_finaldata(1).csv")
data_file <- candidate_files[file.exists(candidate_files)][1]
if (is.na(data_file)) {
  stop("Dataset not found. Place alex_finaldata.csv in the working directory or edit candidate_files.")
}

full_data <- read_csv(data_file, show_col_types = FALSE)
required_columns <- c("Asset", "Date_Year", "Date_Month", "Quarter", "Monthly_Return")
missing_columns <- setdiff(required_columns, names(full_data))
if (length(missing_columns) > 0) {
  stop("Dataset is missing required columns: ", paste(missing_columns, collapse = ", "))
}

full_data <- full_data %>%
  mutate(
    Date = make_date(Date_Year, Date_Month, 1),
    Expected_Quarter = paste0("Q", quarter(Date))
  ) %>%
  arrange(Asset, Date)

if (any(is.na(full_data$Monthly_Return))) stop("Monthly_Return contains missing values; resolve before analysis.")
if (anyDuplicated(full_data[c("Asset", "Date")]) > 0) stop("Duplicate Asset-Date observations detected.")
if (any(full_data$Quarter != full_data$Expected_Quarter)) stop("Quarter labels do not match Date_Month.")

# 3. RESEARCH CONFIGURATION ---------------------------------------------------
CFG <- list(
  min_train_years = 5,
  cost_bps = 65,
  # "round_trip_once": deduct 65 bps once when entering the selected quarter.
  # Use this if 65 bps represents total cost of one annual quarter-long trade.
  # "per_side": deduct 65 bps at entry and 65 bps at exit (130 bps per selected quarter).
  # "per_month_in_market": reproduces the original script (195 bps per selected quarter).
  cost_mode = "round_trip_once",
  # Original methodology: choose the quarter with the highest mean monthly return in past data.
  # Alternative robustness check below uses mean compounded quarterly return.
  signal_metric = "mean_monthly_return_by_quarter",
  require_complete_years = TRUE,
  perm_reps = 1000,
  seed = 123,
  portfolio_assets = c("LUMBER", "WHEAT", "CORN")
)

valid_cost_modes <- c("round_trip_once", "per_side", "per_month_in_market")
valid_signal_metrics <- c("mean_monthly_return_by_quarter", "mean_compounded_quarterly_return")
if (!(CFG$cost_mode %in% valid_cost_modes)) stop("Invalid cost_mode.")
if (!(CFG$signal_metric %in% valid_signal_metrics)) stop("Invalid signal_metric.")

# 4. HELPER FUNCTIONS ----------------------------------------------------------
keep_complete_calendar_years <- function(asset_data) {
  complete_years <- asset_data %>%
    group_by(Date_Year) %>%
    summarise(n_months = n_distinct(Date_Month), .groups = "drop") %>%
    filter(n_months == 12) %>%
    pull(Date_Year)
  asset_data %>% filter(Date_Year %in% complete_years)
}

choose_quarter <- function(training_data, signal_metric) {
  if (signal_metric == "mean_monthly_return_by_quarter") {
    stats <- training_data %>%
      group_by(Quarter) %>%
      summarise(Score = mean(Monthly_Return, na.rm = TRUE), .groups = "drop")
  } else {
    stats <- training_data %>%
      group_by(Date_Year, Quarter) %>%
      summarise(Quarter_Return = (prod(1 + Monthly_Return / 100, na.rm = TRUE) - 1) * 100,
                .groups = "drop") %>%
      group_by(Quarter) %>%
      summarise(Score = mean(Quarter_Return, na.rm = TRUE), .groups = "drop")
  }
  stats %>% arrange(desc(Score), Quarter) %>% slice(1) %>% pull(Quarter)
}

apply_transaction_costs <- function(test_set, chosen_quarter, cfg) {
  result <- test_set %>%
    mutate(
      Chosen_Q = chosen_quarter,
      In_Market = as.integer(Quarter == chosen_quarter),
      Gross_Strat_Ret = if_else(In_Market == 1, Monthly_Return, 0),
      Cost_Pct = 0
    )

  traded_rows <- which(result$In_Market == 1)
  if (length(traded_rows) == 0) return(result %>% mutate(Strat_Ret = Gross_Strat_Ret))

  cost_pct <- cfg$cost_bps / 100
  if (cfg$cost_mode == "round_trip_once") {
    result$Cost_Pct[traded_rows[1]] <- cost_pct
  } else if (cfg$cost_mode == "per_side") {
    result$Cost_Pct[traded_rows[1]] <- cost_pct
    result$Cost_Pct[traded_rows[length(traded_rows)]] <- result$Cost_Pct[traded_rows[length(traded_rows)]] + cost_pct
  } else if (cfg$cost_mode == "per_month_in_market") {
    result$Cost_Pct[traded_rows] <- cost_pct
  }

  result %>% mutate(Strat_Ret = Gross_Strat_Ret - Cost_Pct)
}

run_backtest <- function(asset_data, cfg) {
  q_data <- asset_data %>% arrange(Date)
  if (cfg$require_complete_years) q_data <- keep_complete_calendar_years(q_data)

  years <- sort(unique(q_data$Date_Year))
  if (length(years) <= cfg$min_train_years) return(NULL)

  oos_list <- list()
  for (i in (cfg$min_train_years + 1):length(years)) {
    train_years <- years[1:(i - 1)]
    test_year <- years[i]
    training_set <- q_data %>% filter(Date_Year %in% train_years)
    test_set <- q_data %>% filter(Date_Year == test_year) %>% arrange(Date)
    chosen_quarter <- choose_quarter(training_set, cfg$signal_metric)
    oos_list[[as.character(test_year)]] <- apply_transaction_costs(test_set, chosen_quarter, cfg)
  }

  bind_rows(oos_list) %>%
    arrange(Date) %>%
    mutate(
      Wealth_Strat = cumprod(1 + Strat_Ret / 100),
      Wealth_BH = cumprod(1 + Monthly_Return / 100),
      Peak_Strat = cummax(Wealth_Strat),
      DD_Strat = (Wealth_Strat - Peak_Strat) / Peak_Strat
    )
}

# This is a full-sample seasonality diagnostic, not an out-of-sample p-value for
# the trading strategy. It tests whether quarterly mean dispersion is unusual
# after randomly reassigning quarter labels across observed monthly returns.
seasonal_dispersion_pvalue <- function(asset_data, cfg) {
  set.seed(cfg$seed)
  observed <- asset_data %>%
    group_by(Quarter) %>%
    summarise(M = mean(Monthly_Return, na.rm = TRUE), .groups = "drop") %>%
    summarise(Spread = max(M) - min(M)) %>%
    pull(Spread)

  null_distribution <- replicate(cfg$perm_reps, {
    shuffled <- asset_data %>%
      mutate(Shuffled_Quarter = sample(Quarter)) %>%
      group_by(Shuffled_Quarter) %>%
      summarise(M = mean(Monthly_Return, na.rm = TRUE), .groups = "drop")
    max(shuffled$M) - min(shuffled$M)
  })

  # Plus-one correction: with a finite Monte Carlo sample, never report a
  # literal p-value of zero. With 1,000 permutations, the minimum reported
  # value is approximately 0.001 rather than 0.
  (sum(null_distribution >= observed) + 1) / (cfg$perm_reps + 1)
}

summarise_backtest <- function(asset_name, wf_res, p_value) {
  years_in_test <- nrow(wf_res) / 12
  strategy_ann <- (tail(wf_res$Wealth_Strat, 1)^(1 / years_in_test) - 1) * 100
  benchmark_ann <- (tail(wf_res$Wealth_BH, 1)^(1 / years_in_test) - 1) * 100
  tibble(
    Asset = asset_name,
    OOS_Start = min(wf_res$Date),
    OOS_End = max(wf_res$Date),
    Ann_Return = strategy_ann,
    Benchmark_Return = benchmark_ann,
    Excess_Return = strategy_ann - benchmark_ann,
    Sharpe = mean(wf_res$Strat_Ret) / sd(wf_res$Strat_Ret) * sqrt(12),
    Max_DD = min(wf_res$DD_Strat) * 100,
    Full_Sample_Seasonality_P = p_value
  )
}

run_all_assets <- function(data, cfg) {
  assets <- unique(data$Asset)
  backtests <- list()
  summary_rows <- list()

  for (a in assets) {
    asset_data <- data %>% filter(Asset == a)
    wf_res <- run_backtest(asset_data, cfg)
    if (!is.null(wf_res)) {
      p_value <- seasonal_dispersion_pvalue(asset_data, cfg)
      backtests[[a]] <- wf_res
      summary_rows[[a]] <- summarise_backtest(a, wf_res, p_value)
    }
  }

  summary_table <- bind_rows(summary_rows) %>%
    mutate(
      Adjusted_P = p.adjust(Full_Sample_Seasonality_P, method = "BH"),
      Seasonal_Diagnostic = case_when(
        Adjusted_P <= 0.05 ~ "Full-sample seasonality diagnostic significant after BH adjustment",
        Full_Sample_Seasonality_P <= 0.05 ~ "Full-sample seasonality diagnostic nominal only; not significant after adjustment",
        TRUE ~ "Full-sample seasonality diagnostic not significant"
      )
    ) %>%
    arrange(desc(Excess_Return))

  list(summary = summary_table, backtests = backtests)
}

build_portfolio <- function(backtests, portfolio_assets) {
  missing_assets <- setdiff(portfolio_assets, names(backtests))
  if (length(missing_assets) > 0) stop("Portfolio assets missing backtests: ", paste(missing_assets, collapse = ", "))

  return_series <- map(portfolio_assets, function(a) {
    backtests[[a]] %>% select(Date, !!a := Strat_Ret)
  })

  portfolio_data <- reduce(return_series, inner_join, by = "Date") %>%
    arrange(Date) %>%
    mutate(
      Portfolio_Ret = rowMeans(across(all_of(portfolio_assets))),
      Wealth = cumprod(1 + Portfolio_Ret / 100),
      Peak = cummax(Wealth),
      Drawdown = (Wealth - Peak) / Peak
    )

  years <- nrow(portfolio_data) / 12
  summary <- tibble(
    Strategy = paste("Illustrative Equal-Weight Composite:", paste(portfolio_assets, collapse = " + ")),
    Start = min(portfolio_data$Date),
    End = max(portfolio_data$Date),
    Ann_Return = (tail(portfolio_data$Wealth, 1)^(1 / years) - 1) * 100,
    Max_DD = min(portfolio_data$Drawdown) * 100,
    Sharpe = mean(portfolio_data$Portfolio_Ret) / sd(portfolio_data$Portfolio_Ret) * sqrt(12)
  )

  list(data = portfolio_data, summary = summary)
}

# 5. PRIMARY ANALYSIS ----------------------------------------------------------
primary <- run_all_assets(full_data, CFG)
portfolio <- build_portfolio(primary$backtests, CFG$portfolio_assets)

results_display <- primary$summary %>%
  mutate(across(c(Ann_Return, Benchmark_Return, Excess_Return, Sharpe, Max_DD,
                  Full_Sample_Seasonality_P, Adjusted_P), ~ round(.x, 4)))
portfolio_display <- portfolio$summary %>%
  mutate(across(c(Ann_Return, Max_DD, Sharpe), ~ round(.x, 4)))

print(kable(results_display,
            caption = paste0("Recursive OOS Results | Cost mode: ", CFG$cost_mode,
                             " | ", CFG$cost_bps, " bps")))
print(kable(portfolio_display,
            caption = "Illustrative Portfolio Summary (selection is not itself validated out-of-sample)"))

# Save primary output for reproducibility
write_csv(primary$summary, "oos_asset_results_audited.csv")
write_csv(portfolio$summary, "portfolio_summary_audited.csv")

# 6. ROBUSTNESS CHECKS ---------------------------------------------------------
# Shows how results change under alternative cost and signal assumptions.
robustness_configs <- tribble(
  ~Scenario, ~cost_mode, ~signal_metric,
  "Original cost implementation", "per_month_in_market", "mean_monthly_return_by_quarter",
  "65 bps once per selected quarter", "round_trip_once", "mean_monthly_return_by_quarter",
  "65 bps per entry and exit", "per_side", "mean_monthly_return_by_quarter",
  "Quarter-compounded signal; 65 bps once", "round_trip_once", "mean_compounded_quarterly_return"
)

robustness_table <- pmap_dfr(robustness_configs, function(Scenario, cost_mode, signal_metric) {
  scenario_cfg <- CFG
  scenario_cfg$cost_mode <- cost_mode
  scenario_cfg$signal_metric <- signal_metric
  scenario_results <- run_all_assets(full_data, scenario_cfg)
  scenario_portfolio <- build_portfolio(scenario_results$backtests, scenario_cfg$portfolio_assets)
  scenario_portfolio$summary %>%
    transmute(
      Scenario = Scenario,
      Ann_Return = Ann_Return,
      Max_DD = Max_DD,
      Sharpe = Sharpe
    )
}) %>%
  mutate(across(c(Ann_Return, Max_DD, Sharpe), ~ round(.x, 4)))

print(kable(robustness_table, caption = "Portfolio Robustness Across Implementation Assumptions"))
write_csv(robustness_table, "portfolio_robustness_checks.csv")

# 7. FIGURES -------------------------------------------------------------------
cost_subtitle <- paste0("Equal-weight selected-asset composite | Cost mode: ", CFG$cost_mode,
                        " (", CFG$cost_bps, " bps)")

equity_plot <- ggplot(portfolio$data, aes(x = Date, y = Wealth)) +
  geom_area(fill = "#2ecc71", alpha = 0.2) +
  geom_line(color = "#27ae60", linewidth = 1.1) +
  labs(title = "Illustrative Commodity Composite: Cumulative Growth of $1",
       subtitle = cost_subtitle,
       x = "Year", y = "Growth of $1") +
  theme_minimal()

comparison_plot_data <- primary$summary %>%
  filter(Asset %in% c(CFG$portfolio_assets, "DOWJONES")) %>%
  transmute(Asset, Value = Excess_Return, Type = if_else(Asset == "DOWJONES", "Benchmark Asset", "Commodity Asset")) %>%
  bind_rows(tibble(Asset = "Composite", Value = portfolio$summary$Ann_Return, Type = "Illustrative Portfolio Return"))

comparison_plot <- ggplot(comparison_plot_data,
                          aes(x = reorder(Asset, -Value), y = Value, fill = Type)) +
  geom_col(color = "black") +
  labs(title = "Out-of-Sample Strategy Results",
       subtitle = "Asset excess returns; composite shown as annualized return",
       x = NULL, y = "Annualized return metric (%)") +
  theme_minimal()

print(equity_plot)
print(comparison_plot)

ggsave("audited_equity_curve.png", equity_plot, width = 9, height = 5.5, dpi = 300)
ggsave("audited_result_comparison.png", comparison_plot, width = 9, height = 5.5, dpi = 300)

# END OF SCRIPT ---------------------------------------------------------------
