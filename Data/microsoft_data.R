# Load necessary libraries
library(tseries)
library(DescTools)
library(RVAideMemoire)
library(np)
library(zoo)
library(Kendall)

# Load and clean data
data <- read.csv("Microsoft_Stock.csv")
data <- na.omit(data)

# Summary of data
summary(data)

mean_sd_stats <- function(column, name) {
  mean_val <- mean(column, na.rm = TRUE)
  sd_val <- sd(column, na.rm = TRUE)
  cat(name, "- Mean:", mean_val, "SD:", sd_val, "\n")
}

mean_sd_stats(data$Close, "Close")
mean_sd_stats(data$Open, "Open")
mean_sd_stats(data$High, "High")
mean_sd_stats(data$Low, "Low")
mean_sd_stats(data$Volume, "Volume")

# Exploratory time series plots
par(mfrow = c(2, 2))
plot(data$Open,  type = "l", main = "Time Series of Open Prices",  xlab = "Index", ylab = "Open")
plot(data$High,  type = "l", main = "Time Series of High Prices",  xlab = "Index", ylab = "High")
plot(data$Low,   type = "l", main = "Time Series of Low Prices",   xlab = "Index", ylab = "Low")
plot(data$Close, type = "l", main = "Time Series of Close Prices", xlab = "Index", ylab = "Close")
par(mfrow = c(1, 1))

plot(log(1 + data$Volume), type = "l",
     main = "Time Series of Log(1 + Volume)", xlab = "Index", ylab = "Log(1 + Volume)")

# Run Test for Randomness (raw prices/volume)
run_test_sorted <- function(sorted_data, name) {
  median_val <- median(sorted_data, na.rm = TRUE)
  binary_sequence <- ifelse(sorted_data > median_val, 1, 0)
  test_result <- runs.test(factor(binary_sequence))
  cat(name, "Run Test\n")
  cat("Test Statistic:", test_result$statistic, "\n")
  cat("p-value:", test_result$p.value, "\n\n")
}

run_test_sorted(data$Close,  "Close")
run_test_sorted(data$Open,   "Open")
run_test_sorted(data$High,   "High")
run_test_sorted(data$Low,    "Low")
run_test_sorted(data$Volume, "Volume")

# Mann-Kendall Trend Test
mk_trend_test <- function(series, name) {
  test_result <- MannKendall(series)
  cat(name, "Mann-Kendall Trend Test\n")
  cat("Tau:", test_result$tau, "\n")
  cat("p-value:", test_result$sl, "\n\n")
}

mk_trend_test(data$Close, "Close")
mk_trend_test(data$Open,  "Open")
mk_trend_test(data$High,  "High")
mk_trend_test(data$Low,   "Low")

# Binary (direction-of-change) correlation between Volume and prices
binary_sequence <- function(series) {
  return(as.numeric(series - c(NA, series[-length(series)]) < 0))
}

binary_correlation <- function(dat, var1, var2) {
  seq1 <- na.omit(binary_sequence(dat[[var1]]))
  seq2 <- na.omit(binary_sequence(dat[[var2]]))
  correlation <- sum(seq1 != seq2)
  cat("Binary Correlation between", var1, "and", var2, ":", correlation, "\n")
}

binary_correlation(data, "Volume", "Close")
binary_correlation(data, "Volume", "Open")
binary_correlation(data, "Volume", "High")
binary_correlation(data, "Volume", "Low")

# Log Transformation
data$Log_Close  <- log(data$Close + 1)
data$Log_Open   <- log(data$Open + 1)
data$Log_High   <- log(data$High + 1)
data$Log_Low    <- log(data$Low + 1)
data$Log_Volume <- log(data$Volume + 1)

par(mfrow = c(2, 2))
plot(data$Log_Open,  type = "l", main = "Log-Transformed Open Prices",  xlab = "Index", ylab = "Log(Open)")
plot(data$Log_High,  type = "l", main = "Log-Transformed High Prices",  xlab = "Index", ylab = "Log(High)")
plot(data$Log_Low,   type = "l", main = "Log-Transformed Low Prices",   xlab = "Index", ylab = "Log(Low)")
plot(data$Log_Close, type = "l", main = "Log-Transformed Close Prices", xlab = "Index", ylab = "Log(Close)")
par(mfrow = c(1, 1))

# Detrended Analysis (subtract 20-period moving average)
data$Detrended_Close  <- data$Log_Close  - rollmean(data$Log_Close,  k = 20, fill = NA)
data$Detrended_Open   <- data$Log_Open   - rollmean(data$Log_Open,   k = 20, fill = NA)
data$Detrended_High   <- data$Log_High   - rollmean(data$Log_High,   k = 20, fill = NA)
data$Detrended_Low    <- data$Log_Low    - rollmean(data$Log_Low,    k = 20, fill = NA)
data$Detrended_Volume <- data$Log_Volume - rollmean(data$Log_Volume, k = 20, fill = NA)

par(mfrow = c(2, 2))
plot(data$Detrended_Open,  type = "l", main = "Detrended Open Prices",  xlab = "Index", ylab = "Detrended Log(Open)")
plot(data$Detrended_High,  type = "l", main = "Detrended High Prices",  xlab = "Index", ylab = "Detrended Log(High)")
plot(data$Detrended_Low,   type = "l", main = "Detrended Low Prices",   xlab = "Index", ylab = "Detrended Log(Low)")
plot(data$Detrended_Close, type = "l", main = "Detrended Close Prices", xlab = "Index", ylab = "Detrended Log(Close)")
par(mfrow = c(1, 1))

# Run Test on Detrended Data
run_test_detrended <- function(series, name) {
  median_val <- median(series, na.rm = TRUE)
  binary_sequence <- ifelse(series > median_val, 1, 0)
  test_result <- runs.test(factor(binary_sequence))
  cat(name, "Run Test on Detrended Data\n")
  cat("Test Statistic:", test_result$statistic, "\n")
  cat("p-value:", test_result$p.value, "\n\n")
}

run_test_detrended(na.omit(data$Detrended_Close),  "Detrended Close")
run_test_detrended(na.omit(data$Detrended_Open),   "Detrended Open")
run_test_detrended(na.omit(data$Detrended_High),   "Detrended High")
run_test_detrended(na.omit(data$Detrended_Low),    "Detrended Low")
run_test_detrended(na.omit(data$Detrended_Volume), "Detrended Volume")

# Two-Sample Tests for Location and Scale
scale_test_ansari <- function(column1, column2, name1 = "Column1", name2 = "Column2") {
  test_result <- ansari.test(na.omit(column1), na.omit(column2))
  cat("Ansari-Bradley Scale Test between", name1, "and", name2, "\n")
  cat("Test Statistic:", test_result$statistic, "\n")
  cat("p-value:", test_result$p.value, "\n\n")
}

scale_test_ansari(data$Close, data$Open, "Close", "Open")
scale_test_ansari(data$Close, data$High, "Close", "High")
scale_test_ansari(data$Close, data$Low,  "Close", "Low")
scale_test_ansari(data$Open,  data$High, "Open",  "High")
scale_test_ansari(data$Open,  data$Low,  "Open",  "Low")
scale_test_ansari(data$High,  data$Low,  "High",  "Low")

two_sample_location_test_wilcox <- function(column1, column2, name1 = "Column1", name2 = "Column2") {
  test_result <- wilcox.test(na.omit(column1), na.omit(column2))
  cat("Two-Sample Location Test (Wilcoxon Rank Sum) between", name1, "and", name2, "\n")
  cat("Test Statistic:", test_result$statistic, "\n")
  cat("p-value:", test_result$p.value, "\n\n")
}

two_sample_location_test_wilcox(data$Close, data$Open, "Close", "Open")
two_sample_location_test_wilcox(data$Close, data$High, "Close", "High")
two_sample_location_test_wilcox(data$Close, data$Low,  "Close", "Low")
two_sample_location_test_wilcox(data$Open,  data$High, "Open",  "High")
two_sample_location_test_wilcox(data$Open,  data$Low,  "Open",  "Low")
two_sample_location_test_wilcox(data$High,  data$Low,  "High",  "Low")

ks_test_samples <- function(sample1, sample2, name1 = "Sample1", name2 = "Sample2") {
  sample1 <- na.omit(sample1)
  sample2 <- na.omit(sample2)
  ks_result <- suppressWarnings(ks.test(sample1, sample2, alternative = "two.sided"))
  cat("Kolmogorov-Smirnov Test between", name1, "and", name2, "\n")
  cat("D statistic:", ks_result$statistic, "\n")
  cat("p-value:", ks_result$p.value, "\n\n")
}

ks_test_samples(data$Close, data$Open, "Close", "Open")
ks_test_samples(data$Close, data$High, "Close", "High")
ks_test_samples(data$Close, data$Low,  "Close", "Low")
ks_test_samples(data$Open,  data$High, "Open",  "High")
ks_test_samples(data$Open,  data$Low,  "Open",  "Low")
ks_test_samples(data$High,  data$Low,  "High",  "Low")

# Spearman Rank Correlation (including Volume)
spearman_correlation <- function(column1, column2, name1 = "Column1", name2 = "Column2") {
  result <- suppressWarnings(cor.test(column1, column2, method = "spearman"))
  cat("Spearman Rank Correlation between", name1, "and", name2, "\n")
  cat("Correlation Coefficient:", result$estimate, "\n")
  cat("p-value:", result$p.value, "\n\n")
}

spearman_correlation(data$Close, data$Open,   "Close", "Open")
spearman_correlation(data$Close, data$High,   "Close", "High")
spearman_correlation(data$Close, data$Low,    "Close", "Low")
spearman_correlation(data$Close, data$Volume, "Close", "Volume")
spearman_correlation(data$Open,  data$High,   "Open",  "High")
spearman_correlation(data$Open,  data$Low,    "Open",  "Low")
spearman_correlation(data$Open,  data$Volume, "Open",  "Volume")
spearman_correlation(data$High,  data$Low,    "High",  "Low")
spearman_correlation(data$High,  data$Volume, "High",  "Volume")
spearman_correlation(data$Low,   data$Volume, "Low",   "Volume")

# Non-Parametric Kernel Regression: predict next value from lags
# Create lagged variables (lags 1 to 4)
max_lag <- 4
variables <- c("Close", "Open", "High", "Low", "Volume")

for (var in variables) {
  for (lag in 1:max_lag) {
    lagged_var_name <- paste0(var, "_lag", lag)
    data[[lagged_var_name]] <- c(rep(NA, lag), data[[var]][1:(nrow(data) - lag)])
  }
}

# Remove rows with NA values introduced by lagging
data <- data[(max_lag + 1):nrow(data), ]

# Split into training (first 80%) and testing (last 20%) sets
n <- nrow(data)
train_indices <- 1:floor(0.8 * n)
test_indices  <- (floor(0.8 * n) + 1):n

train_data <- data[train_indices, ]
test_data  <- data[test_indices, ]

# Kernel regression predicting the next value from lagged predictors
# (excluding the lags of the response variable itself)
kernel_regression_next_value <- function(response_name, data_train, data_test) {
  lagged_predictors <- names(data_train)[grepl("_lag[1-4]$", names(data_train))]
  response_lagged_vars <- paste0(response_name, "_lag", 1:max_lag)
  predictors <- setdiff(lagged_predictors, response_lagged_vars)
  
  predictors_str <- paste(predictors, collapse = "+")
  formula_str <- paste(response_name, "~", predictors_str)
  formula <- as.formula(formula_str)
  
  model <- npreg(formula, data = data_train)
  
  predicted_values <- predict(model, newdata = data_test)
  actual_values <- data_test[[response_name]]
  
  mse <- mean((actual_values - predicted_values)^2)
  r_squared <- 1 - (sum((actual_values - predicted_values)^2) / sum((actual_values - mean(actual_values))^2))
  
  cat("Kernel Regression for", response_name, "\n")
  cat("Mean Squared Error:", mse, "\n")
  cat("R-squared:", r_squared, "\n\n")
  
  plot(actual_values, type = "l", main = paste("Kernel Regression for", response_name),
       xlab = "Index", ylab = response_name)
  lines(predicted_values, col = "blue")
  legend("topright", legend = c("Actual", "Predicted"), col = c("black", "blue"), lty = 1)
  
  return(model)
}

models <- list()
for (var in variables) {
  models[[var]] <- kernel_regression_next_value(var, train_data, test_data)
}

