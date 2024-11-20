# Load necessary libraries
library(tseries)
library(DescTools)
library(RVAideMemoire)
library(np)

# Load and clean data (replace with your file path if needed)
data <- read.csv("cat_and_dog.csv")
data <- na.omit(data)

# Function to normalize data
normalize <- function(column) {
  return((column - mean(column, na.rm = TRUE)) / sd(column, na.rm = TRUE))
# Load necessary libraries
library(tseries)
library(DescTools)
library(RVAideMemoire)
library(np)

# Load and clean data (replace with your file path if needed)
data <- read.csv("cat_and_dog.csv")
data <- na.omit(data)

# Function to normalize data
normalize <- function(column) {
  return((column - mean(column, na.rm = TRUE)) / sd(column, na.rm = TRUE))
}

# Run Test for Randomness
run_test <- function(data_column, name) {
  median_val <- median(data_column, na.rm = TRUE)
  binary_sequence <- ifelse(data_column > median_val, 1, 0)
  test_result <- runs.test(factor(binary_sequence))
  cat(name, "Run Test p-value:", test_result$p.value, "\n")
}

# Normalize columns
data$cat_pitch <- normalize(data$cat_pitch)
data$dog_pitch <- normalize(data$dog_pitch)
data$cat_spectral_centroid <- normalize(data$cat_spectral_centroid)
data$dog_spectral_centroid <- normalize(data$dog_spectral_centroid)
data$cat_zero_crossing_rate <- normalize(data$cat_zero_crossing_rate)
data$dog_zero_crossing_rate <- normalize(data$dog_zero_crossing_rate)

# Run test on each column
run_test(data$cat_pitch, "cat_pitch")
run_test(data$dog_pitch, "dog_pitch")
run_test(data$cat_spectral_centroid, "cat_spectral_centroid")
run_test(data$dog_spectral_centroid, "dog_spectral_centroid")
run_test(data$cat_zero_crossing_rate, "cat_zero_crossing_rate")
run_test(data$dog_zero_crossing_rate, "dog_zero_crossing_rate")

# Mean and Standard Deviation
mean_sd_stats <- function(column, name) {
  mean_val <- mean(column, na.rm = TRUE)
  sd_val <- sd(column, na.rm = TRUE)
  cat(name, "- Mean:", mean_val, "SD:", sd_val, "\n")
}

mean_sd_stats(data$cat_pitch, "cat_pitch")
mean_sd_stats(data$dog_pitch, "dog_pitch")
mean_sd_stats(data$cat_spectral_centroid, "cat_spectral_centroid")
mean_sd_stats(data$dog_spectral_centroid, "dog_spectral_centroid")
mean_sd_stats(data$cat_zero_crossing_rate, "cat_zero_crossing_rate")
mean_sd_stats(data$dog_zero_crossing_rate, "dog_zero_crossing_rate")

# Mood's Median Test for Central Tendency Differences
mood_median_test <- function(column1, column2, name1 = "Column1", name2 = "Column2") {
  test_result <- suppressWarnings(mood.medtest(column1, column2))
  cat("Mood's Median Test between", name1, "and", name2, "p-value:", test_result$p.value, "\n")
}

mood_median_test(data$cat_pitch, data$dog_pitch, "cat_pitch", "dog_pitch")
mood_median_test(data$cat_spectral_centroid, data$dog_spectral_centroid, "cat_spectral_centroid", "dog_spectral_centroid")
mood_median_test(data$cat_zero_crossing_rate, data$dog_zero_crossing_rate, "cat_zero_crossing_rate", "dog_zero_crossing_rate")

# Wilcoxon Rank Sum Test for Location Differences
two_sample_wilcox_test <- function(column1, column2, name1 = "Column1", name2 = "Column2") {
  test_result <- suppressWarnings(wilcox.test(column1, column2))
  cat("Wilcoxon Rank Sum Test between", name1, "and", name2, "p-value:", test_result$p.value, "\n")
}

two_sample_wilcox_test(data$cat_pitch, data$dog_pitch, "cat_pitch", "dog_pitch")
two_sample_wilcox_test(data$cat_spectral_centroid, data$dog_spectral_centroid, "cat_spectral_centroid", "dog_spectral_centroid")
two_sample_wilcox_test(data$cat_zero_crossing_rate, data$dog_zero_crossing_rate, "cat_zero_crossing_rate", "dog_zero_crossing_rate")

# Kolmogorov-Smirnov Test for Distribution Differences
two_sample_ks_test <- function(column1, column2, name1 = "Column1", name2 = "Column2") {
  test_result <- suppressWarnings(ks.test(column1, column2))
  cat("Kolmogorov-Smirnov Test between", name1, "and", name2, "p-value:", test_result$p.value, "\n")
}

two_sample_ks_test(data$cat_pitch, data$dog_pitch, "cat_pitch", "dog_pitch")
two_sample_ks_test(data$cat_spectral_centroid, data$dog_spectral_centroid, "cat_spectral_centroid", "dog_spectral_centroid")
two_sample_ks_test(data$cat_zero_crossing_rate, data$dog_zero_crossing_rate, "cat_zero_crossing_rate", "dog_zero_crossing_rate")

# Ansari-Bradley Scale Test
scale_test_ansari <- function(column1, column2, name1 = "Column1", name2 = "Column2") {
  test_result <- ansari.test(column1, column2)
  cat("Ansari-Bradley Scale Test between", name1, "and", name2, "p-value:", test_result$p.value, "\n")
}

scale_test_ansari(data$cat_pitch, data$dog_pitch, "cat_pitch", "dog_pitch")
scale_test_ansari(data$cat_spectral_centroid, data$dog_spectral_centroid, "cat_spectral_centroid", "dog_spectral_centroid")
scale_test_ansari(data$cat_zero_crossing_rate, data$dog_zero_crossing_rate, "cat_zero_crossing_rate", "dog_zero_crossing_rate")

# Fligner-Killeen Test for Scale Differences across Multiple Columns
data_long <- data.frame(
  Value = c(data$cat_pitch, data$dog_pitch, data$cat_spectral_centroid, data$dog_spectral_centroid, 
            data$cat_zero_crossing_rate, data$dog_zero_crossing_rate),
  Group = factor(rep(c("cat_pitch", "dog_pitch", "cat_spectral_centroid", "dog_spectral_centroid", 
                       "cat_zero_crossing_rate", "dog_zero_crossing_rate"), each = nrow(data)))
)
fligner_test_result <- fligner.test(Value ~ Group, data = data_long)
cat("Fligner-Killeen Test for Scale Differences p-value:", fligner_test_result$p.value, "\n")

# Spearman Rank and Kendall Tau Correlations
spearman_correlation <- function(column1, column2, name1 = "Column1", name2 = "Column2") {
  result <- suppressWarnings(cor.test(column1, column2, method = "spearman"))
  cat("Spearman Rank Correlation between", name1, "and", name2, "\n")
  cat("Correlation Coefficient:", result$estimate, "\n")
  cat("p-value:", result$p.value, "\n\n")
}

kendall_correlation <- function(column1, column2, name1 = "Column1", name2 = "Column2") {
  result <- suppressWarnings(cor.test(column1, column2, method = "kendall"))
  cat("Kendall Tau Correlation between", name1, "and", name2, "\n")
  cat("Correlation Coefficient:", result$estimate, "\n")
  cat("p-value:", result$p.value, "\n\n")
}

# Apply to pairs of variables for Spearman and Kendall
spearman_correlation(data$cat_pitch, data$dog_pitch, "cat_pitch", "dog_pitch")
kendall_correlation(data$cat_pitch, data$dog_pitch, "cat_pitch", "dog_pitch")

spearman_correlation(data$cat_spectral_centroid, data$dog_spectral_centroid, "cat_spectral_centroid", "dog_spectral_centroid")
kendall_correlation(data$cat_spectral_centroid, data$dog_spectral_centroid, "cat_spectral_centroid", "dog_spectral_centroid")

spearman_correlation(data$cat_zero_crossing_rate, data$dog_zero_crossing_rate, "cat_zero_crossing_rate", "dog_zero_crossing_rate")
kendall_correlation(data$cat_zero_crossing_rate, data$dog_zero_crossing_rate, "cat_zero_crossing_rate", "dog_zero_crossing_rate")

# Non-Parametric Kernel-Based Regression
kernel_regression_with_plot <- function(response, predictors, response_name = "Response") {
  regression_data <- data.frame(Response = response, predictors)
  formula <- as.formula(paste("Response ~", paste(names(predictors), collapse = "+")))
  model <- npreg(formula, data = regression_data)
  fitted_values <- fitted(model)
  mse <- mean((response - fitted_values)^2)
  r_squared <- 1 - (sum((response - fitted_values)^2) / sum((response - mean(response))^2))
  cat("Kernel Regression for", response_name, "\n")
  cat("Mean Squared Error:", mse, "\n")
  cat("R-squared:", r_squared, "\n\n")
  plot(response, main = paste("Kernel Regression for", response_name), xlab = "Index", ylab = response_name)
  points(fitted_values, col = "blue", pch = 16)
  legend("topright", legend = c("Actual", "Estimated"), col = c("black", "blue"), pch = c(1, 16))
  return(model)
}

# Example for cat_pitch as response
predictors <- data[, c("dog_pitch", "cat_spectral_centroid", "dog_spectral_centroid", 
                       "cat_zero_crossing_rate", "dog_zero_crossing_rate")]
cat_pitch_model <- kernel_regression_with_plot(data$cat_pitch, predictors, response_name = "cat_pitch")

# Example for dog_pitch as response
dog_pitch_model <- kernel_regression_with_plot(data$dog_pitch, predictors, response_name = "dog_pitch")

# Create lagged variables (lags 1 to 4)
max_lag <- 4
variables <- c("Close", "Open", "High", "Low", "Volume")

for (var in variables) {
  for (lag in 1:max_lag) {
    lagged_var_name <- paste0(var, "_lag", lag)
    data[[lagged_var_name]] <- c(rep(NA, lag), data[[var]][1:(nrow(data) - lag)])
  }
}

# Remove rows with NA values due to lagging
data <- data[(max_lag + 1):nrow(data), ]

# Split the data into training (first 80%) and testing (last 20%) sets
n <- nrow(data)
train_indices <- 1:floor(0.8 * n)
test_indices <- (floor(0.8 * n) + 1):n

train_data <- data[train_indices, ]
test_data <- data[test_indices, ]

# Function for kernel regression predicting the next value
kernel_regression_next_value <- function(response_name, data_train, data_test) {
  # Predictors are lagged variables of all variables excluding the lags of the response variable
  lagged_predictors <- names(data_train)[grepl("_lag[1-4]$", names(data_train))]
  response_lagged_vars <- paste0(response_name, "_lag", 1:max_lag)
  predictors <- setdiff(lagged_predictors, response_lagged_vars)
  
  # Create formula for regression
  predictors_str <- paste(predictors, collapse = "+")
  formula_str <- paste(response_name, "~", predictors_str)
  formula <- as.formula(formula_str)
  
  # Fit kernel regression model
  model <- npreg(formula, data = data_train)
  
  # Predict on test data
  predicted_values <- predict(model, newdata = data_test)
  
  # Actual values
  actual_values <- data_test[[response_name]]
  
  # Calculate statistics
  mse <- mean((actual_values - predicted_values)^2)
  r_squared <- 1 - (sum((actual_values - predicted_values)^2) / sum((actual_values - mean(actual_values))^2))
  
  # Print statistics
  cat("Kernel Regression for", response_name, "\n")
  cat("Mean Squared Error:", mse, "\n")
  cat("R-squared:", r_squared, "\n\n")
  
  # Plot actual vs. predicted values
  plot(actual_values, type = "l", main = paste("Kernel Regression for", response_name),
       xlab = "Index", ylab = response_name)
  lines(predicted_values, col = "blue")
  legend("topright", legend = c("Actual", "Predicted"), col = c("black", "blue"), lty = 1)
  
  return(model)
}

# Apply kernel regression for each variable as the response
models <- list()

for (var in variables) {
  models[[var]] <- kernel_regression_next_value(var, train_data, test_data)
}