# Load necessary libraries
library(tseries)
library(DescTools)
library(RVAideMemoire)
library(np)

# Load data (replace with your file path if needed)
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
data$cat_pitch_z <- normalize(data$cat_pitch)
data$dog_pitch_z <- normalize(data$dog_pitch)
data$cat_spectral_centroid_z <- normalize(data$cat_spectral_centroid)
data$dog_spectral_centroid_z <- normalize(data$dog_spectral_centroid)
data$cat_zero_crossing_rate_z <- normalize(data$cat_zero_crossing_rate)
data$dog_zero_crossing_rate_z <- normalize(data$dog_zero_crossing_rate)

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
  # Combine the two samples
  combined <- c(column1, column2)
  # Calculate overall median
  overall_median <- median(combined, na.rm = TRUE)
  # Divide observations based on the overall median
  group1 <- ifelse(column1 <= overall_median, "Below_or_Equal", "Above")
  group2 <- ifelse(column2 <= overall_median, "Below_or_Equal", "Above")
  # Create contingency table
  cont <- table(
    Group = c(rep(name1, length(column1)),
              rep(name2, length(column2))),
    Median_Category = c(group1, group2)
  )
  # Chi-square approximation instead of exact Fisher test
  test_result <- chisq.test(cont, correct = FALSE)
  cat("Mood's Median Test between", name1, "and", name2, "\n")
  cat("Overall Median:", overall_median, "\n")
  cat("p-value:", test_result$p.value, "\n\n")
  return(test_result)
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
scale_test_ansari <- function(column1, column2,
                              name1 = "Column1",
                              name2 = "Column2") {
  test_result <- ansari.test(
    column1,
    column2,
    exact = FALSE
  )
  cat("Ansari-Bradley Scale Test between",
      name1, "and", name2, "\n")
  cat("p-value:", test_result$p.value, "\n\n")
  return(test_result)
}

scale_test_ansari(data$cat_pitch, data$dog_pitch, "cat_pitch", "dog_pitch")
scale_test_ansari(data$cat_spectral_centroid, data$dog_spectral_centroid, "cat_spectral_centroid", "dog_spectral_centroid")
scale_test_ansari(data$cat_zero_crossing_rate, data$dog_zero_crossing_rate, "cat_zero_crossing_rate", "dog_zero_crossing_rate")

# Fligner-Killeen Test: Pitch
pitch_data <- data.frame(
  Value = c(data$cat_pitch, data$dog_pitch),
  Group = factor(c(
    rep("Cat", nrow(data)),
    rep("Dog", nrow(data))
  ))
)
fligner_pitch <- fligner.test(Value ~ Group, data = pitch_data)
cat("Fligner-Killeen Test for Pitch\n")
cat("p-value:", fligner_pitch$p.value, "\n\n")

# Fligner-Killeen Test: Spectral Centroid
spectral_data <- data.frame(
  Value = c(data$cat_spectral_centroid,
            data$dog_spectral_centroid),
  Group = factor(c(
    rep("Cat", nrow(data)),
    rep("Dog", nrow(data))
  ))
)
fligner_spectral <- fligner.test(Value ~ Group, data = spectral_data)
cat("Fligner-Killeen Test for Spectral Centroid\n")
cat("p-value:", fligner_spectral$p.value, "\n\n")

# Fligner-Killeen Test: Zero Crossing Rate
zcr_data <- data.frame(
  Value = c(data$cat_zero_crossing_rate,
            data$dog_zero_crossing_rate),
  Group = factor(c(
    rep("Cat", nrow(data)),
    rep("Dog", nrow(data))
  ))
)
fligner_zcr <- fligner.test(Value ~ Group, data = zcr_data)
cat("Fligner-Killeen Test for Zero Crossing Rate\n")
cat("p-value:", fligner_zcr$p.value, "\n\n")

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

# You can repeat the kernel regression for other response variables as needed
