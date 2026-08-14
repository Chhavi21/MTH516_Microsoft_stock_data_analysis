# Report

The full written report for the Microsoft Stock Data Analysis project.

## Contents

- **`report.Rmd`** — R Markdown source with all code, statistical tests, and narrative explanations. Knit to PDF to reproduce `report.pdf` from scratch.
- **`report.pdf`** — Compiled version, ready to read without running any code.

## Methodology, in order

1. **Summary statistics & EDA** — means, medians, SDs, and time series plots of Open/High/Low/Close/Volume.
2. **Run test for randomness** — tests whether values above/below the median cluster in time.
3. **Mann-Kendall trend test** — confirms a significant monotonic upward trend in all four price variables (τ ≈ 0.93).
4. **Log transformation + detrending** — removes the exponential trend by subtracting a 20-period moving average of the log-transformed series.
5. **Run test on detrended data** — re-checks randomness after the trend is removed; randomness increases but doesn't fully disappear.
6. **Two-sample location & scale tests** — Wilcoxon Rank Sum (location) and Ansari-Bradley (scale) comparing Open/High/Low/Close pairwise.
7. **Two-sample distribution comparison** — Kolmogorov-Smirnov test across the same pairs.
8. **Spearman rank correlation** — monotonic relationships between all price variables and Volume.
9. **Complementary analysis** — the same non-parametric approach applied to cat/dog audio features (Pitch, Spectral Centroid, Zero Crossing Rate), including Ansari-Bradley and Fligner-Killeen tests.
10. **Future work** — using the run test as a stopping criterion for ML residual analysis, and extending non-parametric feature selection to other data types (images, text).

## Reproducing the report

Open `report.Rmd` in RStudio, make sure `Microsoft_Stock.csv` and `cat_and_dog.csv` (from [`../data`](../data)) are in the working directory, install the packages listed in the [root README](../README.md), and knit to PDF.

<!-- ## Read online

Optional: link a hosted/rendered version, e.g. RPubs or GitHub Pages
[Read the full report (PDF)](./report.pdf) -->
