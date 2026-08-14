# 📈 Microsoft Stock Data Analysis
A non-parametric statistical analysis of Microsoft's historical stock data — exploring price trends, randomness, and volume patterns — with a complementary analysis on an unrelated audio-feature dataset (cats vs. dogs) to validate the methodology.

## 📄 Read the Report / View the Slides
### [Open the Full Report (PDF)](https://drive.google.com/drive/folders/1w6cgtKQ7u49pAkOJKC1egjF5Fs1_d8gx) · [Open the Presentation (PDF)](https://docs.google.com/presentation/d/1yyYT6Wf2hGscNIkstiZvm-ZQwCGkpu19/edit?slide=id.p1#slide=id.p1)

<!-- If you host either one online later (RPubs, Google Slides, GitHub Pages), swap the links above -->

## 📊 Project
This project uses **R, tseries, DescTools, RVAideMemoire, np, zoo, and Kendall** to:
- Test Microsoft's stock prices and volume for randomness and trend
- Detrend log-transformed price series using moving-average smoothing
- Compare Open, High, Low, and Close statistically (location, scale, and distribution)
- Measure monotonic relationships between price variables and trading volume
- Apply the same non-parametric toolkit to a cat/dog audio-feature dataset as a cross-domain check

## 🔍 Analysis Sections
**Exploratory Data Analysis** · **Run Test for Randomness** · **Mann-Kendall Trend Test** · **Log Transformation & Detrending** · **Two-Sample Location & Scale Tests** · **Spearman Rank Correlation** · **Cats vs. Dogs Audio Analysis**

## ✅ Key Findings
- Strong, significant upward trend in all price variables (Mann-Kendall τ ≈ 0.93)
- Raw price/volume series are highly non-random; randomness increases substantially after detrending
- Open, High, Low, and Close are statistically indistinguishable in location and scale
- Volume shows only a weak relationship with price direction
- The same non-parametric tests hold up cleanly on the unrelated cats/dogs dataset

## 📁 Structure
```text
├── data/                # Datasets and R analysis scripts
│   ├── Microsoft_Stock.csv
│   ├── cat_and_dog.csv
│   ├── microsoft_data.R
│   └── cats_dogs_data.R
├── report/               # Full written report (R Markdown + PDF)
│   ├── report.Rmd
│   ├── report.pdf
│   └── README.md
├── presentation/         # Slide deck
│   ├── Microsoft_Stock_Data_Analysis_Slides.pdf
│   └── README.md
└── README.md
```

