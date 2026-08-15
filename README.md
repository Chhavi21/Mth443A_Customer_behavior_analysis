# Customer Behavior Analysis for Campaign Optimization

Statistical analysis of a 200,000-record marketing campaign dataset to study customer segmentation, spending behavior, and campaign response. Course project for **MTH443A – Statistical & AI Techniques in Data Mining**, IIT Kanpur, under Prof. Amit Mitra.

## What's in here

- **EDA** — income segmentation (Low/Medium/High) by education, family structure, and spending category
- **PCA** — dimensionality reduction to identify the main axes of variance across customer attributes
- **Clustering** — K-means and Gaussian Mixture Models, tested against known demographic segments
- **Classification** — LDA, QDA, and Naive Bayes to predict Education, Marital Status, Age, and Income Segment

## Key Findings

- Income Segment shows the clearest separation in PCA space; other variables show minimal separation.
- Unsupervised clustering (K-means, GMM) does **not** cleanly recover predefined demographic segments.
- **LDA on Income Segment performed best overall** (16.1% misclassification rate) among all models and class variables tested.
- Low-income customers spend significantly less on luxury products (e.g. gold products) than medium/high-income segments.

## Structure

```
Data/      → dataset (csv, xlsx) + analysis scripts (EDA.R, analysis.R)
Report/    → full write-up (Rmd + PDF)
```

## Running it

```r
install.packages(c("dplyr", "lubridate", "purrr", "corrplot", "reshape2",
                    "ggplot2", "cluster", "factoextra", "plotly", "e1071",
                    "MASS", "caret", "mice"))

source("Data/EDA.R")
source("Data/analysis.R")
# or render the full report:
rmarkdown::render("Report/report.Rmd")
```
