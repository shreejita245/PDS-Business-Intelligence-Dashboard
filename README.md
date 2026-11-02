# Business Performance Intelligence Platform

An interactive R Shiny dashboard for profit analytics, anomaly detection, and ML-driven business intelligence on the Superstore retail dataset.

---

## Features

| Tab | What it does |
|-----|-------------|
| **Command Center** | KPI cards — total sales, profit, anomaly count, avg margin, loss exposure |
| **Anomaly Hunter** | Filterable table of 2,845 anomalous transactions with exportable CSV |
| **What-If Simulator** | Drag discount sliders → see predicted profit in real time (Random Forest) |
| **Discount Optimiser** | Per-category optimal discount thresholds from EDA |
| **Regional Risk** | Region-level anomaly rate & loss exposure chart |
| **Sub-Category Intel** | Loss rate ranking for all 17 sub-categories |
| **ML Lab** | Per-model interactive charts for all 15 ML models (10 regression + 5 classification) |

---

## ML Models

### Regression — Profit Prediction
| Model | RMSE | R² |
|-------|------|-----|
| Linear Regression | — | — |
| Ridge Regression | — | — |
| Lasso Regression | — | — |
| Decision Tree | — | — |
| Random Forest | — | — |
| Gradient Boosting | — | — |
| **SVR** | **57.46** | **0.852** |
| KNN (k=7) | — | — |
| Random Forest (Tuned) | — | — |
| GBM (Tuned) | — | — |

### Classification — Loss Detection
| Model | Accuracy | AUC |
|-------|----------|-----|
| Logistic Regression | — | — |
| Naive Bayes | — | — |
| Decision Tree | — | — |
| **Random Forest** | **91.2%** | **0.96** |
| SVM | — | — |

---

## Setup

### Prerequisites
```r
install.packages(c("shiny", "bslib", "plotly", "DT", "dplyr", "randomForest",
                   "glmnet", "gbm", "e1071", "pROC", "gridExtra", "rpart"))
```

### Data
Place these files in `C:/Users/<you>/Downloads/` (or update paths in `app.R`):
- `superstore_with_anomalies.csv`
- `discount_thresholds.csv`
- `regional_risk.csv`
- `subcategory_analysis.csv`
- `profit_model.rds`

### Run ML Lab charts (optional but recommended)
```r
source("model_plots.R")   # generates reg_preds_all.csv, cls_probs_all.csv, confusion_all.csv
```

### Launch dashboard
```r
shiny::runApp("app.R")
```

---

## File Structure

```
PDS_project/
├── app.R                  # Main Shiny dashboard
├── model_plots.R          # Trains all 15 models, saves CSVs + PNGs
├── model_profit.R         # Random Forest profit model (for What-If tab)
├── anomaly_detection.R    # Isolation Forest anomaly pipeline
├── models_comparison.R    # Model benchmarking & comparison
├── explainability.R       # SHAP / feature importance
├── layers.R               # Data preprocessing utilities
├── db_setup.R             # Optional DB integration
└── PDS_project.Rproj      # RStudio project file
```

---

## Tech Stack
- **Language**: R
- **Framework**: Shiny + bslib (GitHub dark theme)
- **Visualisation**: plotly, DT
- **ML**: randomForest, glmnet, gbm, e1071, rpart, pROC

---

## Dataset
[Superstore Sales Dataset](https://www.kaggle.com/datasets/vivek468/superstore-dataset-final) — 9,994 retail transactions across US regions (2014–2017).
