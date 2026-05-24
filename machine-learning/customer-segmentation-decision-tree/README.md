# Customer Segmentation: K-Means + Decision Tree

**Tools:** Python · scikit-learn · Pandas · Seaborn  
**Topic:** Unsupervised Learning · Clustering · Classification

## Overview

A two-stage ML pipeline for customer analytics: first, **K-Means clustering** discovers natural customer segments from purchase behavior and demographics. Then, a **Decision Tree** is trained on those segments to create a predictive model — allowing fast classification of new customers without running the full clustering pipeline.

## What's Inside

| File | Description |
|------|-------------|
| `customer-segmentation.ipynb` | Full pipeline: EDA → K-Means clustering → Decision Tree classifier → evaluation |
| `technical-report.pdf` | Technical report with business context, methodology and results |

> **Note:** The original dataset (~200 MB) is not included in the repository due to size constraints. The notebook documents its structure and source.

## Highlights

- Optimal number of clusters determined via Elbow Method and Silhouette Score
- Decision Tree trained on K-Means labels as a downstream classification task
- Feature importance analysis and customer profile interpretation
- Business-oriented conclusions for marketing strategy and personalization
