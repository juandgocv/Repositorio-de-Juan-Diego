# Athlete Performance — Principal Component Analysis

**Tools:** R · ggplot2 · corrplot · psych · FactoMineR  
**Dataset:** Olympic Athletes Performance Data  
**Topic:** Multivariate Statistics · Dimensionality Reduction · PCA

## Overview

Principal Component Analysis (PCA) applied to a dataset of athlete performance metrics to identify latent dimensions of athletic performance. The analysis reduces high-dimensional sport variables into interpretable components and visualizes athlete clusters in reduced space.

## What's Inside

| File | Description |
|------|-------------|
| `pca-analysis.R` | R script: MVN testing → correlation analysis → PCA → component interpretation |
| `technical-report.pdf` | Full report with multivariate normality tests, scree plot, loadings, and biplots |
| `athletes-dataset.xlsx` | Athletic performance dataset |

## Highlights

- Multivariate normality testing (Mardia, Henze-Zirkler)
- Correlation matrix visualization
- PCA with varimax rotation and component loading interpretation
- Biplot and scatter plots for athlete profiling
