# Driver Alertness Prediction — Ensemble Model (AUC 0.987)

**Tools:** Python · LightGBM · XGBoost · scikit-learn · Pandas  
**Topic:** Binary Classification · Ensemble Methods · Feature Engineering

## Overview

A competitive ML pipeline predicting whether a driver is alert (`is_alert`) using physiological and contextual signals. The final model is a **rank-average ensemble of LightGBM and XGBoost** achieving **AUC = 0.987** on 5-fold stratified cross-validation.

## What's Inside

| File | Description |
|------|-------------|
| `pipeline.py` | End-to-end reproducible pipeline: feature engineering → CV → ensemble → submission |
| `technical-report.pdf` | Full CRISP-DM report with methodology, feature analysis, and results |
| `crisp-dm-methodology.md` | CRISP-DM structured project documentation |
| `feature-importance.csv` | Top predictive features from the final LightGBM model |

> **Note:** The training dataset (~30 MB) is not included due to size. See the report for its structure.

## Model Architecture

```
LightGBM #1  (127 leaves, η=0.04)   ──┐
LightGBM #2  (191 leaves, η=0.025)  ──┤── Rank-average blend → AUC 0.98771
XGBoost      (depth=8)              ──┘
```

## Key Results

| Model | OOF AUC |
|-------|---------|
| LightGBM #1 | 0.98658 |
| LightGBM #2 | 0.98758 |
| XGBoost | 0.97451 |
| **Final Blend** | **0.98771** |
