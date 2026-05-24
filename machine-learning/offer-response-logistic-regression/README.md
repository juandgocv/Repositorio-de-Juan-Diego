# Marketing Offer Response Prediction — Logistic Regression

**Tools:** Python · scikit-learn · imbalanced-learn · Pandas · Matplotlib  
**Topic:** Binary Classification · Class Imbalance · SMOTE

## Overview

Predicts whether a customer will purchase a tech offer (`respuest = 1`) from a dataset of 6,400 records and 28 predictors. Three models are compared to handle class imbalance: a baseline model, a cost-sensitive model with `class_weight='balanced'`, and a **SMOTE-augmented model**.

## What's Inside

| File | Description |
|------|-------------|
| `offer-response-prediction.ipynb` | Full pipeline: EDA → preprocessing → 3 model comparison → interpretation |
| `dataset.xlsx` | Dataset with customer demographics and behavioral features |

## Pipeline Structure

1. EDA and variable exploration  
2. Preprocessing: ordinal encoding, binary encoding, OHE  
3. Model without balancing (baseline)  
4. Model with `class_weight='balanced'`  
5. Model with SMOTE oversampling  
6. Comparative evaluation (Precision, Recall, F1, ROC-AUC)  
7. Coefficient interpretation and business insights  
