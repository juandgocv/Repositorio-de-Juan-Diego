# Student Academic Performance — Multiple Regression Analysis

**Tools:** R · ggplot2 · car · lmtest · MASS · caret  
**Dataset:** Students Performance (1,000 students, 8 variables)  
**Topic:** Linear Regression · Model Diagnostics · Inferential Statistics

## Overview

A two-part regression study analyzing the factors that explain math scores among high school students. The analysis spans model building, assumption testing, influence analysis, and model selection with a final interpretable equation.

## What's Inside

| File | Description |
|------|-------------|
| `regression-analysis.R` | Full R script: data prep → model building → diagnostics → selection |
| `report-1-exploratory-regression.pdf` | Report 1: Exploratory analysis and initial model |
| `report-2-advanced-regression.pdf` | Report 2: Advanced diagnostics, influential points, and final model |
| `students-performance-dataset.csv` | Dataset with demographics, socioeconomic variables, and test scores |

## Key Variables

- **Response:** Math score
- **Predictors:** Reading score, writing score, gender, ethnic group, parental education, lunch type, test preparation

## Techniques Applied

- Multiple linear regression with stepwise and manual selection
- Assumption diagnostics: normality, homoscedasticity, autocorrelation
- Outlier and leverage analysis (Cook's distance, hat values)
- VIF for multicollinearity assessment
