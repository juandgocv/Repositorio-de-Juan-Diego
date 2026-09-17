# COLCAP Volatility vs. TRM: Causal Inference

Preliminary statistical analysis for my undergraduate thesis. Tests whether
the Colombian peso exchange rate (TRM) has a significant relationship with
the COLCAP stock index using hypothesis testing and causal inference
(Granger causality, cointegration) on 4,524 daily paired observations
(2008-2026), ahead of full ML modeling (LSTM, Random Forest, XGBoost) under
CRISP-DM.

**Key finding:** statistically significant but economically weak short-term
relationship (bidirectional Granger causality, p < 0.05), with no long-run
cointegration (p = 0.79) between price levels.

Tools: Python, pandas, statsmodels, scipy
