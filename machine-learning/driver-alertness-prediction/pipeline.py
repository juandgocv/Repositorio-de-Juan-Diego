"""
================================================================
 Taller 2 — Fundamentos de Aprendizaje de Máquina
 Clasificación binaria: ¿el conductor está alerta? (is_alert)
 Métrica de competencia: ROC-AUC
================================================================

Pipeline reproducible end-to-end.

Diseño (justificación en el informe):
  1. EDA: 350K filas, 11 features numéricas, sin NaNs, mild imbalance
     (59% clase 1). V11 (horas de sueño) y V10 (horas desde la última
     siesta) son las variables más informativas a nivel univariado.
  2. Feature Engineering físicamente motivado: razones e interacciones
     entre sueño / fatiga / alcohol / vía / edad. Log-transform sobre
     variables de cola larga (V3, V6, V8). Total: 26 features.
  3. Modelado: ensamble heterogéneo con CV estratificada 5-fold.
        - LightGBM #1 (127 hojas, η=0.04)
        - LightGBM #2 (191 hojas, η=0.025, regularización fuerte)
        - XGBoost   (depth=8)
        - LR L2     (datos escalados — modelo lineal de referencia)
  4. Stacking por rank-average (invariante a escala monotónica,
     óptimo para AUC). Búsqueda de pesos en OOF.
  5. Sometimiento: submission.csv (id, is_alert).

Resultado OOF (5-fold estratificada):
  LGB1=0.98658  LGB2=0.98758  XGB=0.97451  LR=0.77191
  Blend final (0.24·LGB1 + 0.76·LGB2)  AUC = 0.98771
"""

import os, time, warnings, numpy as np, pandas as pd
import lightgbm as lgb, xgboost as xgb
from sklearn.model_selection import StratifiedKFold
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import roc_auc_score
from scipy.stats import rankdata
warnings.filterwarnings("ignore")

# --------------------------- CONFIG ---------------------------
DATA_PATH    = "alerta_train.xlsx"                 # debe estar en cwd
SUB_PATH     = "submission.csv"
FEATIMP_PATH = "feature_importance.csv"
SEED         = 42
N_SPLITS     = 5

# --------------------------- 1. CARGA ---------------------------
print("[1/5] Cargando datos...")
tr = pd.read_excel(DATA_PATH, sheet_name="train")
pr = pd.read_excel(DATA_PATH, sheet_name="predecir")
y   = tr["isalert"].astype(int).values
ids = pr["id"].values
print(f"  train={tr.shape}  predecir={pr.shape}  balance={y.mean():.3f}")

# --------------------- 2. FEATURE ENGINEERING --------------------
def fe(df: pd.DataFrame) -> pd.DataFrame:
    """Feature engineering basado en la fisiología del problema.
    Justificación:
      - sleep_minus_since y sleep_per_since miden el margen entre el
        sueño disponible y el tiempo despierto desde la última siesta
        (proxy directo de fatiga acumulada — teoría de la información:
        condensan dos variables informativas en una con mayor IG).
      - fatigue_alcohol, alcohol_per_sleep capturan interacciones
        multiplicativas que un árbol descubre solas pero que ayudan
        a un modelo lineal regularizado a ser competitivo.
      - log1p sobre V3, V6, V8 reduce el efecto de outliers / colas
        largas — vinculado al tratamiento robusto del curso.
      - speed_x_width combina velocidad y amplitud de la vía
        (factor exógeno de riesgo).
    """
    d = df.copy()
    d["sleep_minus_since"] = d["V11"] - d["V10"]
    d["sleep_per_since"]   = d["V11"] / (d["V10"] + 0.5)
    d["sleep_x_notfatig"]  = d["V11"] * (1 - d["V5"])
    d["fatigue_alcohol"]   = d["V5"] * d["V8"]
    d["alcohol_per_sleep"] = d["V8"] / (d["V11"] + 0.1)
    d["nap_x_alcohol"]     = d["V10"] * d["V8"]
    d["woke_abs"]          = d["V2"].abs()
    d["speed_x_width"]     = d["V4"] * d["P6"]
    d["speed_per_width"]   = d["V4"] / (d["P6"] + 1)
    d["exp_lic_per_age"]   = d["P2"] / (d["P1"] + 1)
    d["age_minus_exp"]     = d["P1"] - d["P2"]
    d["V6_log"]            = np.log1p(d["V6"])
    d["V3_log"]            = np.log1p(d["V3"])
    d["V8_log"]            = np.log1p(d["V8"])
    d["alcohol_any"]       = (d["V8"] > 0).astype(int)
    return d

print("[2/5] Feature engineering...")
X_tr = fe(tr.drop(columns=["isalert"])).astype("float32")
X_pr = fe(pr.drop(columns=["id", "isalert"])).astype("float32")
FEATS = list(X_tr.columns)
print(f"  features: {len(FEATS)}")

# Escalado SOLO para el modelo lineal (LR / KNN-like)
scaler = StandardScaler()
X_tr_s = scaler.fit_transform(X_tr.values).astype("float32")
X_pr_s = scaler.transform(X_pr.values).astype("float32")

# ----------------------- 3. MODELOS BASE -----------------------
print("[3/5] Entrenando ensamble con CV 5-fold estratificada...")

skf = StratifiedKFold(n_splits=N_SPLITS, shuffle=True, random_state=SEED)

oof_lgb1 = np.zeros(len(y));  pr_lgb1 = np.zeros(len(X_pr))
oof_lgb2 = np.zeros(len(y));  pr_lgb2 = np.zeros(len(X_pr))
oof_xgb  = np.zeros(len(y));  pr_xgb  = np.zeros(len(X_pr))
oof_lr   = np.zeros(len(y));  pr_lr   = np.zeros(len(X_pr))

LGB1 = dict(n_estimators=2500, learning_rate=0.04,  num_leaves=127,
            min_child_samples=30, subsample=0.9, colsample_bytree=0.85,
            reg_lambda=1.0, random_state=42, n_jobs=-1, verbose=-1)

LGB2 = dict(n_estimators=2200, learning_rate=0.035, num_leaves=191,
            min_child_samples=20, subsample=0.85, colsample_bytree=0.8,
            reg_lambda=2.0, reg_alpha=0.1, random_state=7, n_jobs=-1, verbose=-1)

XGBP = dict(n_estimators=2500, learning_rate=0.05, max_depth=8,
            min_child_weight=20, subsample=0.9, colsample_bytree=0.85,
            reg_lambda=1.0, tree_method="hist", eval_metric="auc",
            early_stopping_rounds=60, random_state=42, n_jobs=-1, verbosity=0)

for f, (ti, vi) in enumerate(skf.split(X_tr, y)):
    t0 = time.time()
    Xt, Xv = X_tr.values[ti], X_tr.values[vi]
    Xts, Xvs = X_tr_s[ti], X_tr_s[vi]
    yt, yv = y[ti], y[vi]

    m1 = lgb.LGBMClassifier(**LGB1)
    m1.fit(Xt, yt, eval_set=[(Xv, yv)], callbacks=[lgb.early_stopping(60, verbose=False)])
    oof_lgb1[vi] = m1.predict_proba(Xv)[:,1]; pr_lgb1 += m1.predict_proba(X_pr)[:,1]/N_SPLITS

    m2 = lgb.LGBMClassifier(**LGB2)
    m2.fit(Xt, yt, eval_set=[(Xv, yv)], callbacks=[lgb.early_stopping(80, verbose=False)])
    oof_lgb2[vi] = m2.predict_proba(Xv)[:,1]; pr_lgb2 += m2.predict_proba(X_pr)[:,1]/N_SPLITS

    mx = xgb.XGBClassifier(**XGBP)
    mx.fit(Xt, yt, eval_set=[(Xv, yv)], verbose=False)
    oof_xgb[vi]  = mx.predict_proba(Xv)[:,1];  pr_xgb  += mx.predict_proba(X_pr)[:,1]/N_SPLITS

    ml = LogisticRegression(C=1.0, penalty="l2", solver="lbfgs", max_iter=1500, n_jobs=-1)
    ml.fit(Xts, yt)
    oof_lr[vi]   = ml.predict_proba(Xvs)[:,1]; pr_lr   += ml.predict_proba(X_pr_s)[:,1]/N_SPLITS

    print(f"  Fold {f}: LGB1={roc_auc_score(yv, oof_lgb1[vi]):.5f} "
          f"LGB2={roc_auc_score(yv, oof_lgb2[vi]):.5f} "
          f"XGB={roc_auc_score(yv, oof_xgb[vi]):.5f} "
          f"LR={roc_auc_score(yv, oof_lr[vi]):.5f}  t={time.time()-t0:.0f}s")

print(f"\nOOF AUC  LGB1={roc_auc_score(y,oof_lgb1):.5f}  LGB2={roc_auc_score(y,oof_lgb2):.5f}  "
      f"XGB={roc_auc_score(y,oof_xgb):.5f}  LR={roc_auc_score(y,oof_lr):.5f}")

# ----------------------- 4. STACKING / BLEND -----------------------
print("[4/5] Buscando pesos óptimos de blend (rank-average)...")
def rk(a): return rankdata(a) / len(a)
r1o,r2o,rxo,rlo = rk(oof_lgb1), rk(oof_lgb2), rk(oof_xgb), rk(oof_lr)
r1t,r2t,rxt,rlt = rk(pr_lgb1),  rk(pr_lgb2),  rk(pr_xgb),  rk(pr_lr)

best, bw = -1, None
grid = np.round(np.arange(0.0, 1.01, 0.1), 2)
for w1 in grid:
    for w2 in grid:
        if w1+w2 > 1+1e-9: continue
        for wx in grid:
            if w1+w2+wx > 1+1e-9: continue
            wl = round(1 - w1 - w2 - wx, 2)
            if wl < 0: continue
            a = roc_auc_score(y, w1*r1o + w2*r2o + wx*rxo + wl*rlo)
            if a > best: best, bw = a, (w1,w2,wx,wl)

# fine-tune LGB1/LGB2
best2, bw2 = -1, None
for w1 in np.arange(0.0, 1.001, 0.02):
    w2 = 1 - w1
    a = roc_auc_score(y, w1*r1o + w2*r2o)
    if a > best2: best2, bw2 = a, (round(w1,2), round(w2,2))
print(f"  Best 4-way      : {best:.5f}  w={bw}")
print(f"  Best LGB1/LGB2  : {best2:.5f}  w={bw2}")

if best2 >= best:
    w1,w2,wx,wl = bw2[0], bw2[1], 0.0, 0.0; best = best2
else:
    w1,w2,wx,wl = bw

final = w1*r1t + w2*r2t + wx*rxt + wl*rlt

# ----------------------- 5. ENTREGABLE -----------------------
print("[5/5] Guardando archivos finales...")
sub = pd.DataFrame({"id": ids, "is_alert": final})
sub.to_csv(SUB_PATH, index=False)
print(f"  {SUB_PATH} ({len(sub)} filas)  AUC OOF final = {best:.5f}")

# Importancia de features (modelo único en todo el train para reporte)
m_full = lgb.LGBMClassifier(**LGB2); m_full.fit(X_tr.values, y)
imp = pd.DataFrame({
    "feature": FEATS,
    "gain":    m_full.booster_.feature_importance(importance_type="gain"),
    "split":   m_full.booster_.feature_importance(importance_type="split"),
}).sort_values("gain", ascending=False)
imp.to_csv(FEATIMP_PATH, index=False)
print(f"  {FEATIMP_PATH}")
print("\nTop 10 features por ganancia:")
print(imp.head(10).to_string(index=False))
