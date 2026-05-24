library(dplyr)
library(ggplot2)
library(car)
library(lmtest)
library(MASS)
library(caret)
library(ggrepel)    # etiquetas en gráficos de influencia
library(gridExtra)  # grid.arrange para páneles múltiples

set.seed(1234)

#CARGA Y PREPARACIÓN DE DATOS


#IMPORTANTE: CAMBIAR ESTO POR LA UBICACIÓN DONDE SE TENGA
ruta <- "C:/Users/juanm/Downloads/StudentsPerformance.csv"
df_i <- read.csv(ruta, 
                   stringsAsFactors = FALSE)

#Pasamos los nombres a español para tratarlos con mayor facilidad

names(df_i) <- c("genero", "grupo_etnico", "educ_padres", "almuerzo",
                   "preparacion", "punt_mate", "punt_lect", "punt_escr")
#Convertimos a factores y ajustamos los nombres al español
df <- df_i %>%
  mutate(
    genero = factor(genero,
                    levels = c("female", "male"),
                    labels = c("Femenino", "Masculino")),
    grupo_etnico = factor(grupo_etnico,
                          levels = paste("group", c("A","B","C","D","E"))),
    educ_padres = factor(educ_padres,
                         levels = c("some high school", "high school",
                                    "some college", "associate's degree",
                                    "bachelor's degree", "master's degree"),
                         labels = c("Bach_inc", "Bachillerato", "Algo_univ",
                                    "Tecnico", "Universitario", "Posgrado")),
    almuerzo = factor(almuerzo,
                      levels = c("free/reduced", "standard"),
                      labels = c("Subsidiado", "Estandar")),
    preparacion = factor(preparacion,
                         levels = c("none", "completed"),
                         labels = c("Sin_prep", "Completo")),
    # Variable recodificada del Reporte 1 (grupo E vs. resto (A-D)
    grupo_E = factor(ifelse(grupo_etnico == "group E", "GrupoE", "OtroGrupo"),
                     levels = c("OtroGrupo", "GrupoE"))
  )
cat("Datos cargados:", nrow(df), "observaciones,", ncol(df), "variables\n")
cat("Valores faltantes:", sum(is.na(df)), "\n")#Para observar que todo haya
#Quedado de la manera adecuada



# MODELO BASE DEL REPORTE 1 (llamado m5)



m5 <- lm(punt_mate ~ punt_lect + punt_escr + genero + grupo_E +
            educ_padres + almuerzo + preparacion, data = df)

#Modelo base (inicio del Reporte 2)
summary(m5)
#Algunos indicadores específicos del modelo si se quiere solo verlos individualmente
cat("R² =", round(summary(m5)$r.squared, 4),
    "| R²adj =", round(summary(m5)$adj.r.squared, 4),
    "| AIC =", round(AIC(m5), 1),
    "| RSE =", round(summary(m5)$sigma, 3), "\n")





#VARIABLES CONFUSORAS





#Analizamos el efecto de la variable almuerzo con el puntaje en matemáticas junto con otras variables
m_alm_bruto   <- lm(punt_mate ~ almuerzo, data = df)
m_alm_parcial <- lm(punt_mate ~ almuerzo + genero + grupo_E +
                      educ_padres + preparacion, data = df)
m_alm_completo    <- m5   # incluye punt_lect y punt_escr

coef_alm <- round(c(
  Bruto      = coef(m_alm_bruto)["almuerzoEstandar"],
  Parcial    = coef(m_alm_parcial)["almuerzoEstandar"],
  Completo = coef(m_alm_completo)["almuerzoEstandar"]
), 3)
print(coef_alm)

reduccion <- as.numeric((1 - coef_alm[3] / coef_alm[1]) * 100)
cat(sprintf("Reduccion al controlar por academicas: %.1f%%\n", reduccion))

# Reduccion al controlar por academicas: 70.9%
# El coeficiente de almuerzo cae de 11.1 (bruto) a 3.2 (M5) al incluir
# punt_lect y punt_escr. Esto indica que punt_lect y punt_escr son
# confusores parciales de la relacion almuerzo -> punt_mate:
# el 70.9% del efecto bruto del almuerzo era en realidad el efecto
# indirecto del nivel socioeconomico sobre el desempeno academico previo.
# El efecto directo neto del almuerzo es 3.2 puntos (p < 0.001),
# que persiste significativo despues de controlar.


#Punto 2: puntajes académicos como confusores del efecto
#De 'preparacion' sobre punt_mate


m_prep_bruto   <- lm(punt_mate ~ preparacion, data = df)
m_prep_parcial <- lm(punt_mate ~ preparacion + genero + grupo_E +
                       educ_padres + almuerzo, data = df)
m_prep_completo <- m5

coef_prep <- round(c(
  Bruto    = unname(coef(m_prep_bruto)["preparacionCompleto"]),
  Parcial  = unname(coef(m_prep_parcial)["preparacionCompleto"]),
  Completo = unname(coef(m_prep_completo)["preparacionCompleto"])
), 3)
print(coef_prep)
#Inversion de signo (discutido en Reortep 1).\n")

#Medias observadas por grupo de preparacion
print(tapply(df$punt_mate, df$preparacion, mean) %>% round(2))


# Gráfico comparativo de efectos brutos vs completo
efectos_df <- data.frame(
  Variable = rep(c("almuerzo (Estandar)",
                   "preparacion (Completo)"), each = 3),
  Nivel    = rep(c("Bruto", "Parcial", "Completo"), 2),
  Coef     = c(coef_alm, coef_prep)
)
efectos_df$Nivel <- factor(efectos_df$Nivel,
                           levels = c("Bruto", "Parcial", "Completo"))

ggplot(efectos_df, aes(x = Nivel, y = Coef, fill = Nivel)) +
  geom_col(alpha = 0.85, width = 0.65) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ Variable, scales = "free_y") +
  scale_fill_manual(values = c("steelblue","#f0a500","firebrick")) +
  labs(title    = "Efecto de variables categoricas segun nivel de control",
       subtitle = "Comparacion del coeficiente bruto, parcial y completamente controlado",
       x = NULL, y = "Coeficiente estimado") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 10))

# Coeficientes de preparacion segun nivel:
#   Bruto:    +5.618  → completar el curso de preparación se asocia
#                        con 5.6 puntos MAS en matematicas
#   Parcial:  +5.381  → al controlar por genero, grupo etnico, educ. padres
#                        y almuerzo, el efecto apenas cambia (+5.4)
#   Completo: -3.463  → al agregar punt_lect y punt_escr al modelo (M5),
#                        el signo SE INVIERTE: -3.5 puntos

# Medias observadas:
#   Sin preparacion: 64.08  |  Completo el curso: 69.70
#   Diferencia bruta: +5.62 puntos a favor de quienes completaron el curso

# ¿Por que se invierte el signo?  →  SESGO DE SELECCION
# Los estudiantes que se inscriben al curso son, en promedio, quienes
# parten con puntajes MAS BAJOS en lectura y escritura (se inscriben
# precisamente porque necesitan refuerzo). Al meter punt_lect y punt_escr
# al modelo, R compara estudiantes con el MISMO perfil academico previo.
# Dentro de ese grupo igualado, quienes completaron el curso puntuan
# 3.5 puntos menos de lo esperado para su nivel.

# CONCLUSION: punt_lect y punt_escr son confusores totales de la relacion
# preparacion -> punt_mate. El efecto bruto positivo (+5.6) era enteramente
# explicado por el perfil academico previo, no por el curso en si mismo.
# El modelo NO implica que el curso perjudique el rendimiento; indica que,
# igualados los perfiles, los inscritos ya venian mal y el curso
# no alcanzó a compensar esa diferencia de partida.



# SECCIÓN 2: INTERACCIONES ENTRE VARIABLES




#genero x almuerzo (categorica x categorica) ----
m_int_ga <- lm(punt_mate ~ punt_lect + punt_escr + genero * almuerzo +
                 grupo_E + educ_padres + preparacion, data = df)
print(round(coef(summary(m_int_ga))
            [grep("genero|almuerzo", rownames(coef(summary(m_int_ga)))), ], 4))

anova_ga <- anova(m5, m_int_ga)
print(anova_ga)

df_med_ga <- df %>%
  group_by(genero, almuerzo) %>%
  summarise(media = mean(punt_mate),
            ee    = sd(punt_mate)/sqrt(n()), .groups = "drop")

ggplot(df_med_ga, aes(x = almuerzo, y = media,
                       color = genero, group = genero)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = media - 1.96*ee, ymax = media + 1.96*ee),
                width = 0.12, linewidth = 0.8) +
  scale_color_manual(values = c("firebrick","steelblue")) +
  labs(title    = "Interaccion: Genero x Tipo de Almuerzo",
       subtitle = "Medias observadas +/- IC 95% de punt_mate",
       x = "Tipo de almuerzo", y = "Media puntaje en matematicas",
       color = "Genero") +
  theme_minimal()

# INTERACCIÓN: genero x almuerzo
# Coeficiente de la interacción generoMasculino:almuerzoEstandar = -0.2797
# p = 0.6951  →  NO es significativa
# No se rechaza H0: el termino de interaccion no aporta
# Interpretacion: La ventaja de tener almuerzo estandar es practicamente la misma
# para hombres y mujeres. El coeficiente de -0.28 indica que la brecha
# del almuerzo es apenas 0.28 puntos menor en hombres que en mujeres,
# diferencia estadisticamente indistinguible de cero.
# Las lineas de interaccion son aproximadamente paralelas:
#   - En mujeres:  efecto almuerzo estandar ≈ +3.37 puntos
#   - En hombres:  efecto almuerzo estandar ≈ +3.37 - 0.28 = +3.09 puntos
# DECISION: se descarta esta interacción. El modelo aditivo M5 es
# suficiente; no hay modificación del efecto del almuerzo segun género.


#género x preparación (categórica x categórica) ----
m_int_gp <- lm(punt_mate ~ punt_lect + punt_escr + genero * preparacion +
                 grupo_E + educ_padres + almuerzo, data = df)
print(round(coef(summary(m_int_gp))
            [grep("genero|preparacion", rownames(coef(summary(m_int_gp)))), ], 4))

anova_gp <- anova(m5, m_int_gp)
print(anova_gp)

df_med_gp <- df %>%
  group_by(genero, preparacion) %>%
  summarise(media = mean(punt_mate),
            ee    = sd(punt_mate)/sqrt(n()), .groups = "drop")

ggplot(df_med_gp, aes(x = preparacion, y = media,
                       color = genero, group = genero)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = media - 1.96*ee, ymax = media + 1.96*ee),
                width = 0.12, linewidth = 0.8) +
  scale_color_manual(values = c("firebrick","steelblue")) +
  labs(title    = "Interaccion: Genero x Curso de Preparacion",
       subtitle = "Medias observadas +/- IC 95% de punt_mate",
       x = "Curso de preparacion", y = "Media puntaje en matematicas",
       color = "Genero") +
  theme_minimal()

# INTERACCIÓN: genero x preparació
# Coeficiente de la interacción generoMasculino:preparacionCompleto = -0.6421
# p = 0.3668  →  NO es significativa
# No se rechaza H0: el termino de interaccion no aporta
# Interpretación:
# El efecto negativo del curso de preparacion (al controlar por academicas)
# es similar en hombres y mujeres. El coeficiente de -0.64 indica que
# la penalizacion del curso es 0.64 puntos mayor en hombres que en mujeres,
# pero esta diferencia no es estadisticamente distinguible de cero.
# Efectos por grupo:
#   - Mujeres que completaron:  -3.16 puntos vs mujeres sin prep
#   - Hombres que completaron:  -3.16 + (-0.64) = -3.80 puntos vs mujeres sin prep
# Ambos grupos muestran el mismo patron de sesgo de seleccion discutido
# en la seccion de confusores: quienes se inscriben al curso parten con
# puntajes previos mas bajos, independientemente del genero.
# DECISION: se descarta esta interaccion. El efecto de la preparacion
# sobre punt_mate no es modificado por el genero del estudiante.



# punt_lect x genero (continua x categorica) ----
m_int_lg <- lm(punt_mate ~ punt_lect * genero + punt_escr +
                 grupo_E + educ_padres + almuerzo + preparacion, data = df)
print(round(coef(summary(m_int_lg))
            [grep("punt_lect|genero", rownames(coef(summary(m_int_lg)))), ], 4))

anova_lg <- anova(m5, m_int_lg)
print(anova_lg)

ggplot(df, aes(x = punt_lect, y = punt_mate, color = genero)) +
  geom_point(alpha = 0.18, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(values = c("firebrick","steelblue")) +
  labs(title    = "Interaccion: Puntaje en Lectura x Genero",
       subtitle = "Pendientes de regresion por genero (con banda IC 95%)",
       x = "Puntaje en lectura", y = "Puntaje en matematicas",
       color = "Genero") +
  theme_minimal()

# INTERACCIÓN: punt_lect x genero
# Coeficiente de la interaccion punt_lect:generoMasculino = -0.0506
# p = 0.0357  →  SÍ es significativa (p < 0.05)
# Se rechaza H0: la interaccion aporta significativamente al modelo
# Interpretacion:
# La pendiente de punt_lect sobre punt_mate difiere segun genero:
#   - Mujeres:  por cada punto adicional en lectura, matematicas sube 0.295
#   - Hombres:  por cada punto adicional en lectura, matematicas sube
#               0.295 + (-0.051) = 0.245
# Es decir, la lectura "predice más" matematicas en mujeres que en hombres.
# En hombres la brecha de 13+ puntos a su favor viene principalmente del
# intercepto (ventaja de base), no de la pendiente de lectura.
# En mujeres, un mayor puntaje en lectura se traduce en mayor ganancia
# relativa en matematicas.
# Aunque la interaccion es estadisticamente significativa, el efecto
# es pequeno: la diferencia de pendientes es de solo 0.05 puntos por
# cada punto de lectura. En un rango de 0-100, la diferencia maxima
# acumulada entre generos atribuible a esta interaccion seria de
# 0.05 * 100 = 5 puntos.
# DECISION: la interaccion es significativa (p = 0.036) pero de magnitud
# modesta. Se evalua en la seccion de seleccion de variables si su
# inclusion mejora el modelo segun AIC.



# punt_escr x almuerzo (continua x categorica) ----
m_int_ea <- lm(punt_mate ~ punt_lect + punt_escr * almuerzo +
                 genero + grupo_E + educ_padres + preparacion, data = df)
print(round(coef(summary(m_int_ea))
            [grep("punt_escr|almuerzo", rownames(coef(summary(m_int_ea)))), ], 4))

anova_ea <- anova(m5, m_int_ea)
print(anova_ea)

# INTERACCIÓN: punt_escr x almuerzo
# Coeficiente de la interaccion punt_escr:almuerzoEstandar = 0.0056
# p = 0.8138  →  NO es significativa (la mas debil de las cuatro)
# No se rechaza H0: la interaccion no aporta absolutamente nada
# Interpretacion:
# La pendiente de punt_escr sobre punt_mate es practicamente identica
# en ambos grupos de almuerzo:
#   - Almuerzo subsidiado:  por cada punto en escritura, matematicas sube 0.691
#   - Almuerzo estandar:    por cada punto en escritura, matematicas sube
#                           0.691 + 0.006 = 0.697
# Una diferencia de 0.006 puntos por punto de escritura es negligible
# tanto estadistica como practicamente. El nivel socioeconomico (almuerzo)
# no modifica la relacion entre escritura y matematicas.
# RESUMEN DE LAS CUATRO INTERACCIONES EVALUADAS:
#   genero x almuerzo:     F = 0.154, p = 0.695  → No significativa
#   genero x preparacion:  F = 0.815, p = 0.367  → No significativa
#   punt_lect x genero:    F = 4.426, p = 0.036  → Significativa (p < 0.05)
#   punt_escr x almuerzo:  F = 0.056, p = 0.814  → No significativa
# DECISION FINAL: solo punt_lect x genero es significativa y se incluye
# en el espacio de busqueda de la seleccion de variables (seccion siguiente).
# Las otras tres se descartan por no aportar poder explicativo.




#Tabla resumen de interacciones ----
p_vals <- c(
  anova_ga$`Pr(>F)`[2],
  anova_gp$`Pr(>F)`[2],
  anova_lg$`Pr(>F)`[2],
  anova_ea$`Pr(>F)`[2]
)

tabla_int <- data.frame(
  Interaccion = c("genero x almuerzo","genero x preparacion",
                  "punt_lect x genero","punt_escr x almuerzo"),
  F_estadistico = c(anova_ga$F[2], anova_gp$F[2],
                    anova_lg$F[2], anova_ea$F[2]),
  p_valor      = p_vals,
  AIC_modelo   = c(AIC(m_int_ga), AIC(m_int_gp),
                   AIC(m_int_lg), AIC(m_int_ea)),
  Significativa = ifelse(p_vals < 0.05, "Si (p<0.05)", "No")
)
tabla_int[, 2:4] <- round(tabla_int[, 2:4], 4)

cat("\n--- Tabla resumen de interacciones evaluadas ---\n")
print(tabla_int)
cat("AIC de M5 (base):", round(AIC(m5), 1), "\n")

# Interacciones con mejora significativa
int_sig <- tabla_int$Interaccion[tabla_int$p_valor < 0.05 &
                                    tabla_int$AIC_modelo < AIC(m5)]
cat("Interacciones que mejoran AIC y son significativas:",
    ifelse(length(int_sig) == 0, "Ninguna", paste(int_sig, collapse=", ")), "\n")








# SECCIÓN 3: SELECCIÓN DE VARIABLES







# Espacio de búsqueda: M5 extendido con las 4 interacciones evaluadas
m_nulo   <- lm(punt_mate ~ 1, data = df)
m_amplio <- lm(punt_mate ~ punt_lect + punt_escr + genero + grupo_E +
                 educ_padres + almuerzo + preparacion +
                 genero:almuerzo + genero:preparacion +
                 punt_lect:genero + punt_escr:almuerzo,
               data = df)

cat("Espacio de busqueda:\n")
cat("  Modelo nulo   (k=1)    AIC:", round(AIC(m_nulo), 1), "\n")
cat("  Modelo base M5 (k=12)  AIC:", round(AIC(m5), 1), "\n")
cat("  Modelo amplio (k=16)   AIC:", round(AIC(m_amplio), 1), "\n\n")

# Backward
cat("--- Metodo BACKWARD (AIC) ---\n")
m_backward <- stepAIC(m_amplio, direction = "backward", trace = FALSE)
cat("Formula: "); print(formula(m_backward))
cat("AIC =", round(AIC(m_backward), 1),
    "| R²adj =", round(summary(m_backward)$adj.r.squared, 4),
    "| k =", length(coef(m_backward)), "\n")

# Forward
cat("\n--- Metodo FORWARD (AIC) ---\n")
m_forward <- stepAIC(m_nulo,
                     scope  = list(lower = m_nulo, upper = m_amplio),
                     direction = "forward", trace = FALSE)
cat("Formula: "); print(formula(m_forward))
cat("AIC =", round(AIC(m_forward), 1),
    "| R²adj =", round(summary(m_forward)$adj.r.squared, 4),
    "| k =", length(coef(m_forward)), "\n")

# Stepwise bidireccional
cat("\n--- Metodo STEPWISE - ambas direcciones (AIC) ---\n")
m_stepwise <- stepAIC(m5,
                      scope  = list(lower = m_nulo, upper = m_amplio),
                      direction = "both", trace = FALSE)
cat("Formula: "); print(formula(m_stepwise))
cat("AIC =", round(AIC(m_stepwise), 1),
    "| R²adj =", round(summary(m_stepwise)$adj.r.squared, 4),
    "| k =", length(coef(m_stepwise)), "\n")

# 3.4  Tabla comparativa
modelos_sel <- list(
  "M5 base (R1)"  = m5,
  "Backward"      = m_backward,
  "Forward"       = m_forward,
  "Stepwise"      = m_stepwise
)

tabla_sel <- data.frame(
  Metodo = names(modelos_sel),
  R2_adj = sapply(modelos_sel, function(m) round(summary(m)$adj.r.squared, 4)),
  AIC    = sapply(modelos_sel, function(m) round(AIC(m), 1)),
  BIC    = sapply(modelos_sel, function(m) round(BIC(m), 1)),
  RSE    = sapply(modelos_sel, function(m) round(summary(m)$sigma, 3)),
  k_coef = sapply(modelos_sel, function(m) length(coef(m)))
)
cat("\n--- Tabla comparativa metodos de seleccion ---\n")
print(tabla_sel)

# Gráfico comparativo AIC
p_aic_sel <- ggplot(tabla_sel, aes(x = Metodo, y = AIC,
                                    fill = AIC == min(AIC))) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = AIC), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("grey60","steelblue"), guide = "none") +
  labs(title = "AIC por metodo de seleccion",
       subtitle = "Azul: menor AIC (mejor modelo)",
       x = NULL, y = "AIC") +
  theme_minimal()

p_r2_sel <- ggplot(tabla_sel, aes(x = Metodo, y = R2_adj,
                                   fill = R2_adj == max(R2_adj))) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = R2_adj), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("grey60","firebrick"), guide = "none") +
  labs(title = "R² ajustado por metodo",
       x = NULL, y = expression(R^2~ajustado)) +
  theme_minimal()

grid.arrange(p_aic_sel, p_r2_sel, ncol = 2)

# Modelo escogido para el Reporte 2
mejor_idx    <- which.min(tabla_sel$AIC)
mejor_nombre <- tabla_sel$Metodo[mejor_idx]
m_final      <- modelos_sel[[mejor_nombre]]

cat(sprintf("\nModelo escogido: %s\n", mejor_nombre))
cat(sprintf("AIC = %.1f | R²adj = %.4f | RSE = %.3f | k = %d coef.\n",
            AIC(m_final), summary(m_final)$adj.r.squared,
            summary(m_final)$sigma, length(coef(m_final))))
summary(m_final)


# Test F anova del modelo final vs M5
if (!identical(formula(m_final), formula(m5))) {
  cat("\nTest F anidado — modelo final vs M5:\n")
  if (length(coef(m_final)) > length(coef(m5))) {
    print(anova(m5, m_final))
  } else {
    print(anova(m_final, m5))
  }
}

#Resumen completo del modelo final
print(summary(m_final))

#Intervalos de confianza 95% modelo final
print(round(confint(m_final), 4))

#VIF — modelo final
print(round(car::vif(m_final), 4))






# 4: MEDIDAS DE INFLUENCIA




n_obs <- nrow(df)
p_par <- length(coef(m_final))

inf_df <- data.frame(
  obs    = 1:n_obs,
  hat    = hatvalues(m_final),
  cookD  = cooks.distance(m_final),
  dffits = dffits(m_final),
  rstud  = rstudent(m_final)
)
dfb           <- dfbetas(m_final)
inf_df$dfb_max <- apply(abs(dfb), 1, max)

umb_hat    <- 2 * p_par / n_obs
umb_cook   <- 4 / n_obs
umb_dffits <- 2 * sqrt(p_par / n_obs)
umb_dfb    <- 2 / sqrt(n_obs)

cat(sprintf("k = %d parametros | n = %d observaciones\n", p_par, n_obs))
cat(sprintf("Umbral Leverage  h > 2p/n      = %.4f\n",  umb_hat))
cat(sprintf("Umbral Cook      D > 4/n       = %.4f\n",  umb_cook))
cat(sprintf("Umbral DFFITS    > 2*sqrt(p/n) = %.4f\n", umb_dffits))
cat(sprintf("Umbral DFBETAS   > 2/sqrt(n)   = %.4f\n", umb_dfb))

cat("\nConteos por criterio:\n")
cat("  Leverage alto:        ", sum(inf_df$hat    > umb_hat), "\n")
cat("  Cook alto:            ", sum(inf_df$cookD  > umb_cook), "\n")
cat("  DFFITS alto:          ", sum(abs(inf_df$dffits) > umb_dffits), "\n")
cat("  DFBETAS max alto:     ", sum(inf_df$dfb_max > umb_dfb), "\n")

inf_doble <- with(inf_df, cookD > umb_cook & abs(dffits) > umb_dffits)
cat("  Influyentes Cook+DFFITS:", sum(inf_doble), "\n")
if (sum(inf_doble) > 0)
  cat("  Obs numeros:", which(inf_doble), "\n")

# Distancia de Cook
top10_c <- order(inf_df$cookD, decreasing = TRUE)[1:10]

ggplot(inf_df, aes(x = obs, y = cookD)) +
  geom_col(aes(fill = cookD > umb_cook), width = 0.6) +
  geom_hline(yintercept = umb_cook, linetype = "dashed",
             color = "firebrick", linewidth = 0.9) +
  geom_text_repel(data = inf_df[top10_c, ],
                  aes(label = obs), size = 2.8, max.overlaps = 15) +
  scale_fill_manual(values = c("grey55","firebrick"),
                    labels = c("Normal","Influyente"), name = NULL) +
  labs(title    = "Distancia de Cook — Modelo Final",
       subtitle = paste0("Linea roja: umbral 4/n = ", round(umb_cook, 4)),
       x = "Observacion", y = "D de Cook") +
  theme_minimal()

# DFFITS
top10_d <- order(abs(inf_df$dffits), decreasing = TRUE)[1:10]

ggplot(inf_df, aes(x = obs, y = dffits)) +
  geom_col(aes(fill = abs(dffits) > umb_dffits), width = 0.6) +
  geom_hline(yintercept =  umb_dffits, linetype = "dashed",
             color = "firebrick", linewidth = 0.9) +
  geom_hline(yintercept = -umb_dffits, linetype = "dashed",
             color = "firebrick", linewidth = 0.9) +
  geom_text_repel(data = inf_df[top10_d, ],
                  aes(label = obs), size = 2.8, max.overlaps = 15) +
  scale_fill_manual(values = c("grey55","steelblue"),
                    labels = c("Normal","Influyente"), name = NULL) +
  labs(title    = "DFFITS — Modelo Final",
       subtitle = paste0("Umbral: +/-2*sqrt(p/n) = +/-", round(umb_dffits, 3)),
       x = "Observacion", y = "DFFITS") +
  theme_minimal()

# 4.3  Leverage vs Residuos studentizados
ggplot(inf_df, aes(x = hat, y = rstud)) +
  geom_point(alpha = 0.4, size = 1.5, color = "steelblue") +
  geom_vline(xintercept = umb_hat, linetype = "dashed",
             color = "firebrick", linewidth = 0.9) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted",
             color = "orange", linewidth = 0.8) +
  geom_text_repel(
    data = filter(inf_df, hat > umb_hat | abs(rstud) > 3),
    aes(label = obs), size = 2.5, max.overlaps = 20) +
  labs(title    = "Leverage vs Residuo Studentizado Externo",
       subtitle = "Rojo: umbral leverage  |  Naranja: +/- 3 sigma",
       x = "Leverage (h_ii)", y = "Residuo Studentizado Externo") +
  theme_minimal()

# DFBETAS, coeficientes clave
dfb_df    <- as.data.frame(dfb)
vars_show <- colnames(dfb)[2:min(5, ncol(dfb))]
labels_dfb <- colnames(dfb)[2:min(5, ncol(dfb))]

for (i in seq_along(vars_show)) {
  vv  <- vars_show[i]
  col <- dfb_df[[vv]]
  tmp <- data.frame(obs = 1:n_obs, val = col)
  top_d <- order(abs(col), decreasing = TRUE)[1:10]

  p_d <- ggplot(tmp, aes(x = obs, y = val)) +
    geom_col(aes(fill = abs(val) > umb_dfb), width = 0.6) +
    geom_hline(yintercept =  umb_dfb, linetype = "dashed",
               color = "firebrick", linewidth = 0.8) +
    geom_hline(yintercept = -umb_dfb, linetype = "dashed",
               color = "firebrick", linewidth = 0.8) +
    geom_text_repel(data = tmp[top_d, ],
                    aes(label = obs), size = 2.8, max.overlaps = 15) +
    scale_fill_manual(values = c("grey55","tomato"),
                      labels = c("Normal","Influyente"), name = NULL) +
    labs(title    = paste("DFBETAS —", labels_dfb[i]),
         subtitle = paste0("Umbral: +/- 2/sqrt(n) = +/-", round(umb_dfb, 3)),
         x = "Observacion", y = "DFBETAS") +
    theme_minimal()
  print(p_d)
}

# Top 15 observaciones influyentes
cat("\nTop 15 observaciones influyentes (por D de Cook)\n")
top15 <- inf_df %>%
  arrange(desc(cookD)) %>%
  slice_head(n = 15) %>%
  mutate(across(where(is.numeric), ~round(., 4)))
print(top15)

# Análisis de sensibilidad
obs_rem <- with(inf_df, cookD > umb_cook & abs(dffits) > umb_dffits)
cat(sprintf("\nObservaciones que superan Cook Y DFFITS: %d (%.1f%%)\n",
            sum(obs_rem), 100 * mean(obs_rem)))

if (sum(obs_rem) > 0 & sum(obs_rem) < 60) {
  df_sin  <- df[!obs_rem, ]
  m_sin   <- update(m_final, data = df_sin)

  comp <- data.frame(
    Coef       = names(coef(m_final)),
    Original   = round(coef(m_final), 4),
    Sin_influy = round(coef(m_sin),   4),
    Delta_pct  = round((coef(m_sin) - coef(m_final)) /
                         (abs(coef(m_final)) + 1e-10) * 100, 1)
  )
  cat("\nCambio en coeficientes al excluir observaciones influyentes:\n")
  print(comp)
  cat(sprintf("R²adj original: %.4f | sin influyentes: %.4f\n",
              summary(m_final)$adj.r.squared, summary(m_sin)$adj.r.squared))
}



# SECCIÓN 5: VALIDACIÓN FUERA DE MUESTRA Y PRONÓSTICOS




# 5.1  Partición entrenamiento / prueba 80/20
idx_train <- createDataPartition(df$punt_mate, p = 0.80, list = FALSE)
df_train  <- df[ idx_train, ]
df_test   <- df[-idx_train, ]
cat(sprintf("Entrenamiento: %d obs (80%%) | Prueba: %d obs (20%%)\n",
            nrow(df_train), nrow(df_test)))

m_train   <- update(m_final, data = df_train)
pred_trn  <- fitted(m_train)
pred_tst  <- predict(m_train, newdata = df_test)

rmse_fn <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae_fn  <- function(y, yhat) mean(abs(y - yhat))
r2_fn   <- function(y, yhat)
  1 - sum((y - yhat)^2) / sum((y - mean(y))^2)

metricas <- data.frame(
  Conjunto = c("Entrenamiento","Prueba"),
  RMSE     = round(c(rmse_fn(df_train$punt_mate, pred_trn),
                     rmse_fn(df_test$punt_mate,  pred_tst)), 4),
  MAE      = round(c(mae_fn(df_train$punt_mate,  pred_trn),
                     mae_fn(df_test$punt_mate,   pred_tst)), 4),
  R2       = round(c(r2_fn(df_train$punt_mate,   pred_trn),
                     r2_fn(df_test$punt_mate,    pred_tst)), 4)
)
cat("\nMetricas de desempeno predictivo:\n")
print(metricas)
cat(sprintf("Diferencia RMSE (prueba - entrenamiento): %.4f\n",
            metricas$RMSE[2] - metricas$RMSE[1]))

# Gráfico real vs predicho
df_pred <- data.frame(
  real = df_test$punt_mate,
  pred = pred_tst,
  err  = df_test$punt_mate - pred_tst
)

ggplot(df_pred, aes(x = pred, y = real)) +
  geom_point(alpha = 0.45, color = "steelblue", size = 1.8) +
  geom_abline(slope = 1, intercept = 0, color = "firebrick",
              linetype = "dashed", linewidth = 1) +
  annotate("text", x = 18, y = 96,
           label = sprintf("RMSE = %.2f\nMAE  = %.2f\nR2   = %.3f",
                           metricas$RMSE[2], metricas$MAE[2], metricas$R2[2]),
           hjust = 0, size = 3.8, color = "grey20") +
  labs(title    = "Valores reales vs. predichos — Conjunto de prueba (20%)",
       subtitle = "Linea roja discontinua: prediccion perfecta",
       x = "Puntaje predicho", y = "Puntaje real en matematicas") +
  coord_equal(xlim = c(0,105), ylim = c(0,105)) +
  theme_minimal()

# Distribución de errores
ggplot(df_pred, aes(x = err)) +
  geom_histogram(aes(y = ..density..), bins = 25,
                 fill = "steelblue", color = "white", alpha = 0.8) +
  geom_density(color = "firebrick", linewidth = 1) +
  stat_function(fun  = dnorm,
                args = list(mean = mean(df_pred$err), sd = sd(df_pred$err)),
                color = "darkgreen", linetype = "dashed", linewidth = 1) +
  labs(title    = "Errores de prediccion — Conjunto de prueba",
       subtitle = "Rojo: densidad empirica  |  Verde: normal teorica",
       x = "Error (real - predicho)", y = "Densidad") +
  theme_minimal()

# Validación cruzada 10-Fold 
cat("\nValidacion cruzada 10-Fold (datos completos)\n")
ctrl_cv <- trainControl(method = "cv", number = 10)
m_cv    <- train(formula(m_final), data = df, method = "lm",
                 trControl = ctrl_cv)
cv_res  <- m_cv$results

cat(sprintf("CV-10 RMSE medio: %.4f (+/- %.4f)\n",
            cv_res$RMSE, cv_res$RMSESD))
cat(sprintf("CV-10 R2 medio:   %.4f (+/- %.4f)\n",
            cv_res$Rsquared, cv_res$RsquaredSD))
cat(sprintf("CV-10 MAE medio:  %.4f (+/- %.4f)\n",
            cv_res$MAE, cv_res$MAESD))

fold_res      <- m_cv$resample
fold_res$Fold <- seq_len(nrow(fold_res))

ggplot(fold_res, aes(x = Fold, y = RMSE)) +
  geom_line(color = "steelblue", linewidth = 1.1) +
  geom_point(size = 3.2, color = "steelblue") +
  geom_hline(yintercept = cv_res$RMSE, linetype = "dashed",
             color = "firebrick", linewidth = 0.9) +
  annotate("text", x = 1.4, y = cv_res$RMSE + 0.12,
           label = paste0("Media = ", round(cv_res$RMSE, 3)),
           color = "firebrick", size = 3.5) +
  scale_x_continuous(breaks = 1:10) +
  labs(title    = "RMSE por fold — Validacion cruzada 10-Fold",
       subtitle = "Linea roja: RMSE promedio",
       x = "Fold", y = "RMSE") +
  theme_minimal()

# Pronósticos para 5 nuevos perfiles
cat("\nPronosticos para 5 perfiles hipoteticos\n")

nuevos <- data.frame(
  punt_lect   = c(70, 55, 85, 60, 75),
  punt_escr   = c(65, 50, 80, 58, 70),
  genero      = factor(c("Masculino","Femenino","Masculino","Femenino","Masculino"),
                       levels = levels(df$genero)),
  grupo_E     = factor(c("GrupoE","OtroGrupo","GrupoE","OtroGrupo","OtroGrupo"),
                       levels = levels(df$grupo_E)),
  educ_padres = factor(c("Universitario","Bach_inc","Posgrado","Algo_univ","Tecnico"),
                       levels = levels(df$educ_padres)),
  almuerzo    = factor(c("Estandar","Subsidiado","Estandar","Subsidiado","Estandar"),
                       levels = levels(df$almuerzo)),
  preparacion = factor(c("Completo","Sin_prep","Completo","Sin_prep","Completo"),
                       levels = levels(df$preparacion))
)

pred_ic <- predict(m_final, newdata = nuevos,
                   interval = "confidence", level = 0.95)
pred_ip <- predict(m_final, newdata = nuevos,
                   interval = "prediction", level = 0.95)

pron <- data.frame(
  Perfil   = paste0("P", 1:5),
  Lect     = nuevos$punt_lect,
  Escr     = nuevos$punt_escr,
  Genero   = nuevos$genero,
  Almuerzo = nuevos$almuerzo,
  Pred     = round(pred_ic[,"fit"], 2),
  IC_inf   = round(pred_ic[,"lwr"], 2),
  IC_sup   = round(pred_ic[,"upr"], 2),
  IP_inf   = round(pred_ip[,"lwr"], 2),
  IP_sup   = round(pred_ip[,"upr"], 2)
)
cat("\nTabla de pronosticos con intervalos al 95%:\n")
print(pron)

ggplot(pron, aes(x = Perfil, y = Pred)) +
  geom_point(size = 4, color = "steelblue") +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup),
                width = 0.15, color = "firebrick", linewidth = 1.3,
                position = position_nudge(x = -0.1)) +
  geom_errorbar(aes(ymin = IP_inf, ymax = IP_sup),
                width = 0.15, color = "darkgreen", linewidth = 0.9,
                linetype = "dashed",
                position = position_nudge(x = 0.1)) +
  labs(title    = "Pronosticos por perfil de estudiante",
       subtitle = "Rojo: IC 95% media  |  Verde discontinuo: IP 95% individual",
       x = "Perfil", y = "Puntaje predicho en matematicas") +
  ylim(35, 105) +
  theme_minimal()








# VALIDACIÓN DE SUPUESTOS — MODELO FINAL








res_f  <- residuals(m_final)
resstd <- rstandard(m_final)
ajust  <- fitted(m_final)
rdf    <- data.frame(res = res_f, resstd = resstd, ajust = ajust)

# Normalidad
# Shapiro-Wilk sobre residuos")
sw <- shapiro.test(res_f); print(sw)

ggplot(rdf, aes(sample = res)) +
  stat_qq(alpha = 0.4, size = 1.2) +
  stat_qq_line(color = "firebrick", linewidth = 1) +
  labs(title    = "QQ-Plot de residuos — Modelo Final",
       subtitle = sprintf("Shapiro-Wilk: W = %.3f, p = %.3f",
                          sw$statistic, sw$p.value),
       x = "Cuantiles teoricos (Normal estandar)",
       y = "Cuantiles muestrales") +
  theme_minimal()

ggplot(rdf, aes(x = res)) +
  geom_histogram(aes(y = ..density..), bins = 30,
                 fill = "steelblue", color = "white", alpha = 0.75) +
  geom_density(color = "firebrick", linewidth = 1) +
  stat_function(fun  = dnorm,
                args = list(mean = mean(res_f), sd = sd(res_f)),
                color = "darkgreen", linetype = "dashed", linewidth = 1) +
  labs(title    = "Distribucion de residuos — Modelo Final",
       subtitle = "Rojo: densidad empirica  |  Verde: normal teorica",
       x = "Residuos", y = "Densidad") +
  theme_minimal()

# Homocedasticidad
# Prueba de Breusch-Pagan")
bp <- bptest(m_final); print(bp)

ggplot(rdf, aes(x = ajust, y = resstd)) +
  geom_point(alpha = 0.3, size = 1.2) +
  geom_hline(yintercept = 0,       color = "firebrick", linetype = "dashed") +
  geom_hline(yintercept = c(-2,2), color = "orange",    linetype = "dotted") +
  geom_smooth(method = "loess", color = "steelblue", se = FALSE) +
  labs(title    = "Residuos estandarizados vs Valores ajustados — Modelo Final",
       subtitle = "Diagnostico de homocedasticidad",
       x = "Valores ajustados", y = "Residuos estandarizados") +
  theme_minimal()

ggplot(rdf, aes(x = ajust, y = sqrt(abs(resstd)))) +
  geom_point(alpha = 0.3, size = 1.2) +
  geom_smooth(method = "loess", color = "firebrick", se = FALSE) +
  labs(title = "Scale-Location — Modelo Final",
       x = "Valores ajustados",
       y = "sqrt(|Residuos estandarizados|)") +
  theme_minimal()

# Multicolinealidad
#VIF Modelo Final")
print(round(car::vif(m_final), 4))

# Independencia
#Independencia: diseño de corte transversal, se asume por diseño")

# Tabla resumen final
cat("\nTabla comparativa general Reporte 2 ")
todos <- list(
  "M5 base (R1)" = m5,
  "Backward"     = m_backward,
  "Forward"      = m_forward,
  "Stepwise"     = m_stepwise,
  "Final adopt." = m_final
)

tabla_gral <- data.frame(
  Modelo = names(todos),
  R2_adj = sapply(todos, function(m) round(summary(m)$adj.r.squared, 4)),
  AIC    = sapply(todos, function(m) round(AIC(m), 1)),
  BIC    = sapply(todos, function(m) round(BIC(m), 1)),
  RSE    = sapply(todos, function(m) round(summary(m)$sigma, 3)),
  k_coef = sapply(todos, function(m) length(coef(m)))
)
print(tabla_gral)