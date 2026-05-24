# ---------------- Universidad Javeriana
# ---------------- Facultad de Ciencias 
# -------------- Deparamento de Matemáticas
# Isaias Acosta y Juan Diego Carreño

# 0. SELECCION DE TECNICA
set.seed(123)
tec <- sample(c("ACP", "ACC"), 12, replace = T)
numero <- 4       # Grupo 4
tec[numero]       # Resultado: "ACC"

# 1. LECTURA Y PREPROCESAMIENTO
library(readxl)
library(tibble)

Atletas <- read_excel("/Users/juandgocv/Downloads/Atletas.xlsx")
Datos   <- as.data.frame(Atletas)
head(Datos)
str(Datos)

# Grupos de variables:
#   X = Pruebas de velocidad (tiempos, segundos)
#   Y = Pruebas de habilidad (distancias, metros)
X <- Datos[, c("Vallas100m", "X200m", "X800m")]
Y <- Datos[, c("Salto_alto", "L.Peso", "Salto_largo", "Javalina")]

p <- ncol(X)   # 3
q <- ncol(Y)   # 4
n <- nrow(X)   # 25

# 2. ESTADISTICAS DESCRIPTIVAS
cat("\n=== Estadisticas descriptivas - Velocidad (X) ===\n")
desc_X <- rbind(Media  = sapply(X, mean),
                SD     = sapply(X, sd),
                Minimo = sapply(X, min),
                Maximo = sapply(X, max))
print(round(desc_X, 4))

cat("\n=== Estadisticas descriptivas - Habilidad (Y) ===\n")
desc_Y <- rbind(Media  = sapply(Y, mean),
                SD     = sapply(Y, sd),
                Minimo = sapply(Y, min),
                Maximo = sapply(Y, max))
print(round(desc_Y, 4))

# 3. ANALISIS PRELIMINAR

# 3.1 Normalidad Multivariada - Test de Mardia 
require(MVN)
datos_num <- data.frame(X, Y)

# Test de Mardia 
NormTest <- mvn(data = datos_num, mvnTest = "mardia", univariateTest = "SW")
cat("\n=== Test de Mardia (normalidad multivariada) ===\n")
print(NormTest$multivariateNormality)
cat("\n=== Shapiro-Wilk por variable ===\n")
print(NormTest$univariateNormality)

# FIGURA 1: Q-Q multivariado 
# Distancias de Mahalanobis al cuadrado
mah2    <- mahalanobis(datos_num, colMeans(datos_num), cov(datos_num))
chiQ    <- qchisq(ppoints(n), df = p + q)   # cuantiles chi^2 con p+q=7 gl

png("fig1_qq.png", width = 600, height = 520, res = 100)
par(mar = c(5.5, 4.5, 3, 2))
plot(sort(chiQ), sort(mah2),
     xlab = expression("Cuantiles " * chi[7]^2),
     ylab = "Distancias de Mahalanobis al cuadrado",
     pch  = 19, col = "steelblue", cex = 1.1,
     main = "")
abline(0, 1, col = "red2", lwd = 2)
mtext("Figura 1. Grafico Q-Q multivariado: distancias de Mahalanobis vs. cuantiles chi-cuadrado",
      side = 1, line = 4.5, cex = 0.82)
dev.off()
cat(">> fig1_qq.png guardada\n")

# 3.2 Varianzas (decision R vs S)
cat("\n=== Varianzas por variable ===\n")
print(round(diag(cov(datos_num)), 4))
# Conclusion: varianzas muy heterogeneas -> usar matriz de correlaciones

# 3.3 Matrices de correlacion entre grupos 
require(CCA)
corr <- matcor(X, Y)

cat("\n=== Correlaciones internas X ===\n")
print(round(corr$Xcor, 4))
cat("\n=== Correlaciones internas Y ===\n")
print(round(corr$Ycor, 4))
cat("\n=== Correlaciones cruzadas X-Y ===\n")
print(round(corr$XYcor[1:p, (p+1):(p+q)], 4))

# FIGURA 2: Mapa de calor de correlaciones 
require(corrplot)

R_full <- corr$XYcor
rownames(R_full) <- c("Vallas 100m", "200m", "800m",
                      "Salto alto", "Lanz. peso", "Salto largo", "Jabalina")
colnames(R_full) <- c("Vallas 100m", "200m", "800m",
                      "Salto alto", "Lanz. peso", "Salto largo", "Jabalina")

png("fig2_corrplot.png", width = 700, height = 620, res = 100)
par(mar = c(0, 0, 4, 0))
corrplot(R_full,
         method      = "color",
         type        = "upper",
         addCoef.col = "black",
         number.cex  = 0.78,
         tl.cex      = 0.82,
         tl.col      = "black",
         mar         = c(0, 0, 3, 0),
         title       = "Figura 2. Correlaciones entre los dos grupos de variables")
dev.off()
cat(">> fig2_corrplot.png guardada\n")

# 3.4 Prueba de independencia H0: Sigma_XY = 0 
require(expm)

S11 <- corr$Xcor
S22 <- corr$Ycor
S12 <- corr$XYcor[1:p, (p+1):(p+q)]

inv_raiz_11 <- solve(sqrtm(S11))
inv_raiz_22 <- solve(sqrtm(S22))

A <- inv_raiz_11 %*% S12 %*% solve(S22) %*% t(S12) %*% inv_raiz_11
B <- inv_raiz_22 %*% t(S12) %*% solve(S11) %*% S12  %*% inv_raiz_22

autoA   <- eigen(A)
lambdak <- autoA$values

rho_muestral <- sqrt(lambdak)
cat("\n=== Correlaciones canonicas muestrales (via eigen) ===\n")
print(round(rho_muestral, 4))

Lambda_stat <- (n / 2) * sum(log(1 - lambdak))
rho_factor  <- 1 - ((p + q + 3) / (2 * n))
varphi      <- (-2) * rho_factor * Lambda_stat
vc          <- qchisq(0.01, p * q, lower.tail = FALSE)

cat("\n=== Prueba H0: Sigma_XY = 0 ===\n")
cat("Estadistico phi  :", round(varphi, 4), "\n")
cat("Valor critico (chi2, gl =", p*q, ", alpha=0.01):", round(vc, 4), "\n")
cat("Decision:", ifelse(varphi > vc,
                        "Se rechaza H0 -> ACC pertinente",
                        "No se rechaza H0"), "\n")

# 3.5 Correlaciones de Spearman (respaldo no parametrico)
p_sp <- matrix(NA, nrow = p, ncol = q,
               dimnames = list(colnames(X), colnames(Y)))
for (i in 1:p) {
  for (j in 1:q) {
    p_sp[i, j] <- cor.test(X[, i], Y[, j], method = "spearman")$p.value
  }
}
cat("\n=== p-valores Spearman entre grupos ===\n")
print(round(p_sp, 4))

# 4. ANALISIS DE CORRELACION CANONICA

# Estandarizar
zX <- scale(X)
zY <- scale(Y)

acc <- cc(zX, zY)

# 4.1 Correlaciones canonicas
cat("\n=== Correlaciones canonicas ===\n")
print(round(acc$cor, 4))

# 4.2 Significancia (Wilks Lambda - Rao F) 
library(CCP)
cat("\n=== Prueba de significancia (p.asym - Wilks) ===\n")
sig <- p.asym(acc$cor, n, p, q)
print(sig)

# 4.3 Coeficientes de las variables canonicas 
cat("\n=== Coeficientes a (Variables U - Velocidad) ===\n")
print(round(acc$xcoef, 4))

cat("\n=== Coeficientes b (Variables V - Habilidad) ===\n")
print(round(acc$ycoef, 4))

# 4.4 Cargas canonicas y cruzadas
cargas <- comput(zX, zY, acc)

cat("\n=== Cargas canonicas: X con U ===\n")
print(round(cargas$corr.X.xscores, 4))

cat("\n=== Cargas canonicas: Y con V ===\n")
print(round(cargas$corr.Y.yscores, 4))

cat("\n=== Cargas cruzadas: X con V ===\n")
print(round(cargas$corr.X.yscores, 4))

cat("\n=== Cargas cruzadas: Y con U ===\n")
print(round(cargas$corr.Y.xscores, 4))

# 4.5 Potencial explicativo
rhoUX <- cargas$corr.X.xscores
rhoVY <- cargas$corr.Y.yscores

PE_U <- apply(rhoUX^2, 2, mean)
PE_V <- apply(rhoVY^2, 2, mean)

cat("\n=== Potencial explicativo U (varianza de X por par) ===\n")
print(round(PE_U, 4))
cat("\n=== Potencial explicativo V (varianza de Y por par) ===\n")
print(round(PE_V, 4))

# 5. GRAFICAS DEL ACC

etiq_X <- c("Vallas 100m", "200m", "800m")
etiq_Y <- c("Salto alto", "Lanz. peso", "Salto largo", "Jabalina")

# FIGURA 3: Cargas canonicas del primer par
png("fig3_cargas.png", width = 820, height = 440, res = 100)
par(mfrow = c(1, 2), mar = c(8, 4.5, 3.5, 1), oma = c(3.5, 0, 0, 0))

barplot(cargas$corr.X.xscores[, 1],
        names.arg = etiq_X,
        col       = "steelblue",
        main      = expression(U[1] * " vs. Variables de velocidad"),
        ylab      = "Correlacion",
        las       = 2,
        ylim      = c(-1.1, 1.1),
        cex.names = 0.92,
        cex.main  = 1.0)
abline(h = 0, lwd = 1)

barplot(cargas$corr.Y.yscores[, 1],
        names.arg = etiq_Y,
        col       = "tomato3",
        main      = expression(V[1] * " vs. Variables de habilidad"),
        ylab      = "Correlacion",
        las       = 2,
        ylim      = c(-1.1, 1.1),
        cex.names = 0.92,
        cex.main  = 1.0)
abline(h = 0, lwd = 1)

mtext(paste0("Figura 3. Cargas canonicas del primer par canonico",
             "  (rho1 = ", round(acc$cor[1], 4), ")"),
      side = 1, line = 1.5, outer = TRUE, cex = 0.88)

par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
dev.off()
cat(">> fig3_cargas.png guardada\n")

# FIGURA 4: Dispersion del primer par canonico
scores_U <- as.matrix(zX) %*% acc$xcoef
scores_V <- as.matrix(zY) %*% acc$ycoef

continente <- as.factor(Datos$Continente)
paleta     <- c("firebrick", "steelblue", "darkgreen", "goldenrod3", "mediumpurple3")
colores    <- paleta[as.integer(continente)]

png("fig4_scatter.png", width = 680, height = 580, res = 100)
par(mar = c(6, 4.5, 2, 2))
plot(scores_U[, 1], scores_V[, 1],
     xlab = expression("Primera variable canonica  " * U[1] * "  (Velocidad)"),
     ylab = expression("Primera variable canonica  " * V[1] * "  (Habilidad)"),
     pch  = 19,
     col  = colores,
     cex  = 1.15)
abline(h = 0, v = 0, col = "grey65", lty = 2)
legend("topright",
       legend = levels(continente),
       col    = paleta[1:nlevels(continente)],
       pch    = 19,
       cex    = 0.84,
       title  = "Continente",
       bty    = "n")
mtext(paste0("Figura 4. Diagrama de dispersion del primer par canonico",
             "  (rho1 = ", round(acc$cor[1], 4), ")"),
      side = 1, line = 4.8, cex = 0.88)
dev.off()
cat(">> fig4_scatter.png guardada\n")

cat("\n>> Todas las figuras han sido guardadas en el directorio de trabajo.\n")
