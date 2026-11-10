# CASO DE ESTUDIO 1 - REGRESION
# Consumo electrico de una siderurgica (UCI Steel Industry Energy)

# Carga de paquetes y librerias
# Instalación de paquetes.
install.packages("caret")
install.packages("glmnet")
install.packages("mgcv")
install.packages("ranger")
install.packages("rpart")
install.packages("gbm")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("tibble")

# Se instalan las librerias
library(caret)
library(glmnet)
library(mgcv)
library(ranger)
library(rpart)
library(gbm)
library(ggplot2)
library(tidyr)
library(tibble)

# Se fija la seed
set.seed(2026)   
# Crea carpeta "resultados"
# Aquí se guardaran las tablas.
dir.create("resultados", showWarnings = FALSE) 


# 1.Cargar Base de datos
#------------------------------------------------------------------------------
# Define la ruta del archivo (MODIFICAR RUTA PARA CHEQUEAR)
ruta <- "datos/Steel_industry_data.csv"

# Lee el archivo CSV y lo guarda en el data.frame "raw"
raw <- read.csv(ruta)

# Renombra las columnas del 1 al 11 con nombres más manejables
names(raw) <- c("fecha", "usage", "react_t", "lead_react_t", "co2",
                "pf_t", "lead_pf_t", "nsm", "week_status",
                "day_of_week", "load_type")

# Muestra dimensiones.
dim(raw)        
# Cuenta cuántos valores nulos por columna.
colSums(is.na(raw)) 
# Muestra mínimo, máximo, media, mediana del consumo
summary(raw$usage)  

# 2.Verificación de fugas de información.
#------------------------------------------------------------------------------
# Calcula la correlación entre emisiones de CO2 y consumo
cor(raw$co2, raw$usage)

# Calcula el consumo promedio según el tipo de carga
tapply(raw$usage, raw$load_type, mean)

#Gráfica de correlaciones
vars_fuga <- c("usage", "co2", "react_t", "lead_react_t", "pf_t", "lead_pf_t")
matriz_cor <- cor(raw[, vars_fuga], use = "complete.obs")

# Convertimos a formato largo con pivot_longer (como en DataCamp)
cor_long <- as.data.frame(matriz_cor) %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlacion")

# Hacemos el gráfico de calor
p_cor <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlacion)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlacion, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlación de variables en el mismo instante (Fuga)",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Guardamos el gráfico
ggsave("resultados/matriz_fuga.png", p_cor, width = 8, height = 6)


# 3.Se establecen variables.
#------------------------------------------------------------------------------
datos <- raw # Copia los datos crudos a un nuevo data.frame llamado "datos"

# Convierte el texto de fecha a formato de fecha-hora de R.
datos$fecha <- as.POSIXct(raw$fecha, format = "%d/%m/%Y %H:%M", tz = "UTC")

# Si falla se prueba en este formato.
if (any(is.na(datos$fecha))) {
  datos$fecha <- as.POSIXct(raw$fecha, tz = "UTC")
}

# Corrige un error del dataset: la medianoche viene con la fecha del día anterior.
datos$fecha[datos$nsm == 0] <- datos$fecha[datos$nsm == 0] + 86400

# Extrae el "día" (restando 15 min para que la medianoche sea del día que termina)
datos$dia <- as.Date(datos$fecha - 15 * 60)

# Pasa los segundos desde medianoche (nsm) a horas (0 a 24)
datos$hora <- datos$nsm / 3600

# Crea variables cíclicas (seno y coseno) para que las 23:45 y 00:00 sean cercanas
datos$hsin <- sin(2 * pi * datos$hora / 24)
datos$hcos <- cos(2 * pi * datos$hora / 24)

# Crea una bandera (1 o 0) si es fin de semana
datos$weekend <- as.integer(datos$week_status == "Weekend")

# Ordena los datos cronológicamente
datos <- datos[order(datos$fecha), ]

# Calcula la diferencia de tiempo entre filas para verificar que todo sea cada 15 min
difs <- as.numeric(diff(datos$fecha), units = "mins")
table(difs) # Imprime una tabla de frecuencias (debería ser todo 15)

# Define una función para crear rezagos (k = cuántas filas atrás)
rezago <- function(x, k) c(rep(NA, k), head(x, -k))

# Crea los rezagos de consumo (15 min, 1 hora, 24 horas atrás)
datos$y_l1 <- rezago(datos$usage, 1)
datos$y_l4 <- rezago(datos$usage, 4)
datos$y_l96 <- rezago(datos$usage, 96)

# Promedio de las últimas 4 horas (16 intervalos de 15 min)
ma16 <- as.numeric(stats::filter(datos$usage, rep(1 / 16, 16), sides = 1))
datos$y_ma16 <- c(NA, head(ma16, -1)) # Desplaza 1 para no incluir el presente

# Crea rezagos de las variables eléctricas (reactiva, factor de potencia)
datos$react_l1 <- rezago(datos$react_t, 1)
datos$lead_react_l1 <- rezago(datos$lead_react_t, 1)
datos$pf_l1 <- rezago(datos$pf_t, 1)
datos$lead_pf_l1 <- rezago(datos$lead_pf_t, 1)

# Lista de variables rezagadas
vars_lag <- c("y_l1", "y_l4", "y_l96", "y_ma16",
              "react_l1", "lead_react_l1", "pf_l1", "lead_pf_l1")

# Elimina las primeras filas del dataset porque sus rezagos son NA
datos <- datos[complete.cases(datos[, vars_lag]), ]
nrow(datos) # Muestra cuántas filas quedaron


# 4.División para el train, validación y test.
#------------------------------------------------------------------------------
dias <- sort(unique(datos$dia)) # Lista de todos los días únicos
n <- length(dias)               # Cuántos días hay en total

# Calcula el día de corte para el 70% y el 85% (70+15)
corte1 <- floor(0.70 * n)
corte2 <- floor(0.85 * n)

# Asigna los días al conjunto correspondiente
train <- datos[datos$dia %in% dias[1:corte1], ]
val <- datos[datos$dia %in% dias[(corte1 + 1):corte2], ]
test <- datos[datos$dia %in% dias[(corte2 + 1):n], ]

# Limpia los nombres de las filas para evitar problemas
rownames(train) <- NULL
rownames(val) <- NULL
rownames(test) <- NULL

# Imprime un resumen de la división
cat("Dias: train =", length(unique(train$dia)),
    "val =", length(unique(val$dia)),
    "test =", length(unique(test$dia)), "\n")
cat("Filas: train =", nrow(train), "val =", nrow(val),
    "test =", nrow(test), "\n")


# 5.Análisis con Train.
#------------------------------------------------------------------------------
# Se definen las variables numéricas que queremos correlacionar
vars_explorar <- c("usage", "hsin", "hcos", "weekend",
                   "y_l1", "y_l4", "y_l96", "y_ma16",
                   "react_l1", "lead_react_l1", "pf_l1", "lead_pf_l1")

# Se calcula la matriz de correlación.
round(cor(train[, vars_explorar]), 2)

# Correlaciones específicas del consumo con sus rezagos
cor(train$usage, train$y_l1)
cor(train$usage, train$y_l4)
cor(train$usage, train$y_l96)
cor(train$lead_react_l1, train$lead_pf_l1)

# Gráfico de histograma.
p_hist <- ggplot(train, aes(x = usage)) +
  geom_histogram(bins = 45, fill = "steelblue", color = "white") +
  labs(title = "Distribucion del consumo (train)",
       x = "Consumo (kWh)", y = "Intervalos")
ggsave("resultados/hist_consumo.png", p_hist, width = 7, height = 4.5)


# 6.Para Validación cruzada.
#------------------------------------------------------------------------------
set.seed(2026)

# Crea 5 folds agrupados por el día. 
myFolds <- groupKFold(train$dia, k = 5)

# Configura caret para que use estos folds específicos
myControl <- trainControl(
  method = "cv",            # Método validación cruzada
  number = 5,               # 5 pliegues
  index = myFolds,          # Le pasamos los folds definidos arriba
  savePredictions = "final" # Guarda las predicciones out-of-fold (para Stacking)
)


# 7. Definición de fórmulas.
#------------------------------------------------------------------------------
# Fórmula A (lineal y GAM)
fmla_A <- usage ~ hsin + hcos + weekend
fmla_gam_A <- usage ~ s(hsin) + s(hcos) + weekend # s() es para curvas suaves

# Fórmula B (lineal y GAM)
fmla_B <- usage ~ hsin + hcos + weekend +
  y_l1 + y_l4 + y_l96 + y_ma16 +
  react_l1 + lead_react_l1 + pf_l1 + lead_pf_l1
fmla_gam_B <- usage ~ s(hsin) + s(hcos) + weekend +
  s(y_l1) + s(y_l4) + s(y_l96) + s(y_ma16) +
  s(react_l1) + s(lead_react_l1) + s(pf_l1) + s(lead_pf_l1)


# 8.Funciones Auxiliares.
#------------------------------------------------------------------------------
# Calcula RMSE, MAE y R2 en base a los valores reales (obs) y predicciones (pred)
metricas <- function(obs, pred) {
  c(RMSE = sqrt(mean((obs - pred)^2)),
    MAE = mean(abs(obs - pred)),
    R2 = 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2))
}

# Ordena las predicciones que genera caret internamente
alinear_oof <- function(modelo) {
  modelo$pred$pred[order(modelo$pred$rowIndex)]
}

# Hace una validación cruzada manual para el GAM (usando los mismos folds)
cv_gam <- function(fmla, datos, folds) {
  pred <- rep(NA, nrow(datos))
  for (k in seq_along(folds)) {
    dentro <- folds[[k]]
    fuera <- setdiff(seq_len(nrow(datos)), dentro)
    mod <- gam(fmla, data = datos[dentro, ], method = "REML")
    pred[fuera] <- as.numeric(predict(mod, datos[fuera, ]))
  }
  pred
}

# Toma las predicciones en validación, calcula las métricas 
# y arma una tabla ordenada por RMSE
resumir_val <- function(vp, nombre) {
  fin <- val$weekend == 1
  rmse_base_fin <- sqrt(mean((val$usage[fin] - vp$Baseline_media[fin])^2))
  filas <- lapply(names(vp), function(m) {
    met <- metricas(val$usage, vp[[m]])
    rmse_fin <- sqrt(mean((val$usage[fin] - vp[[m]][fin])^2))
    data.frame(Especificacion = nombre, Modelo = m,
               RMSE = unname(met["RMSE"]), MAE = unname(met["MAE"]),
               R2 = unname(met["R2"]), RMSE_finde = rmse_fin,
               Mejora_finde = 100 * (1 - rmse_fin / rmse_base_fin))
  })
  res <- do.call(rbind, filas)
  res[order(res$RMSE), ]
}


# 9.Entrenamiento y Evaluación.
#------------------------------------------------------------------------------
ajustar_especificacion <- function(fmla, fmla_gam, train, val, nombre) {
  cat("\nEspecificacion:", nombre, "\n")
  
# Regresión Lineal (OLS)
  set.seed(2026)
  m_ols <- train(fmla, data = train, method = "lm", trControl = myControl)
  
# Regresión Regularizada (Ridge, Lasso, Elastic Net)
  set.seed(2026)
  m_glmnet <- train(
    fmla, data = train, method = "glmnet", trControl = myControl,
    preProcess = c("center", "scale"),
    tuneGrid = expand.grid(alpha = c(0, 0.5, 1),
                           lambda = seq(0.001, 1, length = 20))
  )
  
# Árbol de decisión
  set.seed(2026)
  m_tree <- train(fmla, data = train, method = "rpart",
                  trControl = myControl, tuneLength = 10)
  
# Random Forest
  n_vars <- ncol(model.matrix(fmla, train)) - 1
  mtry_vals <- unique(pmin(c(2, 4, 6), n_vars))
  set.seed(2026)
  m_rf <- train(
    fmla, data = train, method = "ranger", trControl = myControl,
    tuneGrid = data.frame(mtry = mtry_vals, splitrule = "variance",
                          min.node.size = 5),
    num.trees = 300,
    importance = "permutation"
  )
  
# Gradient Boosting
  set.seed(2026)
  m_gbm <- train(
    fmla, data = train, method = "gbm", trControl = myControl,
    tuneGrid = expand.grid(n.trees = c(100, 200, 300),
                           interaction.depth = c(1, 3),
                           shrinkage = c(0.05, 0.1),
                           n.minobsinnode = 10),
    verbose = FALSE
  )
  
# GAM (Modelo Aditivo Generalizado)
  m_gam <- gam(fmla_gam, data = train, method = "REML")
  
# Recolecta las predicciones "Out-Of-Fold" (fuera de muestra) de todos los modelos
  oof <- data.frame(obs = train$usage)
  oof$OLS <- alinear_oof(m_ols)
  oof$glmnet <- alinear_oof(m_glmnet)
  oof$Arbol <- alinear_oof(m_tree)
  oof$RandomForest <- alinear_oof(m_rf)
  oof$Boosting <- alinear_oof(m_gbm)
  oof$GAM <- cv_gam(fmla_gam, train, myFolds)
  
# Entrena el Stacking (una regresión lineal que combina las predicciones)
  meta <- lm(obs ~ OLS + GAM + RandomForest + Boosting, data = oof)
  
# Predecir todos los modelos en el set de VALIDACIÓN
  vp <- data.frame(
    OLS = predict(m_ols, val),
    glmnet = predict(m_glmnet, val),
    Arbol = predict(m_tree, val),
    RandomForest = predict(m_rf, val),
    Boosting = predict(m_gbm, val),
    GAM = as.numeric(predict(m_gam, val))
  )
  
# Agrega los baselines (Media y último valor "y_l1")
  vp$Baseline_media <- mean(train$usage)
  vp$Baseline_yl1 <- val$y_l1
  
# Crea el promedio simple (Averaging) y la predicción final del Stacking
  vp$Promedio <- rowMeans(vp[, c("OLS", "RandomForest", "Boosting")])
  vp$Stacking <- predict(meta, vp)
  
# Genera la tabla final de métricas
  res <- resumir_val(vp, nombre)
  print(cbind(res[, 1:2], round(res[, 3:7], 2)), row.names = FALSE)
  
# Retorna una lista con todos los modelos guardados
  list(fmla = fmla, oof = oof, val_pred = vp, res = res,
       m_ols = m_ols, m_glmnet = m_glmnet, m_tree = m_tree,
       m_rf = m_rf, m_gbm = m_gbm, m_gam = m_gam, meta = meta)
}


# 10.Se utilizan fórmulas.
#------------------------------------------------------------------------------
res_A <- ajustar_especificacion(fmla_A, fmla_gam_A, train, val, "A")
res_B <- ajustar_especificacion(fmla_B, fmla_gam_B, train, val, "B")


# 11.Construcción de tabla de evaluación.
#------------------------------------------------------------------------------
# Junta los resultados de A y B
tabla_val <- rbind(res_A$res, res_B$res)
write.csv(tabla_val, "resultados/tabla_validacion.csv", row.names = FALSE)

# Gráfico de barras de RMSE
p_val <- ggplot(tabla_val, aes(x = reorder(Modelo, RMSE), y = RMSE, fill = Especificacion)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "RMSE en validacion", x = NULL, y = "RMSE (kWh)")
ggsave("resultados/rmse_validacion.png", p_val, width = 8, height = 5)


# 12.Comparación.
#------------------------------------------------------------------------------
# Agrupa los modelos que queremos comparar
model_list <- list(
  A_OLS = res_A$m_ols, A_glmnet = res_A$m_glmnet,
  A_RF = res_A$m_rf, A_GBM = res_A$m_gbm,
  B_OLS = res_B$m_ols, B_glmnet = res_B$m_glmnet,
  B_RF = res_B$m_rf, B_GBM = res_B$m_gbm
)

# Compara sus métricas a través de los folds
resamples_todos <- resamples(model_list)
summary(resamples_todos)

# Genera un Boxplot (bwplot) para ver la dispersión del error
png("resultados/resamples_rmse.png", width = 900, height = 600)
print(bwplot(resamples_todos, metric = "RMSE"))
dev.off()


# 13.Selección de Modelo.
#------------------------------------------------------------------------------
# Define el ranking de complejidad (1 = simple, 8 = complejo)
complejidad <- c(OLS = 1, glmnet = 2, GAM = 3, Arbol = 4,
                 Boosting = 5, RandomForest = 6, Promedio = 7, Stacking = 8)

# Filtra la tabla para quitar los baselines
candidatos <- tabla_val[!grepl("^Baseline", tabla_val$Modelo), ]

# Le asigna el nivel de complejidad a cada modelo
candidatos$Complejidad <- unname(complejidad[candidatos$Modelo])

# Encuentra el error mínimo
rmse_min <- min(candidatos$RMSE)

# Filtra solo los modelos que están dentro del 1% del mínimo
elegibles <- candidatos[candidatos$RMSE <= rmse_min * 1.01, , drop = FALSE]

# De esos elegibles, ordena por complejidad (y luego por RMSE) y toma el primero
ganador <- elegibles[order(elegibles$Complejidad, elegibles$RMSE), , drop = FALSE][1, ]
ganador

# Define cuál especificación usó el ganador.
sp <- if (ganador$Especificacion == "A: calendario") res_A else res_B


# 14.Predicción en test.
#------------------------------------------------------------------------------
# Predice todos los modelos en el conjunto de test.
base_test <- data.frame(
  OLS = predict(sp$m_ols, test),
  glmnet = predict(sp$m_glmnet, test),
  Arbol = predict(sp$m_tree, test),
  RandomForest = predict(sp$m_rf, test),
  Boosting = predict(sp$m_gbm, test),
  GAM = as.numeric(predict(sp$m_gam, test))
)

# Agrega baselines y promedios
base_test$Baseline_media <- mean(train$usage)
base_test$Baseline_yl1 <- test$y_l1
base_test$Promedio <- rowMeans(base_test[, c("OLS", "RandomForest", "Boosting")])
base_test$Stacking <- predict(sp$meta, base_test)

# Extrae la predicción del modelo ganador.
pred_test <- base_test[[as.character(ganador$Modelo)]]

# Calcula RMSE, MAE y R2 para el ganador, la media y el y_l1.
res_test <- rbind(
  Ganador = metricas(test$usage, pred_test),
  Baseline_media = metricas(test$usage, base_test$Baseline_media),
  Baseline_yl1 = metricas(test$usage, base_test$Baseline_yl1)
)
res_test
write.csv(res_test, "resultados/resultado_test.csv")

# Calcula la mejora porcentual frente a los baselines.
100 * (1 - res_test["Ganador", "RMSE"] / res_test["Baseline_media", "RMSE"])
100 * (1 - res_test["Ganador", "RMSE"] / res_test["Baseline_yl1", "RMSE"])

# Evalúa el error solo en fines de semana.
fin <- test$weekend == 1
rbind(
  Ganador = metricas(test$usage[fin], pred_test[fin]),
  Baseline_media = metricas(test$usage[fin], base_test$Baseline_media[fin]),
  Baseline_yl1 = metricas(test$usage[fin], base_test$Baseline_yl1[fin])
)

# Se calcula el intervalo de predicción al 95%
res_val <- val$usage - sp$val_pred[[as.character(ganador$Modelo)]]
q <- quantile(res_val, c(0.025, 0.975))
lim_inf <- pred_test + q[1]
lim_sup <- pred_test + q[2]
# Cobertura del intervalo
mean(test$usage >= lim_inf & test$usage <= lim_sup)

# Calcula la incertidumbre del RMSE usando Bootstrap.
set.seed(2026)
dias_test <- unique(test$dia)
rmse_boot <- rep(NA, 400)
for (b in 1:400) {
  dias_b <- sample(dias_test, length(dias_test), replace = TRUE)
  idx_b <- unlist(lapply(dias_b, function(x) which(test$dia == x)))
  rmse_boot[b] <- sqrt(mean((test$usage[idx_b] - pred_test[idx_b])^2))
}
quantile(rmse_boot, c(0.025, 0.975))


# 15.Gráficos del test.
#------------------------------------------------------------------------------
# Dispersión (Real vs Estimado)
p_test <- ggplot(data.frame(real = test$usage, estimado = pred_test),
                 aes(x = estimado, y = real)) +
  geom_point(alpha = 0.15, color = "steelblue") +
  geom_abline(color = "red") +
  labs(title = "Test: consumo observado vs estimado",
       x = "Estimado (kWh)", y = "Observado (kWh)")
ggsave("resultados/real_vs_estimado.png", p_test, width = 7, height = 5)

# Serie temporal de la primera semana (Real en negro, Predicción en azul)
idx_semana <- which(test$dia <= min(test$dia) + 6)
p_ts <- ggplot(data.frame(fecha = test$fecha[idx_semana],
                          real = test$usage[idx_semana],
                          est = pred_test[idx_semana]),
               aes(x = fecha)) +
  geom_line(aes(y = real), color = "black") +
  geom_line(aes(y = est), color = "steelblue") +
  labs(title = "Primera semana del test: real vs estimado",
       x = NULL, y = "kWh")
ggsave("resultados/serie_test.png", p_ts, width = 9, height = 4)


# 16.Interpretación OLS
#------------------------------------------------------------------------------
interpretar_ols <- function(fmla, nombre) {
  # Ajusta el modelo lineal
  m <- lm(fmla, data = train)
  print(summary(m))
  
  # Calcula los intervalos de confianza de los coeficientes
  ic <- confint(m)
  tabla <- data.frame(variable = names(coef(m)),
                      coef = coef(m),
                      IC_inf = ic[, 1],
                      IC_sup = ic[, 2],
                      row.names = NULL)
  write.csv(tabla, paste0("resultados/coef_OLS_", nombre, ".csv"), row.names = FALSE)
  
  # Calcula el VIF (Variance Inflation Factor)
  X <- model.matrix(fmla, train)[, -1]
  vif <- sapply(seq_len(ncol(X)), function(i) {
    1 / (1 - summary(lm(X[, i] ~ X[, -i]))$r.squared)
  })
  names(vif) <- colnames(X)
  print(round(vif, 1))
  
  # Guarda gráficos de diagnóstico de residuos
  png(paste0("resultados/residuos_OLS_", nombre, ".png"),
      width = 900, height = 800)
  par(mfrow = c(2, 2))
  plot(m)
  dev.off()
  
  m
}
ols_A <- interpretar_ols(fmla_A, "A")
ols_B <- interpretar_ols(fmla_B, "B")


# 17.Interpretación Random Forest y PDP/ICE
#------------------------------------------------------------------------------
# Extrae la importancia de las variables
imp <- varImp(sp$m_rf)$importance
imp <- imp[order(-imp$Overall), , drop = FALSE]
imp
write.csv(imp, "resultados/importancia_rf.csv")

# Selecciona las 2 variables más importantes que no sean "hsin" o "hcos"
vars_num <- c("hsin", "hcos", "y_l1", "y_l4", "y_l96", "y_ma16",
              "react_l1", "lead_react_l1", "pf_l1", "lead_pf_l1")
vars_pdp <- head(intersect(rownames(imp), vars_num), 2)

# Gráficos PDP/ICE
set.seed(2026)
filas_ice <- train[sample(nrow(train), 40), ] # Toma 40 filas al azar

png("resultados/pdp_ice.png", width = 1000, height = 450)
par(mfrow = c(1, 2))
for (v in vars_pdp) {
  grilla <- seq(min(train[[v]]), max(train[[v]]), length.out = 20)
  ice <- sapply(grilla, function(valor) {
    temp <- filas_ice
    temp[[v]] <- valor
    predict(sp$m_rf, temp)
  })
  
  # Dibuja las 40 líneas grises (matplot) y el promedio (línea naranja)
  matplot(grilla, t(ice), type = "l", lty = 1, col = "grey70",
          xlab = v, ylab = "Consumo estimado (kWh)",
          main = paste("PDP e ICE:", v))
  lines(grilla, colMeans(ice), lwd = 3, col = "orange")
}
dev.off()


# 18.Guardar Información
#------------------------------------------------------------------------------
# Guarda la versión de R y paquetes usados para reproducibilidad
writeLines(capture.output(sessionInfo()), "resultados/sessionInfo.txt")
cat("\nListo. Los resultados quedaron en la carpeta 'resultados'.\n")

