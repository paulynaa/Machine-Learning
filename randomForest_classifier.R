library(dplyr)
library(readr)
library(caTools)
library(randomForest)
library(gtools)

set.seed(42)

# ================================
# DUOMENŲ PARUOŠIMAS
# ================================
df <- read_delim("EKG_pupsniu_analize.csv", delim = ";", show_col_types = FALSE)

df_filtered <- df %>%
  filter(label %in% c(0, 2)) %>%
  na.omit()

df_bal <- df_filtered %>%
  group_by(label) %>%
  slice_sample(n = 1000) %>%
  ungroup()

df_subset <- df_bal %>%
  select(label, `RR_r_0/RR_r_1`, wl_side, P_val) %>%
  mutate(label = factor(label))

write.csv(df_subset, "ekg_0_vs_2_raw.csv", row.names = FALSE)
dataset <- read.csv("ekg_0_vs_2_raw.csv")
dataset$label <- factor(dataset$label, levels = c(0, 2))

# ================================
# AIBIŲ PADALINIMAS
# ================================
set.seed(123)

split1 <- sample.split(dataset$label, SplitRatio = 0.8)
train_block <- subset(dataset, split1 == TRUE)
test_set    <- subset(dataset, split1 == FALSE)

split2 <- sample.split(train_block$label, SplitRatio = 0.8)
training_set    <- subset(train_block, split2 == TRUE)
validation_set  <- subset(train_block, split2 == FALSE)

# ================================
# NORMAVIMAS
# ================================
training_set[, -1]    <- scale(training_set[, -1])
validation_set[, -1]  <- scale(validation_set[, -1])
test_set[, -1]        <- scale(test_set[, -1])

set.seed(123)
classifier = randomForest(x = training_set[-1],
                          y = training_set$label,
                          ntree = 500,
)
y_pred_validation = predict(classifier, newdata = validation_set[-1])

cm = table(validation_set[, 1], y_pred_validation)

cm

#Klasifikavimo vertinimas

n = sum(cm) 
nc = nrow(cm) 
diag = diag(cm) 
rowsums = apply(cm, 1, sum) 
colsums = apply(cm, 2, sum)
p = rowsums / n 
q = colsums / n

accuracy = sum(diag) / n
precision = diag / colsums
recall = diag / rowsums
f1 = 2 * precision * recall / (precision + recall)
data.frame(accuracy, precision, recall, f1)


macroAccuracy = mean(accuracy)
macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)
data.frame(macroAccuracy, macroPrecision, macroRecall, macroF1)

plot(classifier)

library(pROC)   # ROC ir AUC skaičiavimui

# ================================
#  ROC KREIVĖ IR AUC SKAIČIAVIMAS
# ================================

# Random Forest turi prognozių tikimybes
y_prob_validation <- predict(classifier, newdata = validation_set[-1], type = "prob")

roc_obj <- roc(response = validation_set$label,
               predictor = y_prob_validation[, "2"],
               levels = c("0", "2"),
               direction = "<")

# Spausdinti AUC
auc_value <- auc(roc_obj)
cat("AUC =", auc_value, "\n")

# Nubraižyti ROC kreivę
plot(roc_obj,
     main = paste("Random Forest. Validavimo aibė. ROC kreivė (AUC =", round(auc_value, 4), ")"),
     col = "#2C7BB6",
     lwd = 3)
grid()

# ==========================================================
# NTREE EKSPERIMENTAI – STABILUS BLOKAS
# ==========================================================
ntree_values <- c(1, 5, 10, 15, 20, 25, 50, 100, 300, 500, 1000)

ntree_results <- data.frame()

for (nt in ntree_values) {
  
  set.seed(123)
  
  model <- randomForest(
    x = training_set[-1],
    y = training_set$label,
    ntree = nt
  )
  
  pred <- predict(model, validation_set[-1])
  
  cm <- table(validation_set$label, pred)
  
  diag_vals <- diag(cm)
  rowsums <- apply(cm, 1, sum)
  colsums <- apply(cm, 2, sum)
  
  accuracy <- sum(diag_vals) / sum(cm)
  precision <- diag_vals / colsums
  recall    <- diag_vals / rowsums
  f1        <- 2 * precision * recall / (precision + recall)
  
  ntree_results <- rbind(
    ntree_results,
    data.frame(
      ntree = nt,
      accuracy = accuracy,
      macroPrecision = mean(precision),
      macroRecall = mean(recall),
      macroF1 = mean(f1)
    )
  )
  plot(model)
}

cat("\n===========================\n")
cat("   NTREE EKSPERIMENTO LENTELĖ")
cat("\n===========================\n")
print(ntree_results)


plot(ntree_results$ntree, ntree_results$accuracy, type="b",
     xlab="ntree", ylab="Accuracy",
     main="Accuracy priklausomybė nuo ntree")

# ==========================================================
# PERMUTACIJOS (3 požymiai, 6 permutacijos)
# ==========================================================
selected_feats <- c("RR_r_0.RR_r_1", "wl_side", "P_val")

perm_list <- permutations(3, 3, selected_feats)
rf_perm_results <- data.frame()

NTREE_PERM <- 500

for (i in 1:nrow(perm_list)) {
  
  perm <- perm_list[i, ]
  cat("\nPožymių tvarka:", paste(perm, collapse = ", "), "\n")
  
  train_perm <- training_set[, c(perm, "label")]
  val_perm   <- validation_set[, c(perm, "label")]
  
  set.seed(123)
  model <- randomForest(
    x = train_perm[, perm],
    y = train_perm$label,
    ntree = NTREE_PERM
  )
  
  pred <- predict(model, newdata = val_perm[, perm])
  cm <- table(val_perm$label, pred)
  print(cm)
  diag_vals <- diag(cm)
  rowsums <- apply(cm, 1, sum)
  colsums <- apply(cm, 2, sum)
  
  accuracy <- sum(diag_vals) / sum(cm)
  precision <- diag_vals / colsums
  recall    <- diag_vals / rowsums
  f1        <- 2 * precision * recall / (precision + recall)
  
  rf_perm_results <- rbind(
    rf_perm_results,
    data.frame(
      perm_order = paste(perm, collapse = ", "),
      accuracy = accuracy,
      macroPrecision = mean(precision),
      macroRecall = mean(recall),
      macroF1 = mean(f1)
    )
  )
}

cat("\n===========================\n")
cat("   PERMUTACIJŲ GALUTINĖ LENTELĖ")
cat("\n===========================\n")

print(rf_perm_results[order(-rf_perm_results$accuracy), ])

# ==========================================================
# HOLD-OUT VALIDACIJA (10 KARTŲ)
# ==========================================================

holdout_results <- data.frame()

for (i in 1:10) {
  
  set.seed(100 + i)   # kiekvienas kartas – kitoks padalinimas
  
  # Atsitiktinis padalinimas
  split1 <- sample.split(dataset$label, SplitRatio = 0.8)
  train_block <- subset(dataset, split1 == TRUE)
  test_block  <- subset(dataset, split1 == FALSE)
  
  split2 <- sample.split(train_block$label, SplitRatio = 0.8)
  training_set <- subset(train_block, split2 == TRUE)
  validation_set <- subset(train_block, split2 == FALSE)
  
  training_set[, -1] <- scale(training_set[, -1])
  validation_set[, -1] <- scale(validation_set[, -1])
  
  model <- randomForest(
    x = training_set[, -1],
    y = training_set$label,
    ntree = 500
  )
  
  pred <- predict(model, validation_set[, -1])
  
  cm <- table(validation_set$label, pred)
  
  diag_vals <- diag(cm)
  rowsums <- apply(cm, 1, sum)
  colsums <- apply(cm, 2, sum)
  
  accuracy <- sum(diag_vals) / sum(cm)
  precision <- diag_vals / colsums
  recall    <- diag_vals / rowsums
  f1        <- 2 * precision * recall / (precision + recall)
  
  holdout_results <- rbind(
    holdout_results,
    data.frame(
      iter = i,
      accuracy = accuracy,
      macroPrecision = mean(precision),
      macroRecall = mean(recall),
      macroF1 = mean(f1)
    )
  )
}

# --- Galutiniai 10 kartų rezultatai ---
cat("\n===========================\n")
cat("   HOLD-OUT (10 kartų) REZULTATAI")
cat("\n===========================\n")

print(holdout_results)

cat("\nVidutiniai rodikliai (Mean):\n")
print(colMeans(holdout_results[, -1]))

cat("\nDispersijos (Var):\n")
print(round(apply(holdout_results[, -1], 2, var),15))

# ==========================================================
# Vidutinės klasifikavimo klaidos skaičiavimas
# ==========================================================

# Klaida kiekviename cikle: 1 - accuracy
holdout_results$error <- 1 - holdout_results$accuracy

# Vidutinė klaida
mean_error <- mean(holdout_results$error)

# Standartinis nuokrypis
sd_error <- sd(holdout_results$error)

cat("Vidutinė klasifikavimo klaida:", round(mean_error, 6), "\n")
cat("Klaidos standartinis nuokrypis:", round(sd_error, 6), "\n")

# ==========================================================
# BOXPLOT: Hold-out (10 kartų) rodiklių vizualizacija
# ==========================================================

par(mfrow = c(2, 2))

# Accuracy
boxplot(
  holdout_results$accuracy,
  main = "Accuracy Boxplot",
  ylab = "Accuracy"
)

# Precision
boxplot(
  holdout_results$macroPrecision,
  main = "Precision Boxplot",
  ylab = "Precision"
)

# Recall
boxplot(
  holdout_results$macroRecall,
  main = "Recall Boxplot",
  ylab = "Recall"
)

# F1
boxplot(
  holdout_results$macroF1,
  main = "F1 Boxplot",
  ylab = "F1 score"
)

par(mfrow = c(1,1))

# ==============================================================================
#       Testavimas
# ==============================================================================

classifier_final = randomForest(x = training_set[-1],
                                y = training_set$label,
                                ntree = 500
)
y_pred_validation_final = predict(classifier_final, newdata = test_set[-1])

cm_final = table(test_set[, 1], y_pred_validation_final)

cm_final

n_final = sum(cm_final) 
nc_final = nrow(cm_final) 
diag_final = diag(cm_final) 
rowsums_final = apply(cm_final, 1, sum) 
colsums_final = apply(cm_final, 2, sum) 
p_final = rowsums / n_final
q_final = colsums / n_final 

accuracy_final = sum(diag_final) / n_final 
precision_final = diag_final / colsums_final
recall_final = diag_final / rowsums_final
f1_final = 2 * precision_final * recall_final / (precision_final + recall_final)
data.frame(accuracy_final, precision_final, recall_final, f1_final)

macroAccuracy_final = mean(accuracy_final)
macroPrecision_final = mean(precision_final)
macroRecall_final = mean(recall_final)
macroF1_final = mean(f1_final)
data.frame(macroAccuracy_final, macroPrecision_final, macroRecall_final, macroF1_final)

# ==============================================================================
#  UMAP DIMENSIJOS MAŽINIMAS
# ==============================================================================
library(umap)
library(randomForest)
library(dplyr)
library(caTools)

set.seed(42) 
training_features <- training_set %>% select(-label)
validation_features <- validation_set %>% select(-label)
test_features <- test_set %>% select(-label)

#  Apmokome UMAP TIK ant mokymo duomenų
umap_model <- umap(training_features, 
                   n_components = 2,
                   n_neighbors = 200,
                   min_dist = 0.5,
                   metric = "euclidean")

# Transformuojame visas aibes
umap_train_data <- umap_model$layout %>% 
  as.data.frame() %>% 
  rename(UMAP_1 = V1, UMAP_2 = V2)
umap_validation_data <- predict(umap_model, validation_features) %>% 
  as.data.frame() %>% 
  rename(UMAP_1 = V1, UMAP_2 = V2)
umap_test_data <- predict(umap_model, test_features) %>% 
  as.data.frame() %>% 
  rename(UMAP_1 = V1, UMAP_2 = V2)

# Suformuojame naujus duomenų rinkinius
training_set_umap <- cbind(label = training_set$label, umap_train_data)
validation_set_umap <- cbind(label = validation_set$label, umap_validation_data)
test_set_umap <- cbind(label = test_set$label, umap_test_data)

# ==============================================================================
#  RANDOM FOREST KLASIFIKAVIMAS SU UMAP DUOMENIMIS
# ==============================================================================

set.seed(123)
# Mokome RF modeli su UMAP gautais 2 požymiais
classifier_umap = randomForest(x = training_set_umap %>% select(-label),
                               y = training_set_umap$label,
                               ntree = 500 
)

y_pred_validation_umap = predict(classifier_umap, 
                                 newdata = validation_set_umap %>% select(-label))

cm_umap = table(validation_set_umap$label, y_pred_validation_umap)
cat("\n--- Sumaišymo matrica (Validavimo aibė, UMAP duomenys) ---\n")
print(cm_umap)

# ==============================================================================
#  KLASIFIKAVIMO KOKYBĖS VERTINIMAS
# ==============================================================================

n = sum(cm_umap) 
diag = diag(cm_umap)
rowsums = apply(cm_umap, 1, sum)
colsums = apply(cm_umap, 2, sum)

accuracy = sum(diag) / n 
precision = diag / colsums
recall = diag / rowsums
f1 = 2 * precision * recall / (precision + recall)

macroAccuracy = accuracy 
macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)

cat("\n--- Klasifikavimo metrikos (UMAP duomenys) ---\n")
print(data.frame(Accuracy = accuracy, Precision = precision, Recall = recall, F1 = f1))
cat("\nMacro metrikos:\n")
print(data.frame(macroAccuracy, macroPrecision, macroRecall, macroF1))


# ==============================================================================
# Identifikuojame klaidingai klasifikuotus taškus
# ==============================================================================

validation_set_umap_pred <- validation_set_umap
validation_set_umap_pred$Predicted_label <- y_pred_validation_umap

# Sukuriame stulpelį nurodantį, ar prognozė teisinga
validation_set_umap_pred <- validation_set_umap_pred %>%
  mutate(Is_Correct = ifelse(label == Predicted_label, "Teisinga", "Klaidinga"))


# ==============================================================================
# VIZUALIZACIJA
# ==============================================================================

# Sukuriame koordinačių tinklelį ir prognozuojame
X_val_plot <- validation_set_umap %>% select(-label)
UMAP_1_range <- seq(min(X_val_plot$UMAP_1) - 1, max(X_val_plot$UMAP_1) + 1, by = 0.05)
UMAP_2_range <- seq(min(X_val_plot$UMAP_2) - 1, max(X_val_plot$UMAP_2) + 1, by = 0.05)
grid_set <- expand.grid(UMAP_1 = UMAP_1_range, UMAP_2 = UMAP_2_range)
y_grid_pred <- predict(classifier_umap, newdata = grid_set)
plotting_set <- cbind(grid_set, Predicted_Class = y_grid_pred)

legend_shape_df <- data.frame(
  x = c(Inf, Inf),
  y = c(Inf, Inf),
  Type = c("Teisinga", "Klaidinga")
)

ggplot() +
  # Sprendimo ribų fonas
  geom_tile(data = plotting_set,
            aes(x = UMAP_1, y = UMAP_2, fill = Predicted_Class),
            alpha = 0.35) +
  scale_fill_manual(values = c("0" = "red", "2" = "green"),
                    labels = c("0 regionas", "2 regionas")) +
  
  # Teisingi taškai 
  geom_point(data = validation_set_umap_pred %>% filter(Is_Correct == "Teisinga"),
             aes(x = UMAP_1, y = UMAP_2, color = label),
             shape = 19, size = 2.5) +
  
  # Klaidingi taškai
  geom_point(data = validation_set_umap_pred %>% filter(Is_Correct == "Klaidinga"),
             aes(x = UMAP_1, y = UMAP_2, color = label),
             shape = 4, size = 4, stroke = 1.5) +
  
  geom_point(data = legend_shape_df,
             aes(x = x, y = y, shape = Type),
             color = "black", size = 3) +
  
  scale_shape_manual(
    name = "Taško tipas",
    values = c("Teisinga" = 19, "Klaidinga" = 4),
    labels = c("Klaidingai klasifikuota", "Teisingai klasifikuota")
  ) +
  
  scale_color_manual(
    values = c("0" = "darkred", "2" = "darkgreen"),
    name = "Tikroji klasė",
    labels = c("Klasė 0", "Klasė 2"),
    guide = guide_legend(
      override.aes = list(shape = 19, size = 3)  
    )
  ) +
  
  labs(
    title = "Random Forest klasifikavimas (UMAP)",
    subtitle = "Validavimo aibė",
    x = "UMAP_1",
    y = "UMAP_2",
    fill = "Prognozuota klasė"
  ) +
  
  theme_minimal() +
  theme(legend.position = "right")


# Prognozės ant testavimo aibės UMAP erdvėje
y_pred_test_umap <- predict(classifier_umap, newdata = test_set_umap %>% select(-label))

test_set_umap_pred <- test_set_umap %>%
  mutate(Predicted_label = y_pred_test_umap,
         Is_Correct = ifelse(label == Predicted_label, "Teisinga", "Klaidinga"))

# Klasifikavimo kokybės rodikliai (testavimo aibė)
cm_test_umap <- table(test_set_umap_pred$label, test_set_umap_pred$Predicted_label)
cat("\n--- Sumaišymo matrica (Testavimo aibė, UMAP duomenys) ---\n")
print(cm_test_umap)

n_test <- sum(cm_test_umap)
diag_test <- diag(cm_test_umap)
rowsums_test <- apply(cm_test_umap, 1, sum)
colsums_test <- apply(cm_test_umap, 2, sum)

accuracy_test <- sum(diag_test) / n_test
precision_test <- diag_test / colsums_test
recall_test <- diag_test / rowsums_test
f1_test <- 2 * precision_test * recall_test / (precision_test + recall_test)

cat("\n--- Klasifikavimo metrikos (UMAP duomenys, testavimas) ---\n")
print(data.frame(Accuracy = accuracy_test, Precision = precision_test, Recall = recall_test, F1 = f1_test))
cat("\nMacro metrikos (testavimas):\n")
print(data.frame(
  macroAccuracy = accuracy_test,
  macroPrecision = mean(precision_test, na.rm = TRUE),
  macroRecall = mean(recall_test, na.rm = TRUE),
  macroF1 = mean(f1_test, na.rm = TRUE)
))

# Sprendimo ribos paruošimas testavimo aibei
UMAP_1_range <- seq(min(test_set_umap$UMAP_1) - 1, max(test_set_umap$UMAP_1) + 1, by = 0.05)
UMAP_2_range <- seq(min(test_set_umap$UMAP_2) - 1, max(test_set_umap$UMAP_2) + 1, by = 0.05)
grid_test <- expand.grid(UMAP_1 = UMAP_1_range, UMAP_2 = UMAP_2_range)
y_grid_pred_test <- predict(classifier_umap, newdata = grid_test)
plotting_set_test <- cbind(grid_test, Predicted_Class = y_grid_pred_test)

# Vizualizacija testavimo aibei
library(ggplot2)

legend_shape_df_test <- data.frame(
  x = c(Inf, Inf),
  y = c(Inf, Inf),
  Type = c("Teisinga", "Klaidinga")
)

ggplot() +
  geom_tile(data = plotting_set_test,
            aes(x = UMAP_1, y = UMAP_2, fill = Predicted_Class),
            alpha = 0.35) +
  scale_fill_manual(values = c("0" = "red", "2" = "green"),
                    labels = c("0 regionas", "2 regionas"),
                    name = "Prognozuota klasė") +
  
  geom_point(data = test_set_umap_pred %>% filter(Is_Correct == "Teisinga"),
             aes(x = UMAP_1, y = UMAP_2, color = label),
             shape = 19, size = 2.5) +
  
  geom_point(data = test_set_umap_pred %>% filter(Is_Correct == "Klaidinga"),
             aes(x = UMAP_1, y = UMAP_2, color = label),
             shape = 4, size = 4, stroke = 1.5) +
  
  geom_point(data = legend_shape_df_test,
             aes(x = x, y = y, shape = Type),
             color = "black", size = 3) +
  
  scale_shape_manual(
    name = "Taško tipas",
    values = c("Teisinga" = 19, "Klaidinga" = 4),
    labels = c("Klaidingai klasifikuota", "Teisingai klasifikuota")
  ) +
  
  scale_color_manual(
    values = c("0" = "darkred", "2" = "darkgreen"),
    name = "Tikroji klasė",
    labels = c("Klasė 0", "Klasė 2"),
    guide = guide_legend(override.aes = list(shape = 19, size = 3))
  ) +
  
  labs(
    title = "Random Forest klasifikavimas (UMAP)",
    subtitle = "Testavimo aibė",
    x = "UMAP_1",
    y = "UMAP_2"
  ) +
  
  theme_minimal() +
  theme(legend.position = "right")


library(pROC)

# ======================================================================
# ROC: VALIDAVIMO AIBĖ (3 požymiai vs UMAP 2D)
# ======================================================================
y_prob_val_raw <- predict(classifier, newdata = validation_set[-1], type = "prob")

roc_val_raw <- roc(
  response = validation_set$label,
  predictor = y_prob_val_raw[, "2"],
  levels = c("0", "2"),
  direction = "<"
)

auc_val_raw <- auc(roc_val_raw)

y_prob_val_umap <- predict(classifier_umap, newdata = validation_set_umap %>% select(-label), type = "prob")[, "2"]

roc_val_umap <- roc(
  response = validation_set_umap$label,
  predictor = y_prob_val_umap,
  levels = c("0", "2"),
  direction = "<"
)

auc_val_umap <- auc(roc_val_umap)

plot(roc_val_raw,
     col = "#1F78B4",
     lwd = 3,
     main = "Random Forest. Validavimo aibė. ROC kreivės: 3 požymiai ir UMAP 2D")

plot(roc_val_umap, col = "#33A02C", lwd = 3, add = TRUE)

legend("bottomright",
       legend = c(
         paste("Random Forest (3 požymiai), AUC =", round(auc_val_raw, 4)),
         paste("Random Forest (UMAP 2D), AUC =", round(auc_val_umap, 4))
       ),
       col = c("#1F78B4", "#33A02C"),
       lwd = 3)


# ======================================================================
#  ROC: TESTAVIMO AIBĖ (3 požymiai vs UMAP 2D)
# ======================================================================
y_prob_test_raw <- predict(classifier_final, newdata = test_set[-1], type = "prob")[, "2"]

roc_test_raw <- roc(
  response = test_set$label,
  predictor = y_prob_test_raw,
  levels = c("0", "2"),
  direction = "<"
)

auc_test_raw <- auc(roc_test_raw)

y_prob_test_umap <- predict(classifier_umap, newdata = test_set_umap %>% select(-label), type = "prob")[, "2"]

roc_test_umap <- roc(
  response = test_set_umap$label,
  predictor = y_prob_test_umap,
  levels = c("0", "2"),
  direction = "<"
)

auc_test_umap <- auc(roc_test_umap)

plot(roc_test_raw,
     col = "#E31A1C",
     lwd = 3,
     main = "Random Forest. Testavimo aibė. ROC kreivės: 3 požymiai ir UMAP 2D")

plot(roc_test_umap, col = "#6A3D9A", lwd = 3, add = TRUE)

legend("bottomright",
       legend = c(
         paste("Random Forest (3 požymiai), AUC =", round(auc_test_raw, 4)),
         paste("Random Forest (UMAP 2D), AUC =", round(auc_test_umap, 4))
       ),
       col = c("#E31A1C", "#6A3D9A"),
       lwd = 3)

# ======================================================================
#  OUTLIERS ANALIZĖ
# ======================================================================
get_outliers <- function(df, features) {
  
  mild_idx <- c()
  extreme_idx <- c()
  
  for (f in features) {
    Q1 <- quantile(df[[f]], 0.25, na.rm=TRUE)
    Q3 <- quantile(df[[f]], 0.75, na.rm=TRUE)
    IQRv <- IQR(df[[f]], na.rm=TRUE)
    
    lower_mild  <- Q1 - 1.5 * IQRv
    upper_mild  <- Q3 + 1.5 * IQRv
    lower_ext   <- Q1 - 3 * IQRv
    upper_ext   <- Q3 + 3 * IQRv
    
    mild_idx    <- union(mild_idx, which(df[[f]] < lower_mild | df[[f]] > upper_mild))
    extreme_idx <- union(extreme_idx, which(df[[f]] < lower_ext  | df[[f]] > upper_ext))
  }
  
  list(
    mild_idx = mild_idx,
    extreme_idx = extreme_idx
  )
}

assign_outlier_labels <- function(df, features) {
  
  outliers <- get_outliers(df, features)
  
  outlier_cat <- rep("normal", nrow(df))
  
  # Priskiriame "mild" ir "extreme" kategorijas pagal indeksus
  outlier_cat[outliers$mild_idx] <- "mild"
  outlier_cat[outliers$extreme_idx] <- "extreme"
  
  return(outlier_cat)
}

validation_outlier_cat_3feat <- assign_outlier_labels(validation_set, c("RR_r_0.RR_r_1", "wl_side", "P_val"))
test_outlier_cat_3feat <- assign_outlier_labels(test_set, c("RR_r_0.RR_r_1", "wl_side", "P_val"))

cat("\n--- IQR Outlier counts VALIDAVIMAS (3 požymiai) ---\n")
print(table(validation_outlier_cat_3feat))

cat("\n--- IQR Outlier counts TESTAVIMAS (3 požymiai) ---\n")
print(table(test_outlier_cat_3feat))

val_out <- get_outliers(validation_set, c("RR_r_0.RR_r_1", "wl_side", "P_val"))
test_out <- get_outliers(test_set, c("RR_r_0.RR_r_1", "wl_side", "P_val"))

# Mild ir extreme išskirtinių duomenų rinkiniai:
val_mild <- validation_set[val_out$mild_idx, ]
val_extreme <- validation_set[val_out$extreme_idx, ]

test_mild <- test_set[test_out$mild_idx, ]
test_extreme <- test_set[test_out$extreme_idx, ]

# Ištraukiame mild ir extreme outlierius iš validavimo ir testavimo aibių
val_out <- get_outliers(validation_set, c("RR_r_0.RR_r_1", "wl_side", "P_val"))
test_out <- get_outliers(test_set, c("RR_r_0.RR_r_1", "wl_side", "P_val"))

val_mild <- validation_set[val_out$mild_idx, ]
val_extreme <- validation_set[val_out$extreme_idx, ]

test_mild <- test_set[test_out$mild_idx, ]
test_extreme <- test_set[test_out$extreme_idx, ]

# Funkcija prognozei ir sumaišymo matrikai (naudojame jau apmokytą classifier)
run_rf_cm <- function(rf_model, test_df) {
  if (nrow(test_df) == 0) {
    cat("Nėra duomenų šioje grupėje.\n")
    return(NULL)
  }
  pred <- predict(rf_model, newdata = test_df[, -1])  # -1, nes pirmas stulpelis label'as
  cm <- table(Real = test_df$label, Predicted = pred)
  return(cm)
}

cat("\n=== VALIDAVIMO AIBĖ: Mild Outliers (3 požymiai) ===\n")
cm_val_mild <- run_rf_cm(classifier, val_mild)
print(cm_val_mild)

cat("\n=== VALIDAVIMO AIBĖ: Extreme Outliers (3 požymiai) ===\n")
cm_val_extreme <- run_rf_cm(classifier, val_extreme)
print(cm_val_extreme)

cat("\n=== TESTAVIMO AIBĖ: Mild Outliers (3 požymiai) ===\n")
cm_test_mild <- run_rf_cm(classifier_final, test_mild)
print(cm_test_mild)

cat("\n=== TESTAVIMO AIBĖ: Extreme Outliers (3 požymiai) ===\n")
cm_test_extreme <- run_rf_cm(classifier_final, test_extreme)
print(cm_test_extreme)

# ==========================================================
#  RANDOM FOREST + UMAP — HOLD-OUT (10 ITERACIJŲ)
# ==========================================================

library(randomForest)
library(umap)
library(caTools)
library(dplyr)

set.seed(999)

rf_umap_holdout <- data.frame()

for (i in 1:10) {
  cat("\n========================\n")
  cat("     ITERACIJA", i, "\n")
  cat("========================\n")
  
  set.seed(100 + i)
  
  split1 <- sample.split(dataset$label, SplitRatio = 0.8)
  train_block <- subset(dataset, split1 == TRUE)
  test_block  <- subset(dataset, split1 == FALSE)
  
  split2 <- sample.split(train_block$label, SplitRatio = 0.8)
  training_set <- subset(train_block, split2 == TRUE)
  validation_set <- subset(train_block, split2 == FALSE)
  
  training_set[, -1] <- scale(training_set[, -1])
  validation_set[, -1] <- scale(validation_set[, -1])
 
  umap_model_iter <- umap(
    training_set %>% select(-label),
    n_neighbors = 200,
    min_dist = 0.5,
    n_components = 2,
    metric = "euclidean"
  )
  
  umap_train <- as.data.frame(umap_model_iter$layout)
  colnames(umap_train) <- c("UMAP1", "UMAP2")
  umap_train$label <- training_set$label
  
  umap_val <- as.data.frame(predict(umap_model_iter, validation_set %>% select(-label)))
  colnames(umap_val) <- c("UMAP1", "UMAP2")
  umap_val$label <- validation_set$label
  
  rf_model <- randomForest(
    x = umap_train[, c("UMAP1", "UMAP2")],
    y = umap_train$label,
    ntree = 500
  )
  
  pred_val <- predict(rf_model, newdata = umap_val[, c("UMAP1", "UMAP2")])
  
  cm <- table(umap_val$label, pred_val)
  print(cm)
  
  diag_vals <- diag(cm)
  rowsums <- rowSums(cm)
  colsums <- colSums(cm)
  
  accuracy <- sum(diag_vals) / sum(cm)
  precision <- diag_vals / colsums
  recall    <- diag_vals / rowsums
  f1        <- 2 * precision * recall / (precision + recall)
  
  rf_umap_holdout <- rbind(
    rf_umap_holdout,
    data.frame(
      iter = i,
      accuracy = accuracy,
      macroPrecision = mean(precision, na.rm = TRUE),
      macroRecall = mean(recall, na.rm = TRUE),
      macroF1 = mean(f1, na.rm = TRUE)
    )
  )
}

# ==========================================================
#  GALUTINIAI REZULTATAI
# ==========================================================

cat("\n=================================\n")
cat(" RANDOM FOREST + UMAP — 10 ITER. REZULTATAI\n")
cat("=================================\n")

print(rf_umap_holdout)

cat("\nVidurkiai:\n")
print(colMeans(rf_umap_holdout[, -1]))

cat("\nDispersijos:\n")
print(apply(rf_umap_holdout[, -1], 2, var))

# ==========================================================
#  VIDUTINĖ KLASIFIKAVIMO KLAIDA
# ==========================================================

rf_umap_holdout$error <- 1 - rf_umap_holdout$accuracy
mean_error_umap_rf <- mean(rf_umap_holdout$error)
sd_error_umap_rf <- sd(rf_umap_holdout$error)

cat("\nVidutinė klasifikavimo klaida:", round(mean_error_umap_rf, 6), "\n")
cat("Klaidos SD:", round(sd_error_umap_rf, 6), "\n")

# ==========================================================
#  BOX PLOTS
# ==========================================================

par(mfrow = c(2,2))

boxplot(rf_umap_holdout$accuracy, main = "RF+UMAP Accuracy", ylab = "Accuracy")
boxplot(rf_umap_holdout$macroPrecision, main = "RF+UMAP Precision", ylab = "Precision")
boxplot(rf_umap_holdout$macroRecall, main = "RF+UMAP Recall", ylab = "Recall")
boxplot(rf_umap_holdout$macroF1, main = "RF+UMAP F1", ylab = "F1 score")

par(mfrow = c(1,1))
