library(dplyr)
library(readr)
set.seed(42)
# ================================
# DUOMENŲ PARUOŠIMAS
# ================================
df <- read_delim("EKG_pupsniu_analize.csv", delim = ";", show_col_types = FALSE)

df_filtered <- df %>%
  filter(label %in% c(0, 2))

df_filtered <- df_filtered %>% na.omit()

df_bal <- df_filtered %>%
  group_by(label) %>%
  slice_sample(n = 1000) %>%
  ungroup()

write.csv(df_bal, "ekg_0_vs_2_raw.csv", row.names = FALSE)

dataset <- read.csv("ekg_0_vs_2_raw.csv")

which(is.na(dataset))
na_count <- colSums(is.na(dataset))
na_count
dataset$label = factor(dataset$label, levels = c(0, 2))

library(caTools)
set.seed(123)
# ================================
# AIBIŲ PADALINIMAS
# ================================
split1 <- sample.split(dataset$label, SplitRatio = 0.8)
train_block <- subset(dataset, split1 == TRUE)
test_set    <- subset(dataset, split1 == FALSE)

split2 <- sample.split(train_block$label, SplitRatio = 0.8)
training_set    <- subset(train_block, split2 == TRUE)
validation_set  <- subset(train_block, split2 == FALSE)

# ================================
# NORMAVIMAS
# ================================
training_set[-32] = scale(training_set[-32])
test_set[-32] = scale(test_set[-32])
validation_set[-32] = scale(validation_set[-32])


library(dplyr)
library(class)

label_index <- ncol(training_set)

features_all <- colnames(training_set)[-label_index]

evaluate_knn <- function(train_set, val_set, k) {
  
  label_idx <- ncol(train_set)
  
  pred <- knn(
    train = train_set[, -label_idx, drop = FALSE],
    test  = val_set[, -label_idx, drop = FALSE],
    cl    = train_set[, label_idx],
    k = k
  )
  
  cm <- table(val_set[, label_idx], pred)
  acc <- sum(diag(cm)) / sum(cm)
  return(acc)
}

# ===========================================
# SINGLE-FEATURE EVALUATION
# ===========================================

label_index <- ncol(training_set)
all_features <- colnames(training_set)[-label_index]
default_k <- 5

sfe_results <- data.frame(
  feature = all_features,
  accuracy = NA
)

cat("\n Single-Feature Evaluation\n")

for (i in seq_along(all_features)) {
  
  feat <- all_features[i]
  
  # tik vienas požymis
  train_feat <- training_set[, feat, drop = FALSE]
  val_feat   <- validation_set[, feat, drop = FALSE]
  
  pred <- knn(
    train = train_feat,
    test  = val_feat,
    cl = training_set[, label_index],
    k = default_k
  )
  
  cm <- table(validation_set[, label_index], pred)
  acc <- sum(diag(cm)) / sum(cm)
  
  sfe_results$accuracy[i] <- acc
  
  cat("Požymis:", feat, "| ACC =", acc, "\n")
}

sfe_results <- sfe_results[order(-sfe_results$accuracy), ]

cat("\n Single-Feature Evaluation rezultatai:\n")
print(sfe_results)

# ================================
# ATRINKTI POŽYMIAI
# ================================

selected_feats <- c("RR_r_0.RR_r_1", "wl_side", "P_val")

train_sel <- training_set[, c(selected_feats, "label")]
val_sel   <- validation_set[, c(selected_feats, "label")]
test_sel  <- test_set[, c(selected_feats, "label")]

# ============================================
#  IŠSAMUS K parametro VERTINIMAS SU METRIKOMIS
# ============================================

k_values <- seq(1, 51, by = 2)

metrics_results <- data.frame(
  k = integer(),
  Accuracy = numeric(),
  Precision = numeric(),
  Recall = numeric(),
  F1 = numeric(),
  stringsAsFactors = FALSE
)

for (k in k_values) {
  
  label_idx <- ncol(train_sel)
  
  pred <- knn(
    train = train_sel[, -label_idx, drop = FALSE],
    test  = val_sel[, -label_idx, drop = FALSE],
    cl    = train_sel[, label_idx],
    k = k
  )
  print(k)
  cm = table(val_sel[, label_idx], pred)
  print(cm)
  
  n  <- sum(cm)
  diag_vals <- diag(cm)
  rowsums <- rowSums(cm)
  colsums <- colSums(cm)
  
  accuracy <- sum(diag_vals) / n
  precision <- diag_vals / colsums
  recall    <- diag_vals / rowsums
  f1        <- 2 * precision * recall / (precision + recall)
  
  macroPrecision <- mean(precision, na.rm = TRUE)
  macroRecall    <- mean(recall, na.rm = TRUE)
  macroF1        <- mean(f1, na.rm = TRUE)
  
  metrics_results <- rbind(
    metrics_results,
    data.frame(
      k = k,
      Accuracy = accuracy,
      Precision = macroPrecision,
      Recall = macroRecall,
      F1 = macroF1
    )
  )
}

metrics_results <- metrics_results[order(-metrics_results$Accuracy), ]

cat("\n Pilna metrikų lentelė (VALIDATION)\n")
print(metrics_results)

# ============================================
# POŽYMIŲ EILIŠKUMO PERMUTACIJŲ TESTAVIMAS
# ============================================

library(gtools)

k_final <- 5   

perm_list <- permutations(
  n = length(selected_feats),
  r = length(selected_feats),
  v = selected_feats
)

cat("\n============================================\n")
cat(" Testuojame požymių eiliškumą\n")
cat("============================================\n")

for (i in 1:nrow(perm_list)) {
  
  perm <- perm_list[i, ]
  
  cat("\n=====================================\n")
  cat("Požymių tvarka:", paste(perm, collapse = ", "), "\n")
  cat("=====================================\n")
  
  # nauji rinkiniai pagal permutaciją
  train_perm <- training_set[, c(perm, "label")]
  val_perm   <- validation_set[, c(perm, "label")]
  
  label_idx <- ncol(train_perm)
  
  pred <- knn(
    train = train_perm[, -label_idx, drop = FALSE],
    test  = val_perm[, -label_idx, drop = FALSE],
    cl    = train_perm[, label_idx],
    k = k_final
  )
  
  cm <- table(val_perm[, label_idx], pred)
  print(cm)
  
  n  <- sum(cm)
  diag_vals <- diag(cm)
  rowsums <- rowSums(cm)
  colsums <- colSums(cm)
  
  accuracy <- sum(diag_vals) / n
  precision <- diag_vals / colsums
  recall    <- diag_vals / rowsums
  f1        <- 2 * precision * recall / (precision + recall)
  
  macroPrecision <- mean(precision, na.rm = TRUE)
  macroRecall    <- mean(recall, na.rm = TRUE)
  macroF1        <- mean(f1, na.rm = TRUE)
  
  cat("\nKlasių metrikos\n")
  print(data.frame(precision, recall, f1))
  
  cat("\n Kokybės vertinimas\n")
  cat("Macro Precision =", macroPrecision, "\n")
  cat("Macro Recall    =", macroRecall, "\n")
  cat("Macro F1 Score  =", macroF1, "\n")
  
  cat("\nAccuracy (bendras) =", accuracy, "\n")
}

# ==========================================================
#  HOLD-OUT VALIDACIJA (10 kartų)
# ==========================================================

library(class)

holdout_knn <- data.frame()

k_final <- 5

for (i in 1:10) {
  
  set.seed(100 + i)
  
  # Atsitiktinis padalinimas
  split1 <- sample.split(dataset$label, SplitRatio = 0.8)
  train_block <- subset(dataset, split1 == TRUE)
  test_block  <- subset(dataset, split1 == FALSE)
  
  split2 <- sample.split(train_block$label, SplitRatio = 0.8)
  training_set <- subset(train_block, split2 == TRUE)
  validation_set <- subset(train_block, split2 == FALSE)
  
  # Normavimas
  training_set[, -ncol(training_set)] <- scale(training_set[, -ncol(training_set)])
  validation_set[, -ncol(validation_set)] <- scale(validation_set[, -ncol(validation_set)])
  
  # tik pasirinkti požymiai
  train_sel <- training_set[, c(selected_feats, "label")]
  val_sel   <- validation_set[, c(selected_feats, "label")]
  
  label_idx <- ncol(train_sel)
  
  pred <- knn(
    train = train_sel[, -label_idx],
    test  = val_sel[, -label_idx],
    cl    = train_sel[, label_idx],
    k = k_final
  )
  
  cm <- table(val_sel[, label_idx], pred)
  
  diag_vals <- diag(cm)
  rowsums <- rowSums(cm)
  colsums <- colSums(cm)
  
  accuracy <- sum(diag_vals) / sum(cm)
  precision <- diag_vals / colsums
  recall    <- diag_vals / rowsums
  f1        <- 2 * precision * recall / (precision + recall)
  
  holdout_knn <- rbind(
    holdout_knn,
    data.frame(
      iter = i,
      accuracy = accuracy,
      macroPrecision = mean(precision, na.rm = TRUE),
      macroRecall = mean(recall, na.rm = TRUE),
      macroF1 = mean(f1, na.rm = TRUE)
    )
  )
}

#  Galutiniai KNN rezultatai
cat("\n===========================\n")
cat("   KNN HOLD-OUT (10 kartų) REZULTATAI")
cat("\n===========================\n")

print(holdout_knn)

cat("\nVidutiniai rodikliai:\n")
print(colMeans(holdout_knn[, -1]))

cat("\nDispersijos:\n")
print(round(apply(holdout_knn[, -1], 2, var), 15))


# ==========================================================
#  Vidutinė klasifikavimo klaida
# ==========================================================

holdout_knn$error <- 1 - holdout_knn$accuracy
mean_error_knn <- mean(holdout_knn$error)
sd_error_knn <- sd(holdout_knn$error)

cat("\nVidutinė klasifikavimo klaida:", round(mean_error_knn, 6), "\n")
cat("Klaidos standartinis nuokrypis:", round(sd_error_knn, 6), "\n")


# ==========================================================
#  BOXPLOT vizualizacija
# ==========================================================

par(mfrow = c(2, 2))

boxplot(holdout_knn$accuracy, main = "KNN Accuracy", ylab = "Accuracy")
boxplot(holdout_knn$macroPrecision, main = "KNN Precision", ylab = "Precision")
boxplot(holdout_knn$macroRecall, main = "KNN Recall", ylab = "Recall")
boxplot(holdout_knn$macroF1, main = "KNN F1", ylab = "F1 score")

par(mfrow = c(1, 1))


# ==========================================================
# ROC KREIVĖS IR AUC
# ==========================================================

library(pROC)

label_idx <- ncol(test_sel)

knn_prob <- function(train_data, test_data, train_labels, k) {
  pred_knn <- knn(
    train = train_data,
    test  = test_data,
    cl    = train_labels,
    k = k,
    prob = TRUE       
  )
  
  p <- attr(pred_knn, "prob")
  p <- ifelse(pred_knn == "2", p, 1 - p)
  
  return(as.numeric(p))
}

knn_probs <- knn_prob(
  train_data = train_sel[, -label_idx],
  test_data  = test_sel[, -label_idx],
  train_labels = train_sel[, label_idx],
  k = k_final
)

true_labels <- ifelse(test_sel$label == 2, 1, 0)

knn_roc <- roc(true_labels, knn_probs)

auc_value <- auc(knn_roc)
cat("\nAUC =", auc_value, "\n")

# ROC kreivės nubraižymas
plot(
  knn_roc,
  col = "blue",
  lwd = 3,
  main = paste("KNN. Testavimo aibė. ROC kreivė (AUC = ", auc_value, ")")
)
grid()

# ===========================================
#                  Testavimas
# ===========================================

label_idx <- ncol(train_sel)

pred_test <- knn(
  train = train_sel[, -label_idx, drop = FALSE],
  test  = test_sel[, -label_idx, drop = FALSE],
  cl    = train_sel[, label_idx],
  k = 5
)

cm_final = table(test_sel[, label_idx], pred_test)
print(cm_final)

n_final  <- sum(cm_final)
diag_vals_final <- diag(cm_final)
rowsums_final <- rowSums(cm_final)
colsums_final <- colSums(cm_final)

accuracy_final <- sum(diag_vals_final) / n_final
accuracy_final
precision_final <- diag_vals_final / colsums_final
recall_final    <- diag_vals_final / rowsums_final
f1_final        <- 2 * precision_final * recall_final / (precision_final + recall_final)

macroPrecision_final <- mean(precision_final, na.rm = TRUE)
macroRecall_final    <- mean(recall_final, na.rm = TRUE)
macroF1_final        <- mean(f1_final, na.rm = TRUE)
macroPrecision_final
macroRecall_final
macroF1_final

# ===========================================
#                  UMAP
# ===========================================
library(umap)
set.seed(42)

training_features <- train_sel %>% select(-label)
validation_features <- val_sel %>% select(-label)
test_features <- test_sel %>% select(-label)

umap_model_knn <- umap(training_features,
                       n_neighbors = 200,
                       min_dist = 0.5,
                       n_components = 2,
                       metric = "euclidean")

umap_train_knn <- as.data.frame(umap_model_knn$layout)
colnames(umap_train_knn) <- c("UMAP1", "UMAP2")

umap_val_knn <- as.data.frame(predict(umap_model_knn, validation_features))
colnames(umap_val_knn) <- c("UMAP1", "UMAP2")

umap_test_knn <- as.data.frame(predict(umap_model_knn, test_features))
colnames(umap_test_knn) <- c("UMAP1", "UMAP2")

train_knn_umap <- cbind(label = train_sel$label, umap_train_knn)
val_knn_umap   <- cbind(label = val_sel$label, umap_val_knn)
test_knn_umap  <- cbind(label = test_sel$label, umap_test_knn)

library(class)

k_final <- 5

pred_knn_val_umap <- knn(
  train = train_knn_umap[, c("UMAP1", "UMAP2")],
  test  = val_knn_umap[, c("UMAP1", "UMAP2")],
  cl    = train_knn_umap$label,
  k = k_final
)

val_knn_umap$pred <- pred_knn_val_umap
val_knn_umap$correct <- ifelse(val_knn_umap$label == val_knn_umap$pred, TRUE, FALSE)

cm = table(val_knn_umap$label, val_knn_umap$pred)
print(cm)

n  <- sum(cm)
diag_vals <- diag(cm)
rowsums <- rowSums(cm)
colsums <- colSums(cm)

accuracy <- sum(diag_vals) / n

precision <- diag_vals / colsums
recall    <- diag_vals / rowsums
f1        <- 2 * precision * recall / (precision + recall)

macroPrecision <- mean(precision, na.rm = TRUE)
macroRecall    <- mean(recall, na.rm = TRUE)
macroF1        <- mean(f1, na.rm = TRUE)

cat("\n--- KNN + UMAP METRIKOS (VALIDATION) ---\n")
cat("Accuracy: ", accuracy, "\n")
cat("Precision (per class): ", precision, "\n")
cat("Recall (per class): ", recall, "\n")
cat("F1 (per class): ", f1, "\n\n")
cat("Macro Precision: ", macroPrecision, "\n")
cat("Macro Recall: ", macroRecall, "\n")
cat("Macro F1: ", macroF1, "\n")


UMAP1_seq <- seq(min(val_knn_umap$UMAP1)-1, max(val_knn_umap$UMAP1)+1, by=0.05)
UMAP2_seq <- seq(min(val_knn_umap$UMAP2)-1, max(val_knn_umap$UMAP2)+1, by=0.05)

grid_knn <- expand.grid(UMAP1 = UMAP1_seq, UMAP2 = UMAP2_seq)

grid_pred <- knn(
  train = train_knn_umap[, c("UMAP1", "UMAP2")],
  test  = grid_knn,
  cl    = train_knn_umap$label,
  k = k_final
)

grid_knn$pred <- grid_pred

library(ggplot2)
library(ggplot2)
library(dplyr)

val_knn_umap$correct_flag <- ifelse(val_knn_umap$correct, "Teisingai", "Klaidingai")
val_knn_umap$correct_flag <- factor(val_knn_umap$correct_flag, levels = c("Teisingai", "Klaidingai"))

tile_colors <- c("0" = "red", "2" = "green")
point_colors <- c("0" = "darkred", "2" = "darkgreen")

library(ggplot2)

ggplot() +
  geom_tile(data = grid_knn,
            aes(x = UMAP1, y = UMAP2, fill = pred),
            alpha = 0.35) +
  scale_fill_manual(values = tile_colors,
                    name = "Prognozuota klasė",
                    labels = c("Klasė 0", "Klasė 2")) +
  geom_point(
    data = val_knn_umap,
    aes(x = UMAP1, y = UMAP2, color = as.factor(label), shape = correct_flag),
    size = 2.5, stroke = 1.5, alpha = 0.95
  ) +
  scale_shape_manual(
    name = "Taško tipas",
    values = c("Teisingai" = 19, "Klaidingai" = 4),
    labels = c("Teisingai klasifikuota", "Klaidingai klasifikuota")
  ) +
  scale_color_manual(
    name = "Tikroji klasė",
    values = point_colors,
    labels = c("Klasė 0", "Klasė 2"),
    guide = guide_legend(override.aes = list(shape = 19, size = 4))
  ) +
  labs(
    title = "KNN klasifikacija (UMAP)",
    subtitle = "Validavimo aibė.",
    x = "UMAP1",
    y = "UMAP2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# ==========================================================
#  ROC KREIVĖ – VALIDAVIMO AIBĖ (KNN + UMAP)
# ==========================================================

library(FNN)  
library(pROC)

true_val_labels <- ifelse(val_knn_umap$label == 2, 1, 0)

knn_prob_umap <- function(train_points, train_labels, test_points, k = 5) {
  
  nn <- get.knnx(train_points, test_points, k)
  
  # kaimynų klasės
  neigh_classes <- matrix(train_labels[nn$nn.index], nrow = nrow(test_points))
  
  probs <- rowMeans(neigh_classes == 2)
  
  return(probs)
}

# Apskaičiuojame tikimybes validavimo aibei
val_probs_umap <- knn_prob_umap(
  train_points = train_knn_umap[, c("UMAP1", "UMAP2")],
  train_labels = train_knn_umap$label,
  test_points  = val_knn_umap[, c("UMAP1", "UMAP2")],
  k = k_final
)

# ROC kreivė
roc_val_umap <- roc(true_val_labels, val_probs_umap)
auc_val_umap <- auc(roc_val_umap)

cat("\nAUC (KNN + UMAP, VALIDATION) =", auc_val_umap, "\n")

# Braižymas
plot(
  roc_val_umap,
  col = "blue",
  lwd = 3,
  main = paste("KNN. Validavimo aibė. UMAP ROC kreivė\nAUC =", round(auc_val_umap, 4))
)
grid()

# =======================================
# ROC: KNN (3 pož) vs KNN+UMAP (validavimo)
# =======================================

library(pROC)

knn_probs_val <- knn_prob(
  train_data = train_sel[, -label_idx],
  test_data  = val_sel[, -label_idx],
  train_labels = train_sel[, label_idx],
  k = k_final
)

true_labels_val <- ifelse(val_sel$label == 2, 1, 0)

roc_knn_3feat <- roc(true_labels_val, knn_probs_val)
auc_knn_3feat <- auc(roc_knn_3feat)

roc_knn_umap <- roc(true_val_labels, val_probs_umap)
auc_knn_umap <- auc(roc_knn_umap)

plot(
  roc_knn_3feat,
  col = "red",
  lwd = 3,
  main = "KNN. Validavimo aibė. ROC kreivės: 3 požymiai ir UMAP 2D"
)
grid()

lines(roc_knn_umap, col = "blue", lwd = 3)

legend(
  "bottomright",
  legend = c(
    paste0("KNN (3 požymiai), AUC = ", round(auc_knn_3feat, 4)),
    paste0("KNN (UMAP 2D), AUC = ", round(auc_knn_umap, 4))
  ),
  col = c("red", "blue"),
  lwd = 3,
  bty = "n"
)

# ==============================
# UMAP vizualizacija testavimo aibei
# ==============================

library(class)
library(ggplot2)

pred_knn_test_umap <- knn(
  train = train_knn_umap[, c("UMAP1", "UMAP2")],
  test  = test_knn_umap[, c("UMAP1", "UMAP2")],
  cl    = train_knn_umap$label,
  k = k_final
)

test_knn_umap$pred <- pred_knn_test_umap
test_knn_umap$correct <- ifelse(test_knn_umap$label == test_knn_umap$pred, TRUE, FALSE)

cm_test = table(test_knn_umap$label, test_knn_umap$pred)
print(cm_test)

n_test  <- sum(cm_test)
diag_vals_test <- diag(cm_test)
rowsums_test <- rowSums(cm_test)
colsums_test <- colSums(cm_test)

accuracy_test <- sum(diag_vals_test) / n_test
precision_test <- diag_vals_test / colsums_test
recall_test    <- diag_vals_test / rowsums_test
f1_test        <- 2 * precision_test * recall_test / (precision_test + recall_test)

macroPrecision_test <- mean(precision_test, na.rm = TRUE)
macroRecall_test    <- mean(recall_test, na.rm = TRUE)
macroF1_test        <- mean(f1_test, na.rm = TRUE)

cat("\n--- KNN + UMAP METRIKOS (TESTAVIMO AIBĖ) ---\n")
cat("Accuracy: ", accuracy_test, "\n")
cat("Precision (per class): ", precision_test, "\n")
cat("Recall (per class): ", recall_test, "\n")
cat("F1 (per class): ", f1_test, "\n\n")
cat("Macro Precision: ", macroPrecision_test, "\n")
cat("Macro Recall: ", macroRecall_test, "\n")
cat("Macro F1: ", macroF1_test, "\n")

ggplot() +
  geom_tile(data = grid_knn,
            aes(x = UMAP1, y = UMAP2, fill = pred),
            alpha = 0.35) +
  scale_fill_manual(values = tile_colors,
                    name = "Prognozuota klasė",
                    labels = c("Klasė 0", "Klasė 2")) +
  geom_point(
    data = test_knn_umap %>% mutate(correct_flag = ifelse(correct, "Teisingai", "Klaidingai")),
    aes(x = UMAP1, y = UMAP2, color = as.factor(label), shape = correct_flag),
    size = 2.5, stroke = 1.5, alpha = 0.95
  ) +
  scale_shape_manual(
    name = "Taško tipas",
    values = c("Teisingai" = 19, "Klaidingai" = 4),
    labels = c("Klaidingai klasifikuota", "Teisingai klasifikuota")
  ) +
  scale_color_manual(
    name = "Tikroji klasė",
    values = point_colors,
    labels = c("Klasė 0", "Klasė 2"),
    guide = guide_legend(override.aes = list(shape = 19, size = 4))
  ) +
  labs(
    title = "KNN klasifikacija (UMAP)",
    subtitle = "Testavimo aibė.",
    x = "UMAP1",
    y = "UMAP2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# ==========================================================
#  ROC KREIVĖ: TEST AIBĖ — KNN (3 pož) VS KNN+UMAP (2D)
# ==========================================================

library(pROC)

knn_probs_test <- knn_prob(
  train_data = train_sel[, -label_idx],
  test_data  = test_sel[, -label_idx],
  train_labels = train_sel[, label_idx],
  k = k_final
)

true_labels_test <- ifelse(test_sel$label == 2, 1, 0)

roc_knn_test <- roc(true_labels_test, knn_probs_test)
auc_knn_test <- auc(roc_knn_test)

test_probs_umap <- knn_prob_umap(
  train_points = train_knn_umap[, c("UMAP1", "UMAP2")],
  train_labels = train_knn_umap$label,
  test_points  = test_knn_umap[, c("UMAP1", "UMAP2")],
  k = k_final
)

true_labels_test_umap <- ifelse(test_knn_umap$label == 2, 1, 0)

roc_knn_test_umap <- roc(true_labels_test_umap, test_probs_umap)
auc_knn_test_umap <- auc(roc_knn_test_umap)

plot(
  roc_knn_test,
  col = "red",
  lwd = 3,
  main = "KNN. Testavimo aibė. ROC kreivės: 3 požymiai ir UMAP 2D"
)
grid()

lines(roc_knn_test_umap, col = "blue", lwd = 3)

legend(
  "bottomright",
  legend = c(
    paste0("KNN (3 požymiai), AUC = ", round(auc_knn_test, 4)),
    paste0("KNN (UMAP 2D), AUC = ", round(auc_knn_test_umap, 4))
  ),
  col = c("red", "blue"),
  lwd = 3,
  bty = "n"
)

# ==========================================================
#     OUTLIERS (Mild ir Extreme) + matricos
# ==========================================================

library(dplyr)

get_outliers <- function(df, features) {
  
  mild_idx <- c()
  extreme_idx <- c()
  
  for (f in features) {
    Q1 <- quantile(df[[f]], 0.25)
    Q3 <- quantile(df[[f]], 0.75)
    IQRv <- IQR(df[[f]])
    
    lower_mild  <- Q1 - 1.5 * IQRv
    upper_mild  <- Q3 + 1.5 * IQRv
    lower_ext   <- Q1 - 3 * IQRv
    upper_ext   <- Q3 + 3 * IQRv
    
    mild_idx    <- union(mild_idx, which(df[[f]] < lower_mild | df[[f]] > upper_mild))
    extreme_idx <- union(extreme_idx, which(df[[f]] < lower_ext  | df[[f]] > upper_ext))
  }
  
  list(
    mild = df[mild_idx, ],
    extreme = df[extreme_idx, ]
  )
}

feat3 <- c("RR_r_0.RR_r_1", "wl_side", "P_val")
val_out <- get_outliers(val_sel, feat3)
test_out <- get_outliers(test_sel, feat3)

run_knn_cm <- function(train_df, test_df, k = 5) {
  idx <- ncol(train_df)
  if (nrow(test_df) == 0) return(NULL)
  
  pred <- knn(
    train = train_df[, -idx, drop = FALSE],
    test  = test_df[, -idx, drop = FALSE],
    cl    = train_df[, idx],
    k = k
  )
  
  cm <- table(test_df[, idx], pred)
  return(cm)
}


cat("\n=== VALIDATION: Mild Outliers (3 features) ===\n")
cm_val_mild <- run_knn_cm(train_sel, val_out$mild, k_final)
print(cm_val_mild)

cat("\n=== VALIDATION: Extreme Outliers (3 features) ===\n")
cm_val_ext <- run_knn_cm(train_sel, val_out$extreme, k_final)
print(cm_val_ext)

cat("\n=== TEST: Mild Outliers (3 features) ===\n")
cm_test_mild <- run_knn_cm(train_sel, test_out$mild, k_final)
print(cm_test_mild)

cat("\n=== TEST: Extreme Outliers (3 features) ===\n")
cm_test_ext <- run_knn_cm(train_sel, test_out$extreme, k_final)
print(cm_test_ext)

# ================================
# FUNKCIJA OUTLIERIŲ STATISTIKAI
# ================================
count_outlier_stats <- function(df, out_list) {
  n_total <- nrow(df)
  n_mild <- nrow(out_list$mild)
  n_extreme <- nrow(out_list$extreme)
  
  # normalūs taškai = visi – mild – extreme
  n_normal <- n_total - n_mild  # nes mild jau apima extreme
  
  data.frame(
    Total = n_total,
    Normal = n_normal,
    Mild = n_mild,
    Extreme = n_extreme
  )
}

cat("\n===== OUTLIER STATISTIKA — VALIDATION (3 features) =====\n")
val_stat <- count_outlier_stats(val_sel, val_out)
print(val_stat)

cat("\n===== OUTLIER STATISTIKA — TEST (3 features) =====\n")
test_stat <- count_outlier_stats(test_sel, test_out)
print(test_stat)

# ==========================================================
#  HOLD-OUT (10 iteracijų) — KNN + UMAP
# ==========================================================

library(class)
library(umap)

set.seed(999)

holdout_umap <- data.frame()

k_final <- 5
selected_feats <- c("RR_r_0.RR_r_1", "wl_side", "P_val")

for (i in 1:10) {
  cat("\n===== ITERACIJA", i, "=====\n")
  set.seed(100 + i)
  
  split1 <- sample.split(dataset$label, SplitRatio = 0.8)
  train_block <- subset(dataset, split1 == TRUE)
  test_block  <- subset(dataset, split1 == FALSE)
  
  split2 <- sample.split(train_block$label, SplitRatio = 0.8)
  training_set <- subset(train_block, split2 == TRUE)
  validation_set <- subset(train_block, split2 == FALSE)
  
  training_set[, -ncol(training_set)] <- scale(training_set[, -ncol(training_set)])
  validation_set[, -ncol(validation_set)] <- scale(validation_set[, -ncol(validation_set)])
  
  train_sel <- training_set[, c(selected_feats, "label")]
  val_sel   <- validation_set[, c(selected_feats, "label")]
  
  umap_model_iter <- umap(
    train_sel %>% select(-label),
    n_neighbors = 200,
    min_dist = 0.5,
    n_components = 2,
    metric = "euclidean"
  )
  
  umap_train <- as.data.frame(umap_model_iter$layout)
  colnames(umap_train) <- c("UMAP1", "UMAP2")
  umap_train$label <- train_sel$label
  
  umap_val <- as.data.frame(predict(umap_model_iter, val_sel %>% select(-label)))
  colnames(umap_val) <- c("UMAP1", "UMAP2")
  umap_val$label <- val_sel$label

  pred <- knn(
    train = umap_train[, c("UMAP1","UMAP2")],
    test  = umap_val[, c("UMAP1","UMAP2")],
    cl    = umap_train$label,
    k = k_final
  )
  
  cm <- table(umap_val$label, pred)
  print(cm)
  
  diag_vals <- diag(cm)
  rowsums <- rowSums(cm)
  colsums <- colSums(cm)
  
  accuracy <- sum(diag_vals) / sum(cm)
  precision <- diag_vals / colsums
  recall    <- diag_vals / rowsums
  f1        <- 2 * precision * recall / (precision + recall)
  
  holdout_umap <- rbind(
    holdout_umap,
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
#  REZULTATAI
# ==========================================================

cat("\n===========================\n")
cat("   KNN + UMAP HOLD-OUT (10 kartų) REZULTATAI")
cat("\n===========================\n")

print(holdout_umap)

cat("\nVidurkiai:\n")
print(colMeans(holdout_umap[, -1]))

cat("\nDispersijos:\n")
print(apply(holdout_umap[, -1], 2, var))

# ==========================================================
#  KLASIFIKAVIMO KLAIDA
# ==========================================================

holdout_umap$error <- 1 - holdout_umap$accuracy
mean_error_umap <- mean(holdout_umap$error)
sd_error_umap <- sd(holdout_umap$error)

cat("\nVidutinė klasifikavimo klaida:", round(mean_error_umap, 6), "\n")
cat("Klaidos SD:", round(sd_error_umap, 6), "\n")

# ==========================================================
#  BOXPLOT
# ==========================================================

par(mfrow = c(2, 2))
boxplot(holdout_umap$accuracy, main = "UMAP Accuracy")
boxplot(holdout_umap$macroPrecision, main = "UMAP Precision")
boxplot(holdout_umap$macroRecall, main = "UMAP Recall")
boxplot(holdout_umap$macroF1, main = "UMAP F1")
par(mfrow = c(1, 1))
