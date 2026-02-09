# ===========================================
#     DUOMENŲ NUSKAITYMAS IR PARUOŠIMAS
# ===========================================
library(dplyr)
library(readr)
library(cluster)
library(ggnewscale)
library(umap)
library(dbscan)
library(ggplot2)
library(tidyr)

# 1. Duomenų nuskaitymas
duomenys <- read_delim("EKG_pupsniu_analize.csv", delim = ";", show_col_types = FALSE)

# 2. Atrenkame po 500 įrašų iš kiekvienos klasės
set.seed(42)
duomenys_balansuoti <- duomenys %>%
  filter(label %in% c(0, 1, 2)) %>%
  group_by(label) %>%
  slice_sample(n = 500) %>%
  ungroup()

# 3. Išskirčių funkcija pagal IQR
outlier_class <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  mild_low <- q1 - 1.5 * iqr; mild_high <- q3 + 1.5 * iqr
  extreme_low <- q1 - 3 * iqr; extreme_high <- q3 + 3 * iqr
  ifelse(x < extreme_low | x > extreme_high, "Extreme",
         ifelse(x < mild_low | x > mild_high, "Mild", "Normal"))
}

# 4. Atrinkti 7 požymiai (kaip II užduotyje)
pozymiai7 <- c("RR_l_0", "RR_l_0/RR_l_1", "P_val", "Q_val", "R_val", "S_val", "signal_std")

# ===========================================
#     RINKINYS NR.1 – 7 POŽYMIŲ AIBĖ
# ===========================================
# Trūkstamų reikšmių užpildymas medianomis pagal klasę
duomenys_7 <- duomenys_balansuoti %>%
  select(label, all_of(pozymiai7)) %>%
  group_by(label) %>%
  mutate(across(all_of(pozymiai7),
                ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))) %>%
  ungroup()


# Išskirtys (teisingai per column -> row)
isskirtys_7 <- sapply(duomenys_7[, pozymiai7], outlier_class)
eil_isskirtys_7 <- apply(isskirtys_7, 1, function(row) {
  if ("Extreme" %in% row) "Extreme"
  else if ("Mild" %in% row) "Mild"
  else "Normal"
})

# Pridedam prie duomenų
duomenys_7$Isskirtys <- factor(eil_isskirtys_7, levels = c("Normal", "Mild", "Extreme"))

# Normalizuojam
duomenys_7_norm <- duomenys_7 %>%
  mutate(across(all_of(pozymiai7), ~ (.x - min(.x)) / (max(.x) - min(.x))))

# ===========================================
#     RINKINYS NR.2 – PILNA POŽYMIŲ AIBĖ
# ===========================================
visi_pozymiai <- duomenys_balansuoti %>%
  group_by(label) %>%
  mutate(across(where(is.numeric),
                ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))) %>%
  ungroup()


# Išskirtys
isskirtys_all <- sapply(visi_pozymiai, outlier_class)
eil_isskirtys_all <- apply(isskirtys_all, 1, function(row) {
  if ("Extreme" %in% row) "Extreme"
  else if ("Mild" %in% row) "Mild"
  else "Normal"
})

# Pridedam išskirtis ir klases
visi_pozymiai$Isskirtys <- factor(eil_isskirtys_all, levels = c("Normal", "Mild", "Extreme"))
visi_pozymiai$label <- duomenys_balansuoti$label

# Normalizuojam tik skaitinius, išskyrus 'label' ir 'Isskirtys'
visi_pozymiai_norm <- visi_pozymiai %>%
  mutate(across(
    where(is.numeric) & !matches("label|Isskirtys"),
    ~ (.x - min(.x)) / (max(.x) - min(.x))
  ))


# ===========================================
#     kNN kreive
# ===========================================


# Funkcija automatiniam eps radimui (knee metodas, paprasta versija)
rasti_optimalu_eps <- function(duomenys, k = 14, title = "Duomenų rinkinys", plot_file = NULL) {
  # duomenys: matrica / data.frame su tik skaitiniais požymiais
  require(dbscan)
  knn_distances <- kNNdist(as.matrix(duomenys), k = k)
  dist_vec <- sort(knn_distances)
  
  # paprastas knee: didžiausias skirtumas tarp tvarkingų atstumų
  diffs <- diff(dist_vec)
  knee_point <- which.max(diffs)
  eps_opt <- dist_vec[knee_point]
  
  # Parodyti/įrašyti grafiką
  if (!is.null(plot_file)) png(plot_file, width = 900, height = 600)
  plot(dist_vec, type = "l", main = paste("kNN distancijų kreivė -", title),
       ylab = "Atstumas iki k kaimyno", xlab = "Taškai (rikiuoti)")
  abline(h = eps_opt, col = "red", lty = 2)
  points(knee_point, eps_opt, col = "red", pch = 19)
  text(knee_point, eps_opt, paste0("eps ≈ ", round(eps_opt, 3)), pos = 4, col = "red")
  if (!is.null(plot_file)) dev.off()
  
  cat("\n➡", title, ": rastas optimalus eps =", round(eps_opt, 6), "(k = ", k, ")\n")
  return(eps_opt)
}

# DBSCAN vykdymas ir įvertinimas (silhouette, triukšmo dalis)
ivertinti_dbscan <- function(duomenys, eps, minPts = 5, title = "DBSCAN rezultatai", verbose = TRUE) {
  require(dbscan); require(cluster)
  model <- dbscan(as.matrix(duomenys), eps = eps, minPts = minPts)
  
  if (verbose) {
    cat("\n=== ", title, " ===\n", sep = "")
    num_clusters <- length(unique(model$cluster[model$cluster != 0]))
    cat("Klasterių skaičius (neįskaitant triukšmo 0):", num_clusters, "\n")
    noise_prop <- round(sum(model$cluster == 0) / nrow(duomenys) * 100, 2)
    cat("Triukšmo taškų dalis:", noise_prop, "%\n")
  }
  
  # Silhouette: tik jei yra bent 2 klasteriai ir kiekviename bent 2 taškai
  clusters_nonzero <- model$cluster[model$cluster != 0]
  if (length(unique(clusters_nonzero)) > 1 && min(table(clusters_nonzero)) > 1) {
    data_clustered <- duomenys[model$cluster != 0, , drop = FALSE]
    sil <- silhouette(clusters_nonzero, dist(data_clustered))
    if (verbose) cat("Vidutinis silueto įvertis:", round(mean(sil[, 3]), 4), "\n")
  } else {
    if (verbose) cat("Silueto įvertis neskaičiuojamas (per mažai klasterių arba taškų)\n")
  }
  
  return(model)
}

# ===========================================
#   KLASTERIZAVIMAS: AUTOMATINIS EPS
# ===========================================
# Paruošiam duomenis DBSCAN: 7 požymiai ir pilna aibė
duom1 <- duomenys_7_norm %>% select(all_of(pozymiai7))
minPts1 <- ncol(duom1) 
eps1_auto <- rasti_optimalu_eps(duom1, k = minPts1, title = "Mažasis rinkinys (7 požymiai)", plot_file = "kNN_7_pozymiai.png")
rez1_auto <- ivertinti_dbscan(duom1, eps = eps1_auto, minPts = minPts1, title = "DBSCAN – 7 požymiai (Auto eps)")

duomenys_7_norm$dbscan_cluster_auto <- rez1_auto$cluster

table(rez1_auto$cluster)

numeric_cols_all <- names(visi_pozymiai_norm)[sapply(visi_pozymiai_norm, is.numeric) & !(names(visi_pozymiai_norm) %in% c("label"))]
duom2 <- visi_pozymiai_norm %>% select(all_of(numeric_cols_all))
minPts2 <- ncol(duom2) * 2
eps2_auto <- rasti_optimalu_eps(duom2, k = minPts2, title = "Pilnas rinkinys (visi požymiai)", plot_file = "kNN_all_pozymiai.png")
rez2_auto <- ivertinti_dbscan(duom2, eps = eps2_auto, minPts = minPts2, title = "DBSCAN – Pilnas rinkinys (Auto eps)")

visi_pozymiai_norm$dbscan_cluster_auto <- rez2_auto$cluster

table(rez2_auto$cluster)

set.seed(42)
# # --- UMAP: 7 požymiai ---
# umap_res_7 <- umap(
#     duomenys_7_norm[, pozymiai7],
#     n_neighbors = 200,
#     min_dist = 0.3,
#     metric = "euclidean"
# )
# umap_df_7 <- data.frame(
#     UMAP1 = umap_res_7$layout[, 1],
#     UMAP2 = umap_res_7$layout[, 2],
#     cluster = as.factor(duomenys_7_norm$dbscan_cluster_auto),
#     label = as.factor(duomenys_7_norm$label),
#     outlier = duomenys_7_norm$Isskirtys
# )
# 
# # --- UMAP: visi požymiai ---
# umap_res_all <- umap(
#     visi_pozymiai_norm[, numeric_cols_all],
#     n_neighbors = 200,
#     min_dist = 0.3,
#     metric = "euclidean"
# )
#   umap_df_all <- data.frame(
#     UMAP1 = umap_res_all$layout[, 1],
#     UMAP2 = umap_res_all$layout[, 2],
#     cluster = as.factor(visi_pozymiai_norm$dbscan_cluster_auto),
#     label = as.factor(visi_pozymiai_norm$label),
#     outlier = visi_pozymiai_norm$Isskirtys
#)
  
label_colors <- c("0" = "#A6CEE3", "1" = "#FDBF6F", "2" = "#B2DF8A")
cluster_colors <- c("#80B1D3", "#FB8072", "#B3DE69", "#FFB347", "#B19CD9",
                    "#FCCDE5", "#BC80BD", "#CCEBC5", "#FFED6F", "#999999")


# Konveksinio apvalkalo funkcija
get_hull <- function(df) df[chull(df$UMAP1, df$UMAP2), ]
    
# # ===========================================
# #   UMAP VIZUALIZACIJA 7 POZYMIU
# # ===========================================
# umap_df_7$border_color <- ifelse(
#   umap_df_7$outlier == "Normal",
#   label_colors[as.character(umap_df_7$label)],
#   ifelse(umap_df_7$outlier == "Mild", "red", "black")
# )
# 
# # Apskaičiuojam ribas
# hulls_7 <- umap_df_7 %>%
#     group_by(cluster) %>%
#     do(get_hull(.))
# 
# # Vizualizacija
# ggplot(umap_df_7, aes(UMAP1, UMAP2)) +
#     geom_polygon(data = hulls_7, aes(fill = cluster, group = cluster),
#                  color = "black", alpha = 0.25, linewidth = 0.8) +
#     scale_fill_manual(name = "Klasteriai (apvadai)", values = cluster_colors) +
#     new_scale_fill() +
#     geom_point(aes(fill = label, color = border_color),
#                shape = 21, size = 3, stroke = 1.1, alpha = 0.9) +
#     scale_fill_manual(name = "Klasės (užpildas)", values = label_colors) +
#     scale_color_identity(
#       name = "Išskirtys (rėmeliai)",
#       guide = "legend",
#       breaks = c("red", "black"),
#       labels = c("Mild – vidutinė išskirtis", "Extreme – stipri išskirtis")
#     ) +
#     labs(title = paste0("DBSCAN klasterizavimas – 7 požymiai (eps=", round(eps1_auto, 3),
#                         ", MinPts=", minPts1, ")"),
#          x = "UMAP1", y = "UMAP2") +
#     theme_minimal(base_size = 13) +
#     theme(
#       legend.position = "right",
#       plot.title = element_text(size = 15, face = "bold"),
#       panel.grid = element_line(color = "grey90")
#     )
# 
# # ===========================================
# #   UMAP VIZUALIZACIJA VISAI AIBEI
# # ===========================================
# 
# umap_df_all$border_color <- ifelse(
#   umap_df_all$outlier == "Normal",
#   label_colors[as.character(umap_df_all$label)],
#   ifelse(umap_df_all$outlier == "Mild", "red", "black")
# )
# 
# # Ribos pagal klasterius
# hulls_all <- umap_df_all %>%
#     group_by(cluster) %>%
#     do(get_hull(.))
# 
# # Vizualizacija
# ggplot(umap_df_all, aes(UMAP1, UMAP2)) +
#     geom_polygon(data = hulls_all, aes(fill = cluster, group = cluster),
#                  color = "black", alpha = 0.25, linewidth = 0.8) +
#     scale_fill_manual(name = "Klasteriai (apvadai)", values = cluster_colors) +
#     new_scale_fill() +
#     geom_point(aes(fill = label, color = border_color),
#                shape = 21, size = 3, stroke = 1.1, alpha = 0.9) +
#     scale_fill_manual(name = "Klasės (užpildas)", values = label_colors) +
#     scale_color_identity(
#       name = "Išskirtys (rėmeliai)",
#       guide = "legend",
#       breaks = c("red", "black"),
#       labels = c("Mild – vidutinė išskirtis", "Extreme – stipri išskirtis")
#     ) +
#     labs(title = paste0("DBSCAN klasterizavimas – visi požymiai (eps=", round(eps2_auto, 3),
#                         ", MinPts=", minPts2, ")"),
#          x = "UMAP1", y = "UMAP2") +
#     theme_minimal(base_size = 13) +
#     theme(
#       legend.position = "right",
#       plot.title = element_text(size = 15, face = "bold"),
#       panel.grid = element_line(color = "grey90")
#     )


# # ===========================================
# #   DBSCAN KLASTERIZAVIMAS ANT GERIAUSIOS UMAP PROJEKCIJOS
# # ===========================================
set.seed(42)
umap_best <- umap(
  duomenys_7_norm[, pozymiai7],
    n_neighbors = 200,
    min_dist = 0.5,
    metric = "euclidean" )

umap_best_df <- data.frame(
    UMAP1 = umap_best$layout[, 1],
    UMAP2 = umap_best$layout[, 2],
    label = as.factor(duomenys_7_norm$label),
    outlier = duomenys_7_norm$Isskirtys )

# --- DBSCAN ant UMAP projekcijos ---
duom_best <- umap_best_df[, c("UMAP1", "UMAP2")]

minPts_best <- 2 * ncol(duom_best)
eps_best <- rasti_optimalu_eps(
    duom_best,
    k = minPts_best,
    title = "Geriausia projekcija (dim=2)",
    plot_file = "kNN_best_umap.png"
  )

rez_best <- ivertinti_dbscan(
    duom_best,
    eps = eps_best,
    minPts = minPts_best,
    title = "DBSCAN – Geriausia UMAP projekcija"
)

table(rez_best$cluster)

# Pridedam klasterius prie UMAP rezultato
umap_best_df$cluster <- as.numeric(factor(rez_best$cluster))
umap_best_df$border_color <- ifelse(
    umap_best_df$outlier == "Normal",
    label_colors[as.character(umap_best_df$label)],
    ifelse(umap_best_df$outlier == "Mild", "red", "black")
)

# Apskaičiuojam klasterių ribas
hulls_best <- umap_best_df %>%
    group_by(factor(cluster)) %>%
    do(get_hull(.))

ggplot(umap_best_df, aes(UMAP1, UMAP2)) +
  geom_polygon(data = hulls_best, aes(fill = factor(cluster), group = cluster),
                 color = "black", alpha = 0.25, linewidth = 0.8) +
  scale_fill_manual(name = "Klasteriai (apvadai)", values = setNames(
    cluster_colors[1:length(unique(umap_best_df$cluster))],
    unique(umap_best_df$cluster)
  )
  ) +
    new_scale_fill() +
    geom_point(aes(fill = label, color = border_color),
               shape = 21, size = 3, stroke = 1.1, alpha = 0.9) +
    scale_fill_manual(name = "Klasės (užpildas)", values = label_colors) +
    scale_color_identity(
      name = "Išskirtys (rėmeliai)",
      guide = "legend",
      breaks = c("red", "black"),
      labels = c("Mild – vidutinė išskirtis", "Extreme – stipri išskirtis")
    ) +
    labs(
      title = paste0("DBSCAN klasterizavimas – geriausia UMAP projekcija (eps=", round(eps_best, 3),
                     ", MinPts=", minPts_best, ")"),
      x = "UMAP1", y = "UMAP2"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 15, face = "bold"),
      panel.grid = element_line(color = "grey90")
    )

# ===========================================-----------------------------------------------------------
#   RANKINIS EPS EKSPERIMENTAS (pavyzdys)
# ===========================================
cat("\n\n==== RANKINIS EPS EKSPERIMENTAS ====\n")

# # Rankinis 7 požymių rinkinys
# eps1_manual <- 0.4
# rez1_manual <- ivertinti_dbscan(duom1, eps = eps1_manual, minPts = minPts1,
#                                 title = paste0("DBSCAN – 7 požymiai (Rankinis eps=", eps1_manual, ")"))
# duomenys_7_norm$dbscan_cluster_manual <- rez1_manual$cluster
# 
# table(rez1_manual$cluster)
# # Rankinis pilnas rinkinys
# eps2_manual <- 0.9
# rez2_manual <- ivertinti_dbscan(duom2, eps = eps2_manual, minPts = minPts2,
#                                 title = paste0("DBSCAN – Pilnas rinkinys (Rankinis eps=", eps2_manual, ")"))
# visi_pozymiai_norm$dbscan_cluster_manual <- rez2_manual$cluster
#   
# table(rez2_manual$cluster)
# 
# # --- UMAP 7  pozymiai
# umap_df_7_manual <- data.frame(
#   UMAP1 = umap_res_7$layout[, 1],
#   UMAP2 = umap_res_7$layout[, 2],
#   cluster = as.factor(duomenys_7_norm$dbscan_cluster_manual),
#   label = as.factor(duomenys_7_norm$label),
#   outlier = duomenys_7_norm$Isskirtys
# )
# 
# umap_df_7_manual$border_color <- ifelse(
#   umap_df_7_manual$outlier == "Normal",
#   label_colors[as.character(umap_df_7_manual$label)],
#   ifelse(umap_df_7_manual$outlier == "Mild", "red", "black")
# )
# 
# # Apskaičiuojam ribas
# hulls_7_manual <- umap_df_7_manual %>%
#   group_by(cluster) %>%
#   do(get_hull(.))
# 
# # Vizualizacija
# ggplot(umap_df_7_manual, aes(UMAP1, UMAP2)) +
#   geom_polygon(data = hulls_7_manual, aes(fill = cluster, group = cluster),
#                color = "black", alpha = 0.25, linewidth = 0.8) +
#   scale_fill_manual(name = "Klasteriai (apvadai)", values = cluster_colors) +
#   new_scale_fill() +
#   geom_point(aes(fill = label, color = border_color),
#              shape = 21, size = 3, stroke = 1.1, alpha = 0.9) +
#   scale_fill_manual(name = "Klasės (užpildas)", values = label_colors) +
#   scale_color_identity(
#     name = "Išskirtys (rėmeliai)",
#     guide = "legend",
#     breaks = c("red", "black"),
#     labels = c("Mild – vidutinė išskirtis", "Extreme – stipri išskirtis")
#   ) +
#   labs(title = paste0("DBSCAN klasterizavimas – 7 požymiai (eps=", round(eps1_manual, 3),
#                       ", MinPts=", minPts1, ")"),
#        x = "UMAP1", y = "UMAP2") +
#   theme_minimal(base_size = 13) +
#   theme(
#     legend.position = "right",
#     plot.title = element_text(size = 15, face = "bold"),
#     panel.grid = element_line(color = "grey90")
#   )
# 
# # --- UMAP: visi požymiai ---
# umap_df_all_manual <- data.frame(
#   UMAP1 = umap_res_all$layout[, 1],
#   UMAP2 = umap_res_all$layout[, 2],
#   cluster = as.factor(visi_pozymiai_norm$dbscan_cluster_manual),
#   label = as.factor(visi_pozymiai_norm$label),
#   outlier = visi_pozymiai_norm$Isskirtys
# )
# 
# umap_df_all_manual$border_color <- ifelse(
#   umap_df_all_manual$outlier == "Normal",
#   label_colors[as.character(umap_df_all_manual$label)],
#   ifelse(umap_df_all_manual$outlier == "Mild", "red", "black")
# )
# 
# # # Ribos pagal klasterius
# hulls_all_manual <- umap_df_all_manual %>%
#   group_by(cluster) %>%
#   do(get_hull(.))
# 
# # Vizualizacija
# ggplot(umap_df_all_manual, aes(UMAP1, UMAP2)) +
#   geom_polygon(data = hulls_all_manual, aes(fill = cluster, group = cluster),
#                color = "black", alpha = 0.25, linewidth = 0.8) +
#   scale_fill_manual(name = "Klasteriai (apvadai)", values = cluster_colors) +
#   new_scale_fill() +
#   geom_point(aes(fill = label, color = border_color),
#              shape = 21, size = 3, stroke = 1.1, alpha = 0.9) +
#   scale_fill_manual(name = "Klasės (užpildas)", values = label_colors) +
#   scale_color_identity(
#     name = "Išskirtys (rėmeliai)",
#     guide = "legend",
#     breaks = c("red", "black"),
#     labels = c("Mild – vidutinė išskirtis", "Extreme – stipri išskirtis")
#   ) +
#   labs(title = paste0("DBSCAN klasterizavimas – visi požymiai (eps=", round(eps2_manual, 3),
#                       ", MinPts=", minPts2, ")"),
#        x = "UMAP1", y = "UMAP2") +
#   theme_minimal(base_size = 13) +
#   theme(
#     legend.position = "right",
#     plot.title = element_text(size = 15, face = "bold"),
#     panel.grid = element_line(color = "grey90")
#   )
# 
# # --- UMAP geriausia projekcija
# eps3_manual <- 0.7
# rez_best_manual <- ivertinti_dbscan(
#   duom_best,
#   eps = eps3_manual,
#   minPts = minPts_best,
#   title = "DBSCAN – Geriausia UMAP projekcija"
# )
# 
# table(rez_best_manual$cluster)
#  
# # Pridedam klasterius prie UMAP rezultato
# umap_best_df$cluster <- as.factor(rez_best_manual$cluster)
# umap_best_df$border_color <- ifelse(
#   umap_best_df$outlier == "Normal",
#   label_colors[as.character(umap_best_df$label)],
#   ifelse(umap_best_df$outlier == "Mild", "red", "black")
# )
# 
# # Apskaičiuojam klasterių ribas
# hulls_best_manual <- umap_best_df %>%
#   group_by(factor(cluster)) %>%
#   do(get_hull(.))
# 
# ggplot(umap_best_df, aes(UMAP1, UMAP2)) +
#   geom_polygon(data = hulls_best_manual, aes(fill = factor(cluster), group = cluster),
#                color = "black", alpha = 0.25, linewidth = 0.8) +
#   scale_fill_manual(name = "Klasteriai (apvadai)", values = setNames(
#     cluster_colors[1:length(unique(umap_best_df$cluster))],
#     unique(umap_best_df$cluster)
#   )
#   ) +
#   new_scale_fill() +
#   geom_point(aes(fill = label, color = border_color),
#              shape = 21, size = 3, stroke = 1.1, alpha = 0.9) +
#   scale_fill_manual(name = "Klasės (užpildas)", values = label_colors) +
#   scale_color_identity(
#     name = "Išskirtys (rėmeliai)",
#     guide = "legend",
#     breaks = c("red", "black"),
#     labels = c("Mild – vidutinė išskirtis", "Extreme – stipri išskirtis")
#   ) +
#   labs(
#     title = paste0("DBSCAN klasterizavimas – geriausia UMAP projekcija (eps=", round(eps3_manual, 3),
#                    ", MinPts=", minPts_best, ")"),
#     x = "UMAP1", y = "UMAP2"
#   ) +
#   theme_minimal(base_size = 13) +
#   theme(
#     legend.position = "right",
#     plot.title = element_text(size = 15, face = "bold"),
#     panel.grid = element_line(color = "grey90")
#   )

# ===========================================
#   KLASTERIŲ KOKYBĖS ĮVERTINIMAS (ARI, NMI)
# ===========================================
library(aricode)   # NMI
library(mclust)    # ARI

cat("\n\n==== KLASTERIŲ KOKYBĖS ĮVERTINIMAS ====\n")

# # --- 1. 7 požymių rinkinys ---
# ari_7 <- adjustedRandIndex(duomenys_7_norm$label, duomenys_7_norm$dbscan_cluster_auto)
# nmi_7 <- NMI(duomenys_7_norm$label, duomenys_7_norm$dbscan_cluster_auto)
# 
# cat("\n7 požymių rinkinys (auto eps):", eps1_auto, "\n")
# cat("ARI =", round(ari_7, 4), " | NMI =", round(nmi_7, 4), "\n")
# 
# # --- 2. Pilnas požymių rinkinys ---
# ari_all <- adjustedRandIndex(visi_pozymiai_norm$label, visi_pozymiai_norm$dbscan_cluster_auto)
# nmi_all <- NMI(visi_pozymiai_norm$label, visi_pozymiai_norm$dbscan_cluster_auto)
# 
# cat("\nPilnas požymių rinkinys (auto eps):", eps2_auto, "\n")
# cat("ARI =", round(ari_all, 4), " | NMI =", round(nmi_all, 4), "\n")

# --- 3. Geriausia UMAP projekcija ---
ari_best <- adjustedRandIndex(umap_best_df$label, rez_best$cluster)
nmi_best <- NMI(umap_best_df$label, rez_best$cluster)

cat("\nGeriausia UMAP projekcija:", eps_best, "\n")
cat("ARI =", round(ari_best, 4), " | NMI =", round(nmi_best, 4), "\n")
# 
# # --- 4. Rankinis eps palyginimui ---
# ari_7_man <- adjustedRandIndex(duomenys_7_norm$label, duomenys_7_norm$dbscan_cluster_manual)
# nmi_7_man <- NMI(duomenys_7_norm$label, duomenys_7_norm$dbscan_cluster_manual)
# 
# ari_all_man <- adjustedRandIndex(visi_pozymiai_norm$label, visi_pozymiai_norm$dbscan_cluster_manual)
# nmi_all_man <- NMI(visi_pozymiai_norm$label, visi_pozymiai_norm$dbscan_cluster_manual)
# 
# cat("\n7 požymiai (rankinis eps):", eps1_manual, " ARI =", round(ari_7_man, 4), " | NMI =", round(nmi_7_man, 4), "\n")
# cat("Pilnas rinkinys (rankinis eps):", eps2_manual," ARI =", round(ari_all_man, 4), " | NMI =", round(nmi_all_man, 4), "\n")
# 
# # --- 5. Geriausia projekcija (rankinis eps) ---
# ari_best_man <- adjustedRandIndex(umap_best_df$label, rez_best_manual$cluster)
# nmi_best_man <- NMI(umap_best_df$label, rez_best_manual$cluster)
# 
# cat("Geriausia UMAP projekcija (rankinis eps):", eps3_manual, " ARI =", round(ari_best_man, 4), " | NMI =", round(nmi_best_man, 4), "\n")
# 

# ===========================================
#   KLASTERIŲ STATISTIKOS FUNKCIJA
# ===========================================
klasteriu_statistika <- function(df, cluster_col = "cluster") {
  df %>%
    group_by(!!sym(cluster_col)) %>%
    summarise(
      `Iš viso įrašų` = n(),
      `0 klasės įrašų` = sum(label == 0, na.rm = TRUE),
      `1 klasės įrašų` = sum(label == 1, na.rm = TRUE),
      `2 klasės įrašų` = sum(label == 2, na.rm = TRUE),
      `Įrašų be išskirčių` = sum(Isskirtys == "Normal", na.rm = TRUE),
      `Mild išskirčių` = sum(Isskirtys == "Mild", na.rm = TRUE),
      `Extreme išskirčių` = sum(Isskirtys == "Extreme", na.rm = TRUE)
    ) %>%
    arrange(!!sym(cluster_col))
}


cat("\n\n==== KLASTERIŲ SUDĖTIES STATISTIKA ====\n")

# # --- 1. 7 požymių rinkinys ---
# cat("\n7 požymių rinkinys (auto eps):\n")
# print(klasteriu_statistika(
#   data.frame(
#     cluster = duomenys_7_norm$dbscan_cluster_auto,
#     label = duomenys_7_norm$label,
#     Isskirtys = duomenys_7_norm$Isskirtys
#   )
# ))
# 
# cat("\nPilnas rinkinys (auto eps):\n")
# print(klasteriu_statistika(
#   data.frame(
#     cluster = visi_pozymiai_norm$dbscan_cluster_auto,
#     label = visi_pozymiai_norm$label,
#     Isskirtys = visi_pozymiai_norm$Isskirtys
#   )
# ))

# --- 3. Geriausia UMAP projekcija ---
umap_best_df$cluster <- rez_best$cluster
cat("\nGeriausia UMAP projekcija:\n")
print(klasteriu_statistika(
  data.frame(
    cluster = rez_best$cluster,
    label = umap_best_df$label,
    Isskirtys = umap_best_df$outlier
  ),
  "cluster"
))



# # --- 4. Rankinis eps (pvz. 7 požymiai) ---
# cat("\n7 požymių rinkinys (rankinis eps)::", eps1_manual, "\n")
# print(klasteriu_statistika(
#   data.frame(
#     cluster = duomenys_7_norm$dbscan_cluster_manual,
#     label = duomenys_7_norm$label,
#     Isskirtys = duomenys_7_norm$Isskirtys
#   )
# ))
# 
# cat("\nPilnas požymių rinkinys (rankinis eps)::", eps2_manual, "\n")
# print(klasteriu_statistika(
#   data.frame(
#     cluster = visi_pozymiai_norm$dbscan_cluster_manual,
#     label = visi_pozymiai_norm$label,
#     Isskirtys = visi_pozymiai_norm$Isskirtys
#   )
# ))
# 
# cat("\nGeriausia UMAP projekcija (rankinis eps)::", eps3_manual, "\n")
# print(klasteriu_statistika(
#   data.frame(
#     cluster = rez_best_manual$cluster,
#     label = umap_best_df$label,
#     Isskirtys = umap_best_df$outlier
#   ),
#   "cluster"
# ))

# =========================================================================
# ORIGINALIŲ 7 POŽYMIŲ ANALIZĖ PAGAL UMAP KLASTERIUS 
# =========================================================================

cat("\n\n==== DBSCAN KLASTERIŲ PROFILIS (Pagal 7 Normalizuotus Požymius) ====\n")

# Sujungiame normalizuotus duomenis su DBSCAN klasterių ID
umap_clusters_joined <- duomenys_7_norm %>%
  # Pasirenkame tik 7 požymius (kad netempti kitų stulpelių)
  select(all_of(pozymiai7))

# Pridedame klasterių ID iš geriausio DBSCAN rezultato
umap_clusters_joined$cluster <- rez_best$cluster

# Sukuriame santraukos lentelę (Min, Mean, Max)
feature_summary <- umap_clusters_joined %>%
  filter(cluster != 0) %>% # Išmetame triukšmą (klasteris 0)
  group_by(cluster) %>%
  summarise(across(all_of(pozymiai7), list(
    min = ~min(.x, na.rm = TRUE),
    mean = ~mean(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE)
  ), .names = "{.col}_{.fn}"))

feature_summary_rounded <- feature_summary %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

print(feature_summary_rounded, width = Inf)


# ===========================================
# PROFILIO VIZUALIZACIJA (Boxplots)
# ===========================================

cat("\n\n==== VIZUALUS POŽYMIŲ REIKŠMIŲ PALYGINIMAS (Boxplots) ====\n")

# Paruošimas braižymui
plot_data <- umap_clusters_joined %>%
  filter(cluster != 0) %>% # Išmetame triukšmą
  select(cluster, all_of(pozymiai7)) %>%
  pivot_longer(-cluster, names_to = "Požymis", values_to = "Reikšmė")

# Braižome boxplot'us
ggplot(plot_data, aes(x = factor(cluster), y = Reikšmė, fill = factor(cluster))) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Požymis, scales = "free_y") + 
  theme_minimal(base_size = 12) +
  labs(
    title = "7 Požymių reikšmės pagal UMAP klasterius (Normalizuota skalė [0, 1])",
    x = "Klasteris",
    y = "Normalizuota reikšmė"
  ) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))