library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(corrplot)
library(ggrepel)
### TIRIAMOS AIBĖS PARUOŠIMAS ANALIZEI
#  Nuskaityti duomenis su ; kaip delimiter
pradinis <- read_delim("EKG_pupsniu_analize.csv", delim = ";", show_col_types = FALSE)

#  Pasirinkti tik reikalingus požymius ir klasę
pradinis_atrinkti_pozymiai <- pradinis %>%
  select(label, RR_l_0, `RR_l_0/RR_l_1`, P_val, Q_val, R_val, S_val, signal_std)

#  Atsitiktinai atrinkti po 500 įrašų iš kiekvienos klasės
set.seed(42)  # kiekviena karta ima ta pacia imti
pradinis_failas <- pradinis_atrinkti_pozymiai %>%
  filter(label %in% c(0, 1, 2)) %>%
  group_by(label) %>%
  slice_sample(n = 500) %>%
  ungroup()

#  Patikrinam rezultatą
table(pradinis_failas$label)

#  išsaugom rezultata
write_csv(pradinis_failas, "pradinis_failas_7pozymiai.csv")

## PRALEISTŲ REIKŠMIŲ ANALIZĖ
na_count <- colSums(is.na(pradinis_failas))
na_percent <- round(na_count / nrow(pradinis_failas) * 100, 2)

cat("\n--- Bendras praleistų reikšmių kiekis ir procentas ---\n")
na_summary <- data.frame(Stulpelis = names(na_count),
                         Kiekis = na_count,
                         Procentas = na_percent)
print(na_summary)

# Nustatome, kuriuose stulpeliuose yra praleistų reikšmių
cols_to_fill <- names(na_count[na_count > 0])

cat("\nStulpeliai, kuriuose yra praleistų reikšmių:\n")
print(cols_to_fill)

# Praleistos reikšmės pagal klases
if (length(cols_to_fill) > 0) {
  cat("\n--- Praleistos reikšmės pagal klases ---\n")
  na_by_class <- pradinis_failas %>%
    group_by(label) %>%
    summarise(across(all_of(cols_to_fill),
                     ~sum(is.na(.x)), .names = "NA_{.col}"))
  print(na_by_class)
}

#  UŽPILDYMAS PAGAL KLASĖS MEDIANĄ 
if (length(cols_to_fill) > 0) {
  
  cat("\n--- Užpildymas pagal klasės medianą ---\n")
  cat("\nPrieš užpildymą:\n")
  print(colSums(is.na(pradinis_failas)))
  
  for (kl in unique(pradinis_failas$label)) {
    for (col in cols_to_fill) {
      med <- median(pradinis_failas[pradinis_failas$label == kl, col][[1]], na.rm = TRUE)
      pradinis_failas[pradinis_failas$label == kl & is.na(pradinis_failas[[col]]), col] <- med
    }
  }
  
  cat("\nPo užpildymo:\n")
  print(colSums(is.na(pradinis_failas)))
  
} else {
  cat("\nDuomenyse nėra praleistų reikšmių – nieko pildyti nereikia.\n")
}

## APRASOMOJI STATISTIKA
# Pasirenkame tik kiekybinius rodiklius (visus stulpelius, išskyrus label)
kiekybiniai_rodikliai <- pradinis_failas[, !(names(pradinis_failas) %in% c("label"))]

# funkcijos statistikai skaičiuoti
skaiciavimai_statistikai <- list(
  Vidurkis = function(x) mean(x, na.rm = TRUE),
  Mediana = function(x) median(x, na.rm = TRUE),
  Dispersija = function(x) var(x, na.rm = TRUE),
  SD = function(x) sd(x, na.rm = TRUE),          
  Min = function(x) min(x, na.rm = TRUE),
  Max = function(x) max(x, na.rm = TRUE),
  Q1 = function(x) quantile(x, 0.25, na.rm = TRUE),
  Q3 = function(x) quantile(x, 0.75, na.rm = TRUE), 
  IQR = function(x) IQR(x, na.rm = TRUE)           
)

# Taikome funkcijas kiekvienam kiekybiniam stulpeliui
aprasomoji_statistika <- sapply(kiekybiniai_rodikliai, function(stulpelis) {
  sapply(skaiciavimai_statistikai, function(f) f(stulpelis))
})

# Suapvaliname rezultatus iki 2 skaičių po kablelio
aprasomoji_statistika_apvalinta <- round(aprasomoji_statistika, 2)

# Atspausdiname rezultatų lentelę
print(aprasomoji_statistika_apvalinta)

#  PAGAL KLASĘ 
for (kl in unique(pradinis_failas$label)) {
  cat("\n--- Aprašomoji statistika klasei:", kl, "---\n")
  
  # išfiltruojam tos klasės įrašus
  duomenys_kl <- pradinis_failas[pradinis_failas$label == kl, ]
  kiekybiniai_kl <- duomenys_kl[, !(names(duomenys_kl) %in% c("label"))]
  
  # skaičiuojam tas pačias statistikas
  apr_stat_kl <- sapply(kiekybiniai_kl, function(stulpelis) {
    sapply(skaiciavimai_statistikai, function(f) f(stulpelis))
  })
  
  apr_stat_kl <- round(apr_stat_kl, 2)
  print(apr_stat_kl)
}

## DUOMENŲ NORMAVIMAS 

# požymiai, kuriuos normuosim
pozymiai <- c("RR_l_0", "RR_l_0/RR_l_1", "P_val", "Q_val", "R_val", "S_val", "signal_std")

# min–Max normavimas
minmax_norm <- pradinis_failas
for (f in pozymiai) {
  xmin <- min(minmax_norm[[f]], na.rm = TRUE)
  xmax <- max(minmax_norm[[f]], na.rm = TRUE)
  minmax_norm[[f]] <- (minmax_norm[[f]] - xmin) / (xmax - xmin)
}

# Patikrinam rezultatus
cat("\n=== Pavyzdys Min–Max normalizacijos (pirmos 6 eilutės) ===\n")
print(head(minmax_norm[pozymiai]))

# Išsaugom
write_csv(minmax_norm, "ekg_minmax_norm.csv")

#  Originalūs duomenys 
orig_long <- pradinis_failas %>%
  select(all_of(pozymiai)) %>%
  pivot_longer(everything(), names_to = "pozymis", values_to = "reiksme")

#  Min–Max normuoti 
mm_long <- minmax_norm %>%
  select(all_of(pozymiai)) %>%
  pivot_longer(everything(), names_to = "pozymis", values_to = "reiksme")

#  Histogramos: Originalūs 
g_orig <- ggplot(orig_long, aes(x = reiksme)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  facet_wrap(~pozymis, scales = "free") +
  labs(title = "Originalios reikšmės", x = "Reikšmė", y = "Dažnis")

#  Histogramos: Min–Max 
g_mm <- ggplot(mm_long, aes(x = reiksme)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "black") +
  facet_wrap(~pozymis, scales = "free") +
  labs(title = "Min–max normavimas", x = "Reikšmė [0–1]", y = "Dažnis")

print(g_orig)
print(g_mm)


## OUTLIERS VISIEMS POŽYMiams 
# Funkcija vienam stulpeliui
isskirtis <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  vid_ap <- q1 - 1.5 * iqr
  vid_virs <- q3 + 1.5 * iqr
  isor_ap <- q1 - 3 * iqr
  isor_virs <- q3 + 3 * iqr
  
  ifelse(x < isor_ap | x > isor_virs, "Extreme",
         ifelse(x < vid_ap | x > vid_virs, "Mild", "Normal"))
}

# Taikome visiems kiekybiniams požymiams
outlier_matrix <- sapply(kiekybiniai_rodikliai, isskirtis)

# Nustatome eilutės išskirtį
outlier_type <- apply(outlier_matrix, 1, function(x){
  if("Extreme" %in% x) return("Extreme")
  else if("Mild" %in% x) return("Mild")
  else return("Normal")
})

# Pridedame prie duomenų
pradinis_failas$outlier <- outlier_type
minmax_norm$outlier <- outlier_type

### PILNOS DUOMENŲ AIBĖS PARUOŠIMAS ANALIZEI
cat("\n\n=== PILNOS DUOMENŲ AIBĖS APDOROJIMAS ===\n")

# Atrinkti VISUS kiekybinius požymius ir imtį
# Pakartojame imties atrinkimą su visais stulpeliais
set.seed(42)
pradinis_visa_aibe <- pradinis %>%
  filter(label %in% c(0, 1, 2)) %>%
  group_by(label) %>%
  slice_sample(n = 500) %>%
  ungroup()

# Išskiriame kiekybinių požymių sąrašą (visi stulpeliai, išskyrus label)
pozymiai_visa_aibe <- names(pradinis_visa_aibe)[names(pradinis_visa_aibe) != "label"]
cat("Pilnos duomenų aibės kiekybinių požymių skaičius:", length(pozymiai_visa_aibe), "\n")


## PRALEISTŲ REIKŠMIŲ TVARKYMAS
na_count_all <- colSums(is.na(pradinis_visa_aibe))
na_percent_all <- round(na_count_all / nrow(pradinis_visa_aibe) * 100, 2)

cat("\n--- Bendras praleistų reikšmių kiekis ir procentas ---\n")
na_summary_all <- data.frame(Stulpelis = names(na_count_all),
                         Kiekis = na_count_all,
                         Procentas = na_percent_all)
print(na_summary_all)

# Nustatome, kuriuose stulpeliuose yra praleistų reikšmių
cols_to_fill_all <- names(na_count_all[na_count_all > 0])

cat("\nStulpeliai, kuriuose yra praleistų reikšmių:\n")
print(cols_to_fill_all)

# Praleistos reikšmės pagal klases
if (length(cols_to_fill_all) > 0) {
  cat("\n--- Praleistos reikšmės pagal klases ---\n")
  na_by_class_all <- pradinis_visa_aibe %>%
    group_by(label) %>%
    summarise(across(all_of(cols_to_fill_all),
                     ~sum(is.na(.x)), .names = "NA_{.col}"))
  print(na_by_class_all)
}

#  UŽPILDYMAS PAGAL KLASĖS MEDIANĄ 
if (length(cols_to_fill_all) > 0) {
  
  cat("\n--- Užpildymas pagal klasės medianą ---\n")
  cat("\nPrieš užpildymą:\n")
  print(colSums(is.na(pradinis_visa_aibe)))
  
  for (kl in unique(pradinis_visa_aibe$label)) {
    for (col in cols_to_fill_all) {
      med <- median(pradinis_visa_aibe[pradinis_visa_aibe$label == kl, col][[1]], na.rm = TRUE)
      pradinis_visa_aibe[pradinis_visa_aibe$label == kl & is.na(pradinis_visa_aibe[[col]]), col] <- med
    }
  }
  
  cat("\nPo užpildymo:\n")
  print(colSums(is.na(pradinis_visa_aibe)))
  
} else {
  cat("\nDuomenyse nėra praleistų reikšmių – nieko pildyti nereikia.\n")
}

## IŠSKIRČIŲ NUSTATYMAS IR PRIDĖJIMAS
kiekybiniai_visa_aibe <- pradinis_visa_aibe[, pozymiai_visa_aibe]
outlier_matrix_all <- sapply(kiekybiniai_visa_aibe, isskirtis)
outlier_type_all <- apply(outlier_matrix_all, 1, function(x){
  if("Extreme" %in% x) return("Extreme")
  else if("Mild" %in% x) return("Mild")
  else return("Normal")
})
pradinis_visa_aibe$outlier <- outlier_type_all


## MIN-MAX NORMAVIMAS VISAI AIBEI
minmax_norm_all <- pradinis_visa_aibe
for (f in pozymiai_visa_aibe) {
  xmin <- min(minmax_norm_all[[f]], na.rm = TRUE)
  xmax <- max(minmax_norm_all[[f]], na.rm = TRUE)
  minmax_norm_all[[f]] <- (minmax_norm_all[[f]] - xmin) / (xmax - xmin)
}

### PCA VIZUALIZACIJA

# Funkcija vizualizacijai
vizualizuoti_pca_outliers <- function(duomenys, title="PCA"){
  pca <- prcomp(duomenys[, pozymiai], scale. = FALSE)
  pca_df <- data.frame(
    PC1 = pca$x[,1],
    PC2 = pca$x[,2],
    label = as.factor(duomenys$label),
    outlier = duomenys$outlier)
  ggplot(pca_df, aes(x=PC1, y=PC2, color=label, shape=outlier)) +
    geom_point(size=2.5, alpha=0.8) +
    scale_shape_manual(values = c(Normal=16, Mild=17,Extreme=8)) +
    labs(title = title,
         subtitle = "Dimensijos sumažintos iki dim=2",
         color = "Klasė",
         shape = "Išskirtis") +
    theme_minimal()
}

# Nenormuotų duomenų PCA su atrinktais požymiais
vizualizuoti_pca_outliers(pradinis_failas, title="Nenormuoti duomenys su visomis išskirtimis (PCA)")

# Normuotų duomenų PCA su atrinktais požymiais
vizualizuoti_pca_outliers(minmax_norm, title="Normuoti duomenys (min–max) su visomis išskirtimis (PCA)")

# Visos aibės nenormuotų duomenų PCA
vizualizuoti_pca_outliers(pradinis_visa_aibe, title="PCA - Pilna požymių aibė (nenormuoti)")

# Visos aibės normuotų duomenų PCA
vizualizuoti_pca_outliers(minmax_norm_all, title="PCA - Pilna požymių aibė (normuoti)")

### UMAP VIZUALIZACIJA
# funkcija vizualizacijai
vizualizuoti_umap_outliers <- function(duomenys, title="UMAP", n_neighbors = 15, min_dist = 0.1){
  umap_rezultatas <- umap(
    X = duomenys[, pozymiai],
    n_components = 2,
    scale = FALSE, 
    n_neighbors = n_neighbors,
    min_dist = min_dist,
    verbose = FALSE 
  )
  
  umap_df <- data.frame(
    UMAP1 = umap_rezultatas[,1],
    UMAP2 = umap_rezultatas[,2],
    label = as.factor(duomenys$label),
    outlier = duomenys$outlier)
  
  ggplot(umap_df, aes(x=UMAP1, y=UMAP2, color=label, shape=outlier)) +
    geom_point(size=2.5, alpha=0.8) +
    scale_shape_manual(values = c(Normal=16, Mild=17, Extreme=8)) + 
    labs(title = title,
         subtitle = "Dimensijos sumažintos iki dim=2",
         color = "Klasė",
         shape = "Išskirtis") +
    theme_minimal()
}

# Priverčiame R naudoti senąjį saugyklos nustatymą
options(repos = c(CRAN = "https://cran.r-project.org"))

library(uwot) 
library(Matrix)
library(irlba)
library(ggplot2)

## Pradine vizualizacija su standartiniais parametrais

# Nenormuotų duomenų UMAP su atrinktais požymiais
vizualizuoti_umap_outliers(
  pradinis_failas, 
  title="Nenormuoti duomenys su visomis išskirtimis (UMAP)",
  n_neighbors = 15, 
  min_dist = 0.1
)

# Min-max normuotų duomenų UMAP su atrinktais požymiais
vizualizuoti_umap_outliers(
  minmax_norm, 
  title="Normuoti duomenys (min–max) su visomis išskirtimis (UMAP)",
  n_neighbors = 15, 
  min_dist = 0.1
)

# Visos aibės nenormuotų duomenų UMAP
vizualizuoti_umap_outliers(pradinis_visa_aibe, 
  title="UMAP - Pilna požymių aibė (nenormuoti)",
  n_neighbors = 15, 
  min_dist = 0.1
  
)

# Visos aibės normuotų duomenų UMAP
vizualizuoti_umap_outliers(minmax_norm_all, 
  title="UMAP - Pilna požymių aibė (normuoti)",
  n_neighbors = 15, 
  min_dist = 0.1
                               
)

###  UMAP Parametrų Keitimas testai

# pabreziant globalią struktūrą
vizualizuoti_umap_outliers(
  minmax_norm,
  title="Globalios struktūros pabrėžimas po min-max normavimo (UMAP)",
  n_neighbors = 50,
  min_dist = 0.5
)

# pabreziant globalią struktūrą
vizualizuoti_umap_outliers(
  minmax_norm,
  title="Globalios struktūros pabrėžimas po min-max normavimo (UMAP)",
  n_neighbors = 200,
  min_dist = 0.5
)

# pabreziant lokalią struktūrą
vizualizuoti_umap_outliers(
  minmax_norm,
  title="Lokalios struktūros pabrėžimas po min-max normavimo (UMAP)",
  n_neighbors = 5,
  min_dist = 0.001
)



library(uwot)     
library(cluster)  
library(FNN)      
library(stats)    
library(dplyr)

# --- Pagalbinės funkcijos ---

# Trustworthiness
trustworthiness_score <- function(X_orig, X_emb, k = 15) {
  n <- nrow(X_orig)
  nn_orig <- get.knn(as.matrix(X_orig), k = n - 1)$nn.index
  nn_emb  <- get.knn(as.matrix(X_emb), k = n - 1)$nn.index
  
  rank_orig <- matrix(0, n, n)
  rank_emb  <- matrix(0, n, n)
  for (i in 1:n) {
    rank_orig[i, nn_orig[i,]] <- 1:(n-1)
    rank_emb[i, nn_emb[i,]]   <- 1:(n-1)
  }
  
  tw_sum <- 0
  for (i in 1:n) {
    Uk <- which(rank_emb[i, ] <= k & rank_orig[i, ] > k)
    if (length(Uk) > 0) {
      tw_sum <- tw_sum + sum(rank_orig[i, Uk] - k)
    }
  }
  
  denom <- n * k * (2 * n - 3 * k - 1)
  T <- 1 - (2 * tw_sum) / denom
  return(T)
}

# Continuity
continuity_score <- function(X_orig, X_emb, k = 15) {
  n <- nrow(X_orig)
  nn_orig <- get.knn(as.matrix(X_orig), k = n - 1)$nn.index
  nn_emb  <- get.knn(as.matrix(X_emb), k = n - 1)$nn.index
  
  rank_orig <- matrix(0, n, n)
  rank_emb  <- matrix(0, n, n)
  for (i in 1:n) {
    rank_orig[i, nn_orig[i,]] <- 1:(n-1)
    rank_emb[i, nn_emb[i,]]   <- 1:(n-1)
  }
  
  cont_sum <- 0
  for (i in 1:n) {
    Vk <- which(rank_orig[i, ] <= k & rank_emb[i, ] > k)
    if (length(Vk) > 0) {
      cont_sum <- cont_sum + sum(rank_emb[i, Vk] - k)
    }
  }
  
  denom <- n * k * (2 * n - 3 * k - 1)
  C <- 1 - (2 * cont_sum) / denom
  return(C)
}

# --- Pagrindinė funkcija ---

ivertinti_umap_kokybe <- function(duomenys, pozymiai, label_col = NULL,
                                  n_neighbors = 15, min_dist = 0.1, k_eval = 15) {
  set.seed(42)
  
  # UMAP projekcija
  umap_res <- umap(
    X = duomenys[, pozymiai],
    n_neighbors = n_neighbors,
    min_dist = min_dist,
    n_components = 2,
    verbose = FALSE
  )
  
  # Trustworthiness ir Continuity
  T <- trustworthiness_score(duomenys[, pozymiai], umap_res, k = k_eval)
  C <- continuity_score(duomenys[, pozymiai], umap_res, k = k_eval)
  
  # Silhouette Score (jei turime labels)
  S <- NA
  if (!is.null(label_col) && label_col %in% colnames(duomenys)) {
    labels <- as.factor(duomenys[[label_col]])
    dist_mat <- dist(umap_res)
    sil <- silhouette(as.numeric(labels), dist_mat)
    S <- mean(sil[, 3])
  }
  
  # Rezultatų išvedimas
  cat("UMAP (n_neighbors =", n_neighbors, ", min_dist =", min_dist, ")\n")
  cat("   Trustworthiness:", round(T, 4), 
      "| Continuity:", round(C, 4),
      "| Silhouette:", ifelse(is.na(S), "N/A", round(S, 4)), "\n\n")
  
  # Grąžiname visus rodiklius
  invisible(list(
    embedding = umap_res,
    trustworthiness = T,
    continuity = C,
    silhouette = S
  ))
}

# --- tikrinam
ivertinti_umap_kokybe(minmax_norm, pozymiai, label_col = "label", n_neighbors = 15, min_dist = 0.1)
ivertinti_umap_kokybe(minmax_norm, pozymiai, label_col = "label", n_neighbors = 5, min_dist = 0.001)
ivertinti_umap_kokybe(minmax_norm, pozymiai, label_col = "label", n_neighbors = 50, min_dist = 0.5)
ivertinti_umap_kokybe(minmax_norm, pozymiai, label_col = "label", n_neighbors = 200, min_dist = 0.5)
ivertinti_umap_kokybe(minmax_norm, pozymiai, label_col = "label", n_neighbors = 200, min_dist = 0.3)

