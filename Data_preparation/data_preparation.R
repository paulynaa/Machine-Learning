library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(corrplot)

# 1. Nuskaityti duomenis su ; kaip delimiter
pradinis <- read_delim("EKG_pupsniu_analize.csv", delim = ";", show_col_types = FALSE)

# 2. Pasirinkti tik reikalingus požymius + klasę
pradinis_atrinkti_pozymiai <- pradinis %>%
  select(label, RR_l_0, `RR_l_0/RR_l_1`, P_val, R_pos, S_pos, signal_std)

# 3. Atsitiktinai atrinkti po 500 įrašų iš kiekvienos klasės (0, 1, 2)
set.seed(42)  # kad būtų pakartojama
pradinis_failas <- pradinis_atrinkti_pozymiai %>%
  filter(label %in% c(0, 1, 2)) %>%
  group_by(label) %>%
  slice_sample(n = 500) %>%
  ungroup()

# 4. Patikrinti rezultatą
table(pradinis_failas$label)

# 5. išsaugoti rezultata
write_csv(pradinis_failas, "pradinis_failas_6pozymiai.csv")

#aprasomoji statistika
# Pasirenkame tik kiekybinius rodiklius (visus stulpelius, išskyrus 'label')
kiekybiniai_rodikliai <- pradinis_failas[, !(names(pradinis_failas) %in% c("label"))]

# Sukuriame sąrašą funkcijų statistikai skaičiuoti
skaiciavimai_statistikai <- list(
  Vidurkis = function(x) mean(x, na.rm = TRUE),
  Mediana = function(x) median(x, na.rm = TRUE),
  Dispersija = function(x) var(x, na.rm = TRUE),
  SD = function(x) sd(x, na.rm = TRUE),          # Standartinis nuokrypis
  Min = function(x) min(x, na.rm = TRUE),
  Max = function(x) max(x, na.rm = TRUE),
  Q1 = function(x) quantile(x, 0.25, na.rm = TRUE), # Pirmas kvartilis
  Q3 = function(x) quantile(x, 0.75, na.rm = TRUE), # Trečias kvartilis
  IQR = function(x) IQR(x, na.rm = TRUE)           # Tarpkvartilinis plotis
)

# Taikome funkcijas kiekvienam kiekybiniam stulpeliui
# sapply bandys supaprastinti rezultatą į matricą
aprasomoji_statistika <- sapply(kiekybiniai_rodikliai, function(stulpelis) {
  sapply(skaiciavimai_statistikai, function(f) f(stulpelis))
})

# Suapvaliname rezultatus iki 2 skaičių po kablelio (arba kiek reikia)
aprasomoji_statistika_apvalinta <- round(aprasomoji_statistika, 2)

# Atspausdiname rezultatų lentelę
print(aprasomoji_statistika_apvalinta)

# --- PAGAL KLASĘ ---
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
# --- PRALEISTŲ REIKŠMIŲ ANALIZĖ ---

# bendras praleistų reikšmių skaičius ir procentai
na_count <- colSums(is.na(pradinis_failas))
na_percent <- round(na_count / nrow(pradinis_failas) * 100, 2)

cat("\n--- Bendras praleistų reikšmių kiekis ir procentas ---\n")
print(data.frame(Stulpelis = names(na_count),
                 Kiekis = na_count,
                 Procentas = na_percent))

# praleistos reikšmės pagal klases
cat("\n--- Praleistos reikšmės pagal klases ---\n")
na_by_class <- pradinis_failas %>%
  group_by(label) %>%
  summarise(across(c("S_pos", "signal_std"),
                   ~sum(is.na(.x)), .names = "NA_{.col}"))

print(na_by_class)

# --- UŽPILDYMAS PAGAL KLASĖS MEDIANĄ ---
cols_to_fill <- c("S_pos", "signal_std")

# prieš užpildymą
cat("\n--- Prieš užpildymą ---\n")
print(colSums(is.na(pradinis_failas)))

# ciklas per kiekvieną klasę ir stulpelį
for (kl in unique(pradinis_failas$label)) {
  for (col in cols_to_fill) {
    med <- median(pradinis_failas[pradinis_failas$label == kl, col][[1]], na.rm = TRUE)
    pradinis_failas[pradinis_failas$label == kl & is.na(pradinis_failas[[col]]), col] <- med
  }
}

# po užpildymo
cat("\n--- Po užpildymo ---\n")
print(colSums(is.na(pradinis_failas)))


# #  DUOMENŲ AIBĖS vertinimas ---
# # Įrašų ir požymių skaičius
# m <- nrow(kiekybiniai_rodikliai)
# n <- ncol(kiekybiniai_rodikliai)
# 
# # # --- Retumo skaičiavimas 
# # u <- sum(duomenys != 0, na.rm = TRUE)
# # gamma_n <- 1 - (u / (n * m))
# # retumas <- if (gamma_n <= 0.2) "tanki" else if (gamma_n <= 0.8) "vidutinė" else "reta"
# # 
# # # --- Vidinės dimensijos skaičiavimas  
# # pca <- prcomp(duomenys, scale. = TRUE)
# # dispersijos_santykis <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
# # dimensijos95 <- which(dispersijos_santykis >= 0.95)[1]
# # rho_n <- dimensijos95 / n
# # rho_klase <- if (rho_n <= 0.1) "maža" else if (rho_n <= 0.5) "vidutinė" else "didelė"
# # 
# # # --- Rezultatai ---
# # cat("\n=== Duomenų aibės vertinimas ===\n")
# # cat("Vidinė dimensija (ρₙ):", round(rho_n, 3), "-", rho_klase, "\n")
# # cat("Retumo (sparsity) rodiklis (γₙ):", round(gamma_n, 3), "-", retumas, "\n")

# --- ATSISKYRĖLIŲ (OUTLIERS) ANALIZĖ ---
kiekybiniai <- c("RR_l_0", "RR_l_0/RR_l_1", "P_val", "R_pos", "S_pos", "signal_std")

# Funkcija vienam stulpeliui
isskirtis <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  vidinis_barjeras_apacia <- q1 - 1.5 * iqr
  vidinis_barjeras_virsus <- q3 + 1.5 * iqr
  isorinis_barjeras_apacia <- q1 - 3 * iqr
  isorinis_barjeras_virsus <- q3 + 3 * iqr
  
  ifelse(x < isorinis_barjeras_apacia | x > isorinis_barjeras_virsus, "Extreme",
         ifelse(x < vidinis_barjeras_apacia | x > vidinis_barjeras_virsus, "Mild", "Normal"))
}

# Taikome visiems požymiams
for (v in kiekybiniai) {
  pradinis_failas[[paste0(v, "_isskirtis")]] <- isskirtis(pradinis_failas[[v]])
}

# --- Skaičiuojame kiek yra išskirčių ---
for (v in kiekybiniai) {
  cat("\nPožymis:", v, "\n")
  print(table(pradinis_failas[[paste0(v, "_isskirtis")]]))
}

# --- Pašaliname tik ekstremalias išskirtis ---
be_isskirciu <- pradinis_failas
for (v in kiekybiniai) {
  be_isskirciu <- be_isskirciu %>%
    filter(.data[[paste0(v, "_isskirtis")]] != "Extreme")
}

cat("\nPradinė imtis:", nrow(pradinis_failas), "įrašų\n")
cat("Be ekstremalių išskirčių:", nrow(be_isskirciu), "įrašai\n")


# --- Aprašomoji statistika PO valymo, pagal klases ---
cat("\n=== Aprašomoji statistika PO outlier pašalinimo (pagal klases) ===\n")
for (kl in unique(be_isskirciu$label)) {
  cat("\n--- Klasė:", kl, "---\n")
  duomenys_kl <- be_isskirciu[be_isskirciu$label == kl, kiekybiniai]
  apr_stat_kl <- sapply(duomenys_kl, function(stulpelis) {
    sapply(skaiciavimai_statistikai, function(f) f(stulpelis))
  })
  print(round(apr_stat_kl, 2))
}

# --- DUOMENŲ NORMAVIMAS ---

# požymiai, kuriuos normuosim
pozymiai <- c("RR_l_0", "RR_l_0/RR_l_1", "P_val", "R_pos", "S_pos", "signal_std")

# 1. Min–Max normavimas
minmax_norm <- be_isskirciu
for (f in pozymiai) {
  xmin <- min(minmax_norm[[f]], na.rm = TRUE)
  xmax <- max(minmax_norm[[f]], na.rm = TRUE)
  minmax_norm[[f]] <- (minmax_norm[[f]] - xmin) / (xmax - xmin)
}

# 2. Z-score normavimas (pagal vidurkį ir dispersiją)
zscore_norm <- be_isskirciu
for (f in pozymiai) {
  mean_val <- mean(zscore_norm[[f]], na.rm = TRUE)
  var_val <- var(zscore_norm[[f]], na.rm = TRUE)
  zscore_norm[[f]] <- (zscore_norm[[f]] - mean_val) / sqrt(var_val)
}

# --- Patikrinam rezultatus ---
cat("\n=== Pavyzdys Min–Max normalizacijos (pirmos 6 eilutės) ===\n")
print(head(minmax_norm[pozymiai]))

cat("\n=== Pavyzdys Z-score normalizacijos (pirmos 6 eilutės) ===\n")
print(head(zscore_norm[pozymiai]))

# --- Išsaugoti jei reikia ---
write_csv(minmax_norm, "ekg_minmax_norm.csv")
write_csv(zscore_norm, "ekg_zscore_norm.csv")


# --- Originalūs duomenys ---
orig_long <- be_isskirciu %>%
  select(all_of(pozymiai)) %>%
  pivot_longer(everything(), names_to = "pozymis", values_to = "reiksme")

# --- Min–Max normalizuoti ---
mm_long <- minmax_norm %>%
  select(all_of(pozymiai)) %>%
  pivot_longer(everything(), names_to = "pozymis", values_to = "reiksme")

# --- pagal vidurki ir dispersija normalizuoti ---
zs_long <- zscore_norm %>%
  select(all_of(pozymiai)) %>%
  pivot_longer(everything(), names_to = "pozymis", values_to = "reiksme")

# --- Histogramos: Originalūs ---
g_orig <- ggplot(orig_long, aes(x = reiksme)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  facet_wrap(~pozymis, scales = "free") +
  labs(title = "Originalios reikšmės", x = "Reikšmė", y = "Dažnis")

# --- Histogramos: Min–Max ---
g_mm <- ggplot(mm_long, aes(x = reiksme)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "black") +
  facet_wrap(~pozymis, scales = "free") +
  labs(title = "Min–max normavimas", x = "Reikšmė [0–1]", y = "Dažnis")

# --- Histogramos: Z-score ---
g_zs <- ggplot(zs_long, aes(x = reiksme)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "black") +
  facet_wrap(~pozymis, scales = "free") +
  labs(title = "Normavimas pagal vidurkį ir dispersiją", x = "Reikšmė (vidurkis≈0, sd≈1)", y = "Dažnis")

# Atvaizduoti
print(g_orig)
print(g_mm)
print(g_zs)
#---Koreliacija---

pozymiai <- c("RR_l_0","RR_l_0/RR_l_1","P_val","R_pos","S_pos","signal_std")

# Bendra koreliacijų matrica
cat("\n=== Bendra koreliacijų matrica ===\n")
koreliacijos_bendra <- cor(be_isskirciu[, pozymiai], use = "complete.obs", method = "pearson")
print(round(koreliacijos_bendra, 2))
corrplot(koreliacijos_bendra, method = "circle", type = "lower",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         title = "Bendros koreliacijos", mar = c(0,0,2,0))

# Koreliacijos pagal klases
for (kl in unique(be_isskirciu$label)) {
  cat("\n=== Klasė:", kl, "===\n")
  duomenys_kl <- be_isskirciu[be_isskirciu$label == kl, pozymiai]
  
  korel_kl <- cor(duomenys_kl, use = "complete.obs", method = "pearson")
  print(round(korel_kl, 2))
  
  corrplot(korel_kl, method = "circle", type = "lower",
           tl.col = "black", tl.srt = 45, addCoef.col = "black",
           title = paste("Koreliacijos klasei", kl), mar = c(0,0,2,0))
}


# VIZUALIOJI DUOMENU ANALIZE #
##### 1 Eksperimentas
### Taskiniai grafikai
# Su isskirtimis
taskai <- pradinis_failas %>%
  mutate(indeks = row_number()) %>%
  pivot_longer(all_of(pozymiai), names_to = "pozymis", values_to = "reiksme")

taskiniai_grafikai <- ggplot(taskai, aes(x = indeks, y = reiksme, color = as.factor(label))) +
  geom_point(size = 1) +
  facet_wrap(~pozymis, scales = "free") +
  labs(title = "Taškiniai grafikai visiems požymiams su išskirtimis", x = "Įrašo indeksas", y = "Požymio reikšmė", color = "Klasė")
print(taskiniai_grafikai)

# Be isskirciu
taskai_be_isskirciu <- be_isskirciu %>%
  mutate(indeks = row_number()) %>%
  pivot_longer(all_of(pozymiai), names_to = "pozymis", values_to = "reiksme")

taskiniai_grafikai_be_isskirciu <- ggplot(taskai_be_isskirciu, aes(x = indeks, y = reiksme, color = as.factor(label))) +
  geom_point(size = 1) +
  facet_wrap(~pozymis, scales = "free") +
  labs(title = "Taškiniai grafikai visiems požymiams be išskirčių", x = "Įrašo indeksas", y = "Požymio reikšmė", color = "Klasė")
print(taskiniai_grafikai_be_isskirciu)

##### 2 Eksperimentas
### Staciakampe diagrama
# Su isskirtimis
taskai_RS_su_isskirtim <- pradinis_failas %>%
  select(label, S_pos, R_pos) %>%
  pivot_longer(cols = c(S_pos, R_pos), names_to = "pozymis", values_to = "reiksme")

staciakampe_diagrama_RS_isskirtis <- ggplot(taskai_RS_su_isskirtim, aes(x = as.factor(label), y = reiksme, fill = as.factor(label))) +
  geom_boxplot(color = "black") +
  facet_wrap(~pozymis, scales = "free") +
  labs(title = "Stačiakampės diagramos su išskirtimis: R_pos, S_pos požymiai", 
       x = "Klasė", y = "Požymio reikšmė", fill = "Klasė")
print(staciakampe_diagrama_RS_isskirtis)

# Be isskirciu
taskai_RS <- be_isskirciu %>%
  select(label, S_pos, R_pos) %>%
  pivot_longer(cols = c(S_pos, R_pos), names_to = "pozymis", values_to = "reiksme")

staciakampe_diagrama_RS <- ggplot(taskai_RS, aes(x = as.factor(label), y = reiksme, fill = as.factor(label))) +
  geom_boxplot(color = "black") +
  facet_wrap(~pozymis, scales = "free") +
  labs(title = "Stačiakampės diagramos be išskirčių: R_pos, S_pos požymiai", 
       x = "Klasė", y = "Požymio reikšmė", fill = "Klasė")
print(staciakampe_diagrama_RS)

##### 3 Eksperimentas
### Histogramos
# Nenormalizuoti
histogramos <- ggplot(taskai_be_isskirciu, aes(x = reiksme, color = as.factor(label), fill = as.factor(label))) +
  geom_histogram(bins = 30, alpha = 0.5) +
  facet_wrap(~pozymis, scales = "free") +
  labs(title = "Histogramos be duomenų normavimo", x = "Požymio reikšmė", y = "Dažnis", color = "Klasė", fill = "Klasė")
print(histogramos)

# Normalizuoti
norm_duom <- minmax_norm %>%
  select(label, all_of(pozymiai)) %>%
  pivot_longer(cols = all_of(pozymiai), names_to = "pozymis", values_to = "reiksme") 

histogramos_norm <- ggplot(norm_duom, aes(x = reiksme, color = as.factor(label), fill = as.factor(label))) +
  geom_histogram(bins = 30, alpha = 0.5) +
  facet_wrap(~pozymis, scales = "free") +
  labs(title = "Histogramos su duomenų normavimu", x = "Požymio reikšmė", y = "Dažnis", color = "Klasė", fill = "Klasė")
print(histogramos_norm)

### Tankio diagrama
# Nenormalizuoti
tankio_diagrama_nenormuota <- ggplot(taskai_be_isskirciu, aes(x = reiksme, color = as.factor(label), fill = as.factor(label))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~pozymis, scales = "free") +
  labs(title = "Tankio diagrama be duomenų normavimo", x = "Požymio reikšmė", y = "Dažnis", color = "Klasė", fill = "Klasė")
print(tankio_diagrama_nenormuota)

# Normalizuoti
tankio_diagrama_normuota <- ggplot(norm_duom, aes(x = reiksme, color = as.factor(label), fill = as.factor(label))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~pozymis, scales = "free") +
  labs(title = "Tankio diagrama su duomenų normavimu", x = "Požymio reikšmė", y = "Dažnis", color = "Klasė", fill = "Klasė")
print(tankio_diagrama_normuota)
