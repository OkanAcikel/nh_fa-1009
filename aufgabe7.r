# ============================================================
# PORTFOLIO – METHODENLEHRE IIb
# Paket 7: Anwendung des Bayes-Faktors
#
# Autor: Okan Acikel
# ============================================================


# ============================================================
# 1. PAKET INSTALLIEREN UND LADEN
# ============================================================

# Nur notwendig, falls BayesFactor noch nicht installiert ist
if (!requireNamespace("BayesFactor", quietly = TRUE)) {
  install.packages("BayesFactor", dependencies = TRUE)
}

library(BayesFactor)


# ============================================================
# 2. DATENSATZ LADEN
# ============================================================

data("sleep")

# Überblick über den Datensatz
str(sleep)
head(sleep)
summary(sleep)


# ============================================================
# 3. GRUPPENMITTELWERTE BERECHNEN
# ============================================================

aggregate(
  extra ~ group,
  data = sleep,
  FUN = mean
)


# ============================================================
# 4. TRADITIONELLER T-TEST
# ============================================================

# Laut Aufgabenstellung:
# - unabhängige Stichproben
# - Varianzhomogenität wird angenommen
#
# Deshalb:
# var.equal = TRUE

t_test <- t.test(
  extra ~ group,
  data = sleep,
  var.equal = TRUE
)

# Ergebnis ausgeben
t_test


# ============================================================
# 5. BAYES-FAKTOR BERECHNEN
# ============================================================

bayes_test <- ttestBF(
  formula = extra ~ group,
  data = sleep
)

bayes_test


# ============================================================
# 6. BAYES-FAKTOR ALS ZAHL AUSGEBEN
# ============================================================

bf_wert <- extractBF(
  bayes_test,
  onlybf = TRUE
)

bf_wert


# ============================================================
# 7. ERGEBNISSE KOMPAKT AUSGEBEN
# ============================================================

cat("\n")
cat("============================================\n")
cat("KLASSISCHER T-TEST\n")
cat("============================================\n")

print(t_test)


cat("\n")
cat("============================================\n")
cat("BAYES-FAKTOR\n")
cat("============================================\n")

print(bayes_test)

cat("\nBF10 =", round(bf_wert, 3), "\n")


# ============================================================
# ENDE
# ============================================================
