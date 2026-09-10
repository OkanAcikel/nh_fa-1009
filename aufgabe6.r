# ============================================================
# PORTFOLIO – METHODENLEHRE IIb
# Paket 6: Mediation in der Regression
#
# Autor: Okan Acikel
# ============================================================


# ============================================================
# 1. DATENSATZ
# ============================================================

# Die Datei Arbeitsleistung.rda zunächst in RStudio laden.
#
# Entweder über:
# Environment -> Load Workspace
#
# oder, wenn die Datei im Arbeitsordner liegt:
#
# load("Arbeitsleistung.rda")


# ============================================================
# 2. DATENSATZ KONTROLLIEREN
# ============================================================

# Struktur
str(Arbeitsleistung)

# Erste Beobachtungen
head(Arbeitsleistung)

# Zusammenfassung
summary(Arbeitsleistung)

# Variablennamen
names(Arbeitsleistung)


# ============================================================
# 3. VARIABLEN STANDARDISIEREN
# ============================================================

# Genau wie in der Aufgabenstellung vorgegeben

Arbeitsleistung_std <- data.frame(
  scale(Arbeitsleistung)
)

# Kontrolle
summary(Arbeitsleistung_std)

# Die Mittelwerte sollten nach der Standardisierung
# ungefähr 0 betragen.

colMeans(Arbeitsleistung_std)


# ============================================================
# 4. EINFACHE REGRESSION
# ============================================================

# Fragestellung:
# Hat Motivation einen signifikanten Einfluss
# auf die berufliche Leistung?

mod <- lm(
  performance ~ motivation,
  data = Arbeitsleistung_std
)

summary(mod)


# ============================================================
# 5. MEDIATION – PFAD a
# ============================================================

# X -> M
#
# Beeinflusst Motivation die Fachkompetenz?

modell_a <- lm(
  competence ~ motivation,
  data = Arbeitsleistung_std
)

summary(modell_a)


# ============================================================
# 6. MEDIATION – PFADE b UND c'
# ============================================================

# M + X -> Y
#
# competence -> performance = Pfad b
# motivation -> performance = direkter Effekt c'

modell_b <- lm(
  performance ~ motivation + competence,
  data = Arbeitsleistung_std
)

summary(modell_b)


# ============================================================
# 7. MEDIATION MIT BOOTSTRAPPING
# ============================================================

# Paket mediation automatisch installieren,
# falls es noch nicht vorhanden ist.

if (!requireNamespace("mediation", quietly = TRUE)) {
  install.packages("mediation", dependencies = TRUE)
}

library(mediation)


# ============================================================
# 8. MEDIATIONSMODELL
# ============================================================

mediation_modell <- mediate(
  model.m = modell_a,
  model.y = modell_b,
  
  # X
  treat = "motivation",
  
  # Mediator
  mediator = "competence",
  
  # Bootstrap verwenden
  boot = TRUE,
  
  # Anzahl Bootstrap-Stichproben
  sims = 5000
)


# ============================================================
# 9. ERGEBNIS DER MEDIATION
# ============================================================

summary(mediation_modell)


# ============================================================
# 10. GRAFISCHE DARSTELLUNG
# ============================================================

plot(mediation_modell)


# ============================================================
# 11. KORRELATIONEN ZUR ZUSÄTZLICHEN KONTROLLE
# ============================================================

cor(
  Arbeitsleistung_std[
    c(
      "motivation",
      "competence",
      "performance"
    )
  ],
  use = "complete.obs"
)


# ============================================================
# 12. ALLE REGRESSIONSMODELLE KOMPAKT AUSGEBEN
# ============================================================

cat("\n")
cat("============================================\n")
cat("GESAMTEFFEKT: motivation -> performance\n")
cat("============================================\n")

summary(mod)


cat("\n")
cat("============================================\n")
cat("PFAD a: motivation -> competence\n")
cat("============================================\n")

summary(modell_a)


cat("\n")
cat("============================================\n")
cat("PFADE b UND c'\n")
cat("============================================\n")

summary(modell_b)


cat("\n")
cat("============================================\n")
cat("MEDIATIONSANALYSE\n")
cat("============================================\n")

summary(mediation_modell)


# ============================================================
# ENDE
# ============================================================
