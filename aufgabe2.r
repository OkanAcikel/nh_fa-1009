# ============================================================
# PORTFOLIO – METHODENLEHRE IIb
# Aufgabe 2: Zweiweg-Varianzanalyse ohne Messwiederholung
#
# Autor: Okan Acikel
# ============================================================


# ------------------------------------------------------------
# 1. Vorbereitung
# ------------------------------------------------------------

# Datensatz laden
data("ToothGrowth")

# Überblick über den Datensatz
str(ToothGrowth)
head(ToothGrowth)

# dose von einer numerischen Variable
# in eine Faktorvariable umwandeln
ToothGrowth$dose <- factor(ToothGrowth$dose)

# Kontrolle
str(ToothGrowth)


# ============================================================
# AUFGABE 1: LEVENE-TEST
# ============================================================

# Paket laden
# Falls noch nicht installiert:
# install.packages("car")

library(car)

# Levene-Test durchführen
leveneTest(
  len ~ supp * dose,
  data = ToothGrowth
)


# ============================================================
# AUFGABE 2: ZWEIWEG-ANOVA
# ============================================================

# Zweiweg-ANOVA berechnen
anova_modell <- aov(
  len ~ supp * dose,
  data = ToothGrowth
)

# ANOVA-Tabelle ausgeben
summary(anova_modell)


# ------------------------------------------------------------
# Interaktionsplot erstellen
# ------------------------------------------------------------

interaction.plot(
  x.factor = ToothGrowth$dose,
  trace.factor = ToothGrowth$supp,
  response = ToothGrowth$len,
  fun = mean,
  type = "b",
  xlab = "Dosierung",
  ylab = "Mittlere Zahnlänge",
  trace.label = "Supplement",
  main = "Interaktion zwischen Supplement und Dosierung"
)


# ------------------------------------------------------------
# Gruppenmittelwerte berechnen
# ------------------------------------------------------------

aggregate(
  len ~ supp + dose,
  data = ToothGrowth,
  FUN = mean
)
