# ============================================================
# Aufgabe 2 – Zweiweg-Varianzanalyse ohne Messwiederholung
# Teil 1 und Teil 2
# ============================================================

# Benötigtes Paket für den Levene-Test
install.packages("car")   # Nur einmal notwendig
library(car)

# ------------------------------------------------------------
# Vorbereitung
# ------------------------------------------------------------

# Datensatz laden
data("ToothGrowth")

# Variable dose in einen Faktor umwandeln
ToothGrowth$dose <- factor(ToothGrowth$dose)

# Struktur des Datensatzes kontrollieren
str(ToothGrowth)


# ============================================================
# 1. Levene-Test
# ============================================================

# Prüfung auf Varianzhomogenität
leveneTest(len ~ supp * dose, data = ToothGrowth)

# Interpretation:
# H0: Die Varianzen der Gruppen sind gleich.
#
# Wenn p > 0,05:
# H0 wird nicht verworfen.
# -> Die Varianzhomogenität kann angenommen werden.
#
# Im Portfolio ergibt sich ungefähr:
# p = 0,1484
#
# Da 0,1484 > 0,05 ist, liegt kein signifikanter Hinweis
# auf unterschiedliche Varianzen vor.
# Die Voraussetzung der Varianzhomogenität ist somit erfüllt.


# ============================================================
# 2. Zweiweg-ANOVA
# ============================================================

# Modell berechnen:
# len  = Zahnlänge
# supp = Supplement (OJ oder VC)
# dose = Dosierung

modell <- aov(len ~ supp * dose, data = ToothGrowth)

# ANOVA-Tabelle ausgeben
summary(modell)


# ============================================================
# Interaktionsplot
# ============================================================

interaction.plot(
  x.factor = ToothGrowth$dose,
  trace.factor = ToothGrowth$supp,
  response = ToothGrowth$len,
  fun = mean,
  xlab = "Dosierung",
  ylab = "Mittlere Zahnlänge",
  trace.label = "Supplement"
)
