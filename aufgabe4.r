
# ============================================================
# PORTFOLIO – METHODENLEHRE IIb
# Paket 4: Mixed-ANOVA
#
# Autor: Okan Acikel
# ============================================================


# ============================================================
# 1. PAKET INSTALLIEREN UND LADEN
# ============================================================

# afex installieren, falls noch nicht vorhanden
if (!requireNamespace("afex", quietly = TRUE)) {
  install.packages("afex", dependencies = TRUE)
}

library(afex)


# ============================================================
# 2. DATENSATZ LADEN
# ============================================================

data("CO2")

# Überblick über den Datensatz
str(CO2)
head(CO2)
summary(CO2)


# ============================================================
# 3. DATEN VORBEREITEN
# ============================================================

# Laut Aufgabenstellung werden nur die Konzentrationen
# 175, 350 und 675 verwendet.

CO2_sub <- subset(
  CO2,
  conc %in% c(175, 350, 675)
)

# Konzentration in Faktor umwandeln
CO2_sub$conc <- as.factor(CO2_sub$conc)


# ============================================================
# 4. DATEN KONTROLLIEREN
# ============================================================

str(CO2_sub)

# Konzentrationsstufen
levels(CO2_sub$conc)

# Herkunft der Pflanzen
levels(CO2_sub$Type)

# Pflanzen-IDs
unique(CO2_sub$Plant)

# Anzahl der Beobachtungen
dim(CO2_sub)

# Fehlende Werte prüfen
sum(is.na(CO2_sub))

# Beobachtungen pro Pflanze und Konzentration
table(
  CO2_sub$Plant,
  CO2_sub$conc
)


# ============================================================
# 5. DESKRIPTIVE MITTELWERTE
# ============================================================

# Mittelwerte nach Herkunft und Konzentration
aggregate(
  uptake ~ Type + conc,
  data = CO2_sub,
  FUN = mean
)


# ============================================================
# 6. MIXED-ANOVA DURCHFÜHREN
# ============================================================

# Abhängige Variable: uptake
# ID: Plant
# Between-Subject: Type
# Within-Subject: conc

modell <- aov_ez(
  id = "Plant",
  dv = "uptake",
  data = CO2_sub,
  between = "Type",
  within = "conc",
  anova_table = list(
    correction = "GG",
    es = "pes"
  )
)


# ============================================================
# 7. ERGEBNIS AUSGEBEN
# ============================================================

modell


# Ausführliche ANOVA-Tabelle
nice(
  modell,
  correction = "GG",
  es = "pes"
)


# ============================================================
# 8. ANOVA-TABELLE DIREKT ANZEIGEN
# ============================================================

modell$anova_table


# ============================================================
# 9. SPHÄRIZITÄT / MAUCHLY-TEST
# ============================================================

summary(modell$Anova)


# ============================================================
# 10. INTERAKTIONSPLOT
# ============================================================

interaction.plot(
  x.factor = CO2_sub$conc,
  trace.factor = CO2_sub$Type,
  response = CO2_sub$uptake,
  fun = mean,
  type = "b",
  xlab = "CO2-Konzentration",
  ylab = "Mittlere CO2-Aufnahme",
  trace.label = "Herkunft",
  main = "CO2-Aufnahme nach Konzentration und Herkunft"
)


# ============================================================
# 11. MITTELWERTE FÜR INTERPRETATION
# ============================================================

mittelwerte <- aggregate(
  uptake ~ Type + conc,
  data = CO2_sub,
  FUN = mean
)

mittelwerte


# ============================================================
# ENDE
# ============================================================
