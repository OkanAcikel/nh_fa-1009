# ============================================================
# PORTFOLIO – METHODENLEHRE IIb
# Paket 5: Post-hoc-Tests zu einer Zweiweg-ANOVA
#
# Autor: Okan Acikel
# ============================================================


# ============================================================
# 1. DATENSATZ LADEN
# ============================================================

data("PlantGrowth")

# Überblick
str(PlantGrowth)
head(PlantGrowth)
summary(PlantGrowth)


# ============================================================
# 2. ZWEITEN FAKTOR "LOCATION" ERZEUGEN
# ============================================================

# Genau wie in der Aufgabenstellung vorgegeben
set.seed(123)

PlantGrowth$location <- factor(
  rep(c("A", "B"), each = 15)
)

# Kontrolle
str(PlantGrowth)
head(PlantGrowth)

# Häufigkeiten
table(PlantGrowth$group)
table(PlantGrowth$location)

# Kombinationen von group und location
table(
  PlantGrowth$group,
  PlantGrowth$location
)


# ============================================================
# 3. DESKRIPTIVE STATISTIK
# ============================================================

# Mittelwerte für group
aggregate(
  weight ~ group,
  data = PlantGrowth,
  FUN = mean
)

# Mittelwerte für location
aggregate(
  weight ~ location,
  data = PlantGrowth,
  FUN = mean
)

# Mittelwerte für alle Kombinationen
aggregate(
  weight ~ group + location,
  data = PlantGrowth,
  FUN = mean
)


# ============================================================
# 4. ZWEIWEG-ANOVA
# ============================================================

# Untersucht werden:
#
# 1. Haupteffekt group
# 2. Haupteffekt location
# 3. Interaktion group:location

anova_modell <- aov(
  weight ~ group * location,
  data = PlantGrowth
)

# ANOVA-Tabelle
summary(anova_modell)


# ============================================================
# 5. INTERAKTIONSPLOT
# ============================================================

interaction.plot(
  x.factor = PlantGrowth$group,
  trace.factor = PlantGrowth$location,
  response = PlantGrowth$weight,
  fun = mean,
  type = "b",
  xlab = "Behandlung",
  ylab = "Mittleres Trockengewicht",
  trace.label = "Standort",
  main = "Interaktion zwischen Behandlung und Standort"
)


# ============================================================
# 6. TUKEY-HSD FÜR DIE HAUPTEFFEKTE
# ============================================================

tukey <- TukeyHSD(anova_modell)

tukey


# ============================================================
# 7. POST-HOC-VERGLEICH DER KOMBINATIONEN
# ============================================================

# Für die konkrete Frage nach den Kombinationen aus
# Behandlung UND Standort erzeugen wir einen gemeinsamen Faktor.

PlantGrowth$group_location <- interaction(
  PlantGrowth$group,
  PlantGrowth$location
)

# Faktor kontrollieren
levels(PlantGrowth$group_location)


# ============================================================
# 8. EINFACHE ANOVA DER KOMBINIERTEN GRUPPEN
# ============================================================

komb_modell <- aov(
  weight ~ group_location,
  data = PlantGrowth
)

summary(komb_modell)


# ============================================================
# 9. TUKEY-HSD FÜR ALLE KOMBINATIONEN
# ============================================================

tukey_komb <- TukeyHSD(
  komb_modell,
  "group_location"
)

tukey_komb


# ============================================================
# 10. GRUPPENMITTELWERTE
# ============================================================

gruppenmittelwerte <- aggregate(
  weight ~ group_location,
  data = PlantGrowth,
  FUN = mean
)

gruppenmittelwerte


# ============================================================
# 11. GRÖSSTE MITTELWERTDIFFERENZ AUTOMATISCH BESTIMMEN
# ============================================================

# Mittelwerte extrahieren
mw <- tapply(
  PlantGrowth$weight,
  PlantGrowth$group_location,
  mean
)

# Alle Kombinationen paarweise vergleichen
differenzen <- outer(
  mw,
  mw,
  "-"
)

# Absolute Differenzen
abs_differenzen <- abs(differenzen)

# Diagonale ausschließen
diag(abs_differenzen) <- NA

# Größte Differenz
max_diff <- max(
  abs_differenzen,
  na.rm = TRUE
)

max_diff


# Position der größten Differenz bestimmen
position <- which(
  abs_differenzen == max_diff,
  arr.ind = TRUE
)

# Erste eindeutige Kombination
position <- position[1, ]

gruppe_1 <- names(mw)[position[1]]
gruppe_2 <- names(mw)[position[2]]

gruppe_1
gruppe_2


# Ergebnis ausgeben
cat(
  "\nDie größte Mittelwertdifferenz besteht zwischen",
  gruppe_1,
  "und",
  gruppe_2,
  ".\n"
)

cat(
  "Die Differenz beträgt:",
  round(max_diff, 3),
  "\n"
)


# ============================================================
# 12. TUKEY-ERGEBNISSE NACH DIFFERENZ SORTIEREN
# ============================================================

tukey_tabelle <- as.data.frame(
  tukey_komb$group_location
)

# Absolute Differenz hinzufügen
tukey_tabelle$abs_diff <- abs(
  tukey_tabelle$diff
)

# Größte Differenzen zuerst
tukey_tabelle[
  order(
    -tukey_tabelle$abs_diff
  ),
]


# ============================================================
# ENDE
# ============================================================
