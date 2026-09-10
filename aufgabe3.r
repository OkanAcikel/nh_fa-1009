# ============================================================
# PORTFOLIO – METHODENLEHRE IIb
# Paket 3: Univariate Varianzanalyse mit Messwiederholung
#
# Autor: Okan Acikel
# ============================================================


# ============================================================
# 0. OPTIONAL: ARBEITSUMGEBUNG LEEREN
# ============================================================

rm(list = ls())


# ============================================================
# 1. PAKET INSTALLIEREN UND LADEN
# ============================================================

# afex automatisch installieren, falls es noch nicht vorhanden ist
if (!requireNamespace("afex", quietly = TRUE)) {
  install.packages("afex", dependencies = TRUE)
}

# Paket laden
library(afex)


# ============================================================
# 2. DATENSATZ LADEN
# ============================================================

data("obk.long")

# Prüfen, ob der Datensatz erfolgreich geladen wurde
exists("obk.long")


# ============================================================
# 3. ÜBERBLICK ÜBER DEN DATENSATZ
# ============================================================

# Struktur
str(obk.long)

# Erste Zeilen
head(obk.long)

# Zusammenfassung
summary(obk.long)

# Anzahl Zeilen und Variablen
dim(obk.long)

# Variablennamen
names(obk.long)


# ============================================================
# 4. VERSUCHSPERSONEN UND PHASEN IDENTIFIZIEREN
# ============================================================

# Die Versuchspersonen werden durch "id" identifiziert.
# Die Studienphasen werden durch "phase" dargestellt.

# Anzahl der Versuchspersonen
length(unique(obk.long$id))

# IDs anzeigen
unique(obk.long$id)

# Phasen anzeigen
levels(obk.long$phase)

# Häufigkeiten der einzelnen Phasen
table(obk.long$phase)


# ============================================================
# 5. MESSZEITPUNKTE UNTERSUCHEN
# ============================================================

# Der Datensatz enthält zusätzlich mehrere Messzeitpunkte
# innerhalb jeder Phase.

table(obk.long$hour)

# Kombination aus Versuchsperson, Phase und Messzeitpunkt prüfen
table(
  obk.long$id,
  obk.long$phase
)


# ============================================================
# 6. VOLLSTÄNDIGKEIT DER DATEN PRÜFEN
# ============================================================

# Anzahl aller fehlenden Werte
sum(is.na(obk.long))

# Fehlende Werte für jede einzelne Variable
colSums(is.na(obk.long))

# Prüfen, ob die für die Analyse relevanten Variablen
# vollständig vorhanden sind
all(
  complete.cases(
    obk.long[, c("id", "phase", "hour", "value")]
  )
)

# Anzahl der Beobachtungen pro Versuchsperson und Phase
table(
  obk.long$id,
  obk.long$phase
)


# ============================================================
# 7. DATEN FÜR DIE PHASENANALYSE VORBEREITEN
# ============================================================

# Im Datensatz liegen mehrere Messzeitpunkte je Phase vor.
# Für die hier geforderte Untersuchung des Faktors "phase"
# wird ein vergleichbarer Messzeitpunkt aus jeder Phase verwendet.

obk_phase <- subset(
  obk.long,
  hour == 1
)


# ============================================================
# 8. KONTROLLE DER AUSGEWÄHLTEN DATEN
# ============================================================

str(obk_phase)

head(obk_phase)

# Jede Versuchsperson sollte nun einen Wert
# je Phase besitzen.
table(
  obk_phase$id,
  obk_phase$phase
)


# ============================================================
# 9. REIHENFOLGE DER PHASEN FESTLEGEN
# ============================================================

# Inhaltlich sinnvolle Reihenfolge:
# pre  = vor der Behandlung
# post = nach der Behandlung
# fup  = Follow-up

obk_phase$phase <- factor(
  obk_phase$phase,
  levels = c("pre", "post", "fup")
)

# Kontrolle
levels(obk_phase$phase)


# ============================================================
# 10. DESKRIPTIVE STATISTIK
# ============================================================

# Mittelwerte je Phase
mittelwerte <- aggregate(
  value ~ phase,
  data = obk_phase,
  FUN = mean
)

mittelwerte


# Standardabweichungen je Phase
standardabweichungen <- aggregate(
  value ~ phase,
  data = obk_phase,
  FUN = sd
)

standardabweichungen


# Anzahl Beobachtungen je Phase
aggregate(
  value ~ phase,
  data = obk_phase,
  FUN = length
)


# ============================================================
# 11. ANOVA MIT MESSWIEDERHOLUNG
# ============================================================

# id:
# identifiziert die Versuchsperson
#
# dv:
# abhängige Variable = value
#
# within:
# Messwiederholungsfaktor = phase

modell <- aov_ez(
  id = "id",
  dv = "value",
  data = obk_phase,
  within = "phase",
  anova_table = list(
    correction = "GG",
    es = "pes"
  )
)


# ============================================================
# 12. ANOVA-ERGEBNIS AUSGEBEN
# ============================================================

modell


# Ausführlichere Ausgabe
nice(
  modell,
  correction = "GG",
  es = "pes"
)


# ============================================================
# 13. MAUCHLY-TEST AUF SPHÄRIZITÄT
# ============================================================

summary(
  modell$Anova
)


# ============================================================
# 14. ANOVA-TABELLE DIREKT ANZEIGEN
# ============================================================

modell$anova_table


# ============================================================
# 15. MERKHILFE ZUR INTERPRETATION
# ============================================================

# ------------------------------------------------------------
# Haupteffekt phase:
# ------------------------------------------------------------
#
# p < 0,05:
#
# Die Nullhypothese wird verworfen.
# Das Aggressionsverhalten unterscheidet sich
# statistisch signifikant zwischen den Phasen.
#
#
# p >= 0,05:
#
# Die Nullhypothese wird nicht verworfen.
# Es kann kein statistisch signifikanter Unterschied
# zwischen den Phasen festgestellt werden.


# ------------------------------------------------------------
# Mauchly-Test:
# ------------------------------------------------------------
#
# H0:
# Die Sphärizitätsannahme ist erfüllt.
#
#
# p > 0,05:
#
# H0 wird nicht verworfen.
# Die Sphärizitätsannahme kann als erfüllt
# angesehen werden.
#
# -> Keine Korrektur zwingend notwendig.
#
#
# p <= 0,05:
#
# H0 wird verworfen.
# Die Sphärizitätsannahme ist verletzt.
#
# -> Greenhouse-Geisser-korrigierte Werte verwenden.


# ============================================================
# 16. MITTELWERTE NOCH EINMAL KOMPAKT AUSGEBEN
# ============================================================

cat("\n")
cat("==============================================\n")
cat("MITTELWERTE DER STUDIENPHASEN\n")
cat("==============================================\n")

print(mittelwerte)


# ============================================================
# 17. AUTOMATISCHE AUSGABE DES P-WERTES
# ============================================================

cat("\n")
cat("==============================================\n")
cat("ANOVA-ERGEBNIS\n")
cat("==============================================\n")

print(modell$anova_table)


# ============================================================
# 18. OPTIONAL: GRAFISCHE DARSTELLUNG
# ============================================================

# Einfacher Verlauf der Mittelwerte

plot(
  1:3,
  mittelwerte$value,
  type = "b",
  xaxt = "n",
  xlab = "Studienphase",
  ylab = "Mittleres Aggressionsverhalten",
  main = "Aggressionsverhalten über die Studienphasen"
)

axis(
  1,
  at = 1:3,
  labels = mittelwerte$phase
)


# ============================================================
# ENDE DES SKRIPTS
# ============================================================
