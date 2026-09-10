# ============================================================
# PORTFOLIO METHODENLEHRE IIb
# Aufgabe 1 – Konfirmatorische Faktorenanalyse (CFA)
# Portrait Values Questionnaire (ESS9)
#
# Autor: Okan Acikel
# ============================================================

# ------------------------------------------------------------
# 1. Paket laden
# ------------------------------------------------------------
library(lavaan)

# ------------------------------------------------------------
# 2. Datensatz prüfen
# ------------------------------------------------------------
# ESS9 ist bereits im Environment vorhanden

str(ESS9)
dim(ESS9)
names(ESS9)

# ------------------------------------------------------------
# 3. PVQ-Items auswählen
# ------------------------------------------------------------
pvq_items <- c(
  "ipcrtiv","impfree",
  "impdiff","ipadvnt",
  "ipgdtim","impfun",
  "ipshabt","ipsuces",
  "imprich","iprspot",
  "impsafe","ipstrgv",
  "ipfrule","ipbhprp",
  "ipmodst","imptrad",
  "iphlppl","iplylfr",
  "ipeqopt","ipudrst","impenv"
)

# Prüfen, ob alle Variablen existieren
pvq_items %in% names(ESS9)

# Datensatz mit nur den PVQ-Variablen erzeugen
PVQ <- ESS9[, pvq_items]

# ------------------------------------------------------------
# 4. Fehlende Werte bereinigen
# ------------------------------------------------------------
PVQ[] <- lapply(PVQ, function(x){
  
  x <- as.numeric(x)
  
  # ESS-Codes außerhalb 1–6 als fehlend behandeln
  x[x < 1 | x > 6] <- NA
  
  x
})

# ------------------------------------------------------------
# 5. Items umpolen
# ------------------------------------------------------------
# 1 = sehr ähnlich
# 6 = überhaupt nicht ähnlich
# Nach dem Umpolen gilt:
# Hoher Wert = hohe Zustimmung

PVQ[] <- lapply(PVQ, function(x) 7 - x)

# ------------------------------------------------------------
# 6. Schwartz-Modell definieren
# ------------------------------------------------------------
schwartz_model <- '

SelfDirection =~ a*ipcrtiv + a*impfree
Stimulation   =~ b*impdiff + b*ipadvnt
Hedonism      =~ c*ipgdtim + c*impfun
Achievement   =~ d*ipshabt + d*ipsuces
Power         =~ e*imprich + e*iprspot
Security      =~ f*impsafe + f*ipstrgv
Conformity    =~ g*ipfrule + g*ipbhprp
Tradition     =~ h*ipmodst + h*imptrad
Benevolence   =~ i*iphlppl + i*iplylfr

Universalism =~ ipeqopt + ipudrst + impenv

'

# ------------------------------------------------------------
# 7. Konfirmatorische Faktorenanalyse
# ------------------------------------------------------------
fit <- cfa(
  model = schwartz_model,
  data = PVQ,
  ordered = pvq_items,
  estimator = "WLSMV",
  std.lv = TRUE,
  missing = "pairwise"
)

# ------------------------------------------------------------
# 8. Gesamtausgabe
# ------------------------------------------------------------
summary(
  fit,
  fit.measures = TRUE,
  standardized = TRUE
)

# ------------------------------------------------------------
# 9. Wichtige Fit-Indizes
# ------------------------------------------------------------
fitMeasures(
  fit,
  c("cfi","rmsea","tli","srmr")
)

# ------------------------------------------------------------
# 10. Standardisierte Faktorladungen
# ------------------------------------------------------------
subset(
  standardizedSolution(fit),
  op == "=~"
)

# ------------------------------------------------------------
# 11. Korrelationen der latenten Faktoren
# ------------------------------------------------------------
lavInspect(fit, "cor.lv")

# ------------------------------------------------------------
# 12. Konvergenz prüfen
# ------------------------------------------------------------
lavInspect(fit, "converged")
