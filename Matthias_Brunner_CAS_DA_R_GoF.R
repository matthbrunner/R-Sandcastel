# Problem: Die Unileitung vermutet folgendes Rauchverhalten ihrer Studierenden.
# Heavy Never Occassionaly Regular
# 4.5%  79.5% 8.5%         7.5%
# Prüfen Sie, ob die Stichprobe aus survey sich mit dieser Behauptung
# verträgt. Bestimmen Sie den p-Wert, ohne auf die Funktion
# chisq.test zurückzugreifen.

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
library(MASS)
head(survey)

smoke.prob <- c(.045, .795, .085, .075)

# ------------------------------------------------------------------------------
# Variante 1: Ohne NA Werte
# Lesen der Daten und herausfiltern der NA Werte
smoke <- survey$Smoke[!is.na(survey$Smoke)]

# Berechnen der Frequenz
smoke.freq <- table(smoke)

# Berechnen der Erwartung
smoke.estimate <- smoke.prob * length(smoke)

# Berechnen der Differenz
smoke.difference <- smoke.freq - smoke.estimate
# Berechnen des Chi
smoke.chi <- sum(smoke.difference * smoke.difference / smoke.estimate)

# Bestimmen des Freiheitsgrad
smoke.df = length(smoke.freq)-1

# Bestimmen dew p-chi sqrt
pchisq(smoke.chi, df=smoke.df, lower=FALSE)

# ------------------------------------------------------------------------------
# Variante 2: Mit NA Wert

# Lesen der Daten und herausfiltern der NA Werte
smoke <- survey$Smoke

# Berechnen der Frequenz
smoke.freq <- table(smoke)

# Berechnen der Erwartung
smoke.estimate <- smoke.prob * length(smoke)

# Berechnen der Differenz
smoke.difference <- smoke.freq - smoke.estimate
# Berechnen des Chi
smoke.chi <- sum(smoke.difference * smoke.difference / smoke.estimate)

# Bestimmen des Freiheitsgrad
smoke.df = length(smoke.freq)-1

# Bestimmen dew p-chi sqrt
pchisq(smoke.chi, df=smoke.df, lower=FALSE)

# Der unterschied zwischen der Variante 1 und Variante 2 beträgt 0.0005, das hat keinen Einfluss auf das Resultat!!!

# Die Nullhypothese kann nicht verworfen werden

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Problem: Die Datei RauchenGeschlecht.xlsx zeigt das Geschlecht
# der Neugeborenen mit dem Rauchverhalten der Eltern in den ersten
# Monaten der Schwangerschaft. Sind diese beiden Variablen
# unabhängig? Arbeiten Sie mit alpha = 0.05.
library(xlsx)
library(TeachingDemos)
data <- read.xlsx("C:/Workspace/Weiterbildung/Deskriptive Statistik/R-Unterrichtsdateien/RauchenGeschlecht.xlsx", sheetName="Blatt1")
# head(data)

alpha <- .05

sex.child <- data$Geschlecht.des.Kindes
smoke.parent <- data$Rauchverhalten.der.Eltern

tbl <- table(sex.child, smoke.parent)

test <- chisq.test(tbl)
test
test$p.value > alpha
# H0 wird nicht Verworfen, da p-value grösser als alpha ist!

