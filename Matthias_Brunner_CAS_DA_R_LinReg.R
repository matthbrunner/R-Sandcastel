#-------------------------------------------------------------------------------
# Name:        CAS_DA_R_LinReg_Aufgaben.R
# Purpose:     Aufgaben LineareRegression
#
# Author:      Matthias Brunner
#
# Created:     24.01.2017
#-------------------------------------------------------------------------------

rm(list = ls())

# ---Schätzen eines y-Wertes----------------------------------------------------
# Betrachten Sie den Datensatz mtcars. Modellieren Sie das Gewicht
# wt der Autos als Funktion der Motorleistung hp.
# Welches durchschnittliche Gewicht wird für ein Auto geschätzt, dessen
# Motor eine Leistung von 200 PS aufweist?

# Laden der Library MASS
library(MASS)
head(mtcars)
engin.lm <- lm(wt ~ hp, data=mtcars) 

coeffs <- coefficients(engin.lm)
coeffs

horse.power <- 200
weigth <- coeffs[1] + coeffs[2]*horse.power
weigth

newdata <- data.frame(hp=200)
predict(engin.lm, newdata)

# Das erwartete Gewicht bei 200 PS liegt bei 3.718 lbs (Weight [1000 lbs])


# ---Bestimmtheitsmass----------------------------------------------------------
# Bestimmen Sie das Bestimmtheitsmass r^2 des linearen Modells zu mtcars.
engin.lm <- lm(wt ~ hp, data=mtcars)
summary(engin.lm)$r.squared

# ------------------------------------------------------------------------------


# ---Signifikanztest------------------------------------------------------------
# Untersuchen Sie, ob zwischen den Grössen wt und hp aus mtcars ein
# signifikanter Zusammenhang besteht.
engin.lm <- lm(wt ~ hp, data=mtcars)
summary(engin.lm)

# ------------------------------------------------------------------------------


# ----Konfidenzintervalle-------------------------------------------------------
# Bestimmen Sie ein 95%-Konfidenzintervall für das durchschnittliche
# Gewicht bei einer Motorenleistung von 200 PS.
engin.lm <- lm(wt ~ hp, data=mtcars)
newdata <- data.frame(hp=200)
predict(engin.lm, newdata, interval="confidence")

# Das durchschnittliche Gewicht beträgt bei 200 PS zwischen 3.37 und 4.06 
# (Weight [1000 lbs]), bei einem Signifikanzniveau von 95%
# ------------------------------------------------------------------------------


# ----Prognoseintervalle-------------------------------------------------------
# Bestimmen Sie ein 95%-Prognoseintervall für das durchschnittliche
# Gewicht bei einer Motorenleistung von 200 PS.
engin.lm <- lm(wt ~ hp, data=mtcars)
newdata <- data.frame(hp=200)
predict(engin.lm, newdata, interval="predict")

# Das durchschnittliche Gewicht bei 200 PS liegt zwischen  2.15 und 5.28 (1000 lbs),
# bei einem Signifikanzniveau von 95%.
# ------------------------------------------------------------------------------


# ---Residuen-Plot--------------------------------------------------------------
# Stellen Sie die Residuen des linearen Modells zwischen dem Gewicht
# und der Leistung aus mtcars grafisch dar.
engin.lm <- lm(wt ~ hp, data=mtcars)
engin.res <- resid(engin.lm)

plot(mtcars$wt, engin.res, ylab="Residuen",
     xlab="Weight (1000 lbs)", main="Weight from Cars")
abline(0,0)
# ------------------------------------------------------------------------------


# ---QQ-Plot--------------------------------------------------------------------
# Erstellen Sie das Normal-Wahrscheinlichkeits-Diagramm der Residuen
# aus dem Datensatz mtcars.

plot(engin.lm, which=2)
