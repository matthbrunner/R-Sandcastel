# ------------------------------------------------------------------------------
# Lineare Regression

plot(faithful$waiting, faithful$eruptions)

eruptions.lm <- lm(eruptions ~ waiting, data = faithful)
summary(eruptions.lm)

# Im Resultat bei Coefficients
# Intercept = (Beta2)
# waiting = Steigung (Beta1)
coeffs <- coefficients(eruptions.lm)
coeffs

?abline
abline(coeffs[1], coeffs[2],col = "red")

# Waiting ist der 
waiting <- 80
duration <- coeffs[2]*waiting + coeffs[1]

# Berechnung mit R
# erstellen eines Dataframe, da predict ein dataframe benötigt
newdata <- data.frame(waiting = 80)
predict(eruptions.lm, newdata)

newdata <- data.frame(waiting = c(80,90))
predict(eruptions.lm, newdata)
summary(eruptions.lm)

# Multiple R-squared 80 % der Schwankung wird durch das Lineare Model erklärt
# Ajusted R-squared ist noch der Mittelwert berücksichtig.
# 0.8 ist als eine gute Korrelation 0.6 ist eine Starke Korrelation

# Jede andere Stichprobe wird ein anderes Resultat geben. Daher ist es sinvoll
# für die erwartete wartezeit nicht eine Zeit anzugeben sondern ein confidenceintervall
# damit können andere Stichproben besser verglichen werden
predict(eruptions.lm, newdata, interval = "confidence")

# Prognoseintervall
# Voraussage für die einzelwerte
predict(eruptions.lm, newdata, interval = "predict")

# Die Daten auf eine gerade legen
eruptions.res <- resid(eruptions.lm)
plot(faithful$waiting, eruptions.res)
abline(0,0)
# Hier sind die gleiche Streuung aber es ist auch ein Muster erkennbar.
# Normalverteilung sehen wir nicht!


plot(eruptions.lm)
# Normal Q-Q Plot: Wenn alle Punkte auf der Geraden liegen. Sie die Daten normalverteilt.

