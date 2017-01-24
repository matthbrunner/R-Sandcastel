# ------------------------------------------------------------------------------
# Chi-Test
# Erstellen der Matrix
tab <- matrix(c(110, 120, 20, 30, 20, 90, 60, 30, 10, 10), nrow = 2, byrow = TRUE)
tab
rownames(tab) <- c("weiblich", "männlich")
colnames(tab) <- c("A", "B", "C", "D", "E")

# Berechnung Chi-Quadrat Prüft ob zwischen zwei Merkmalen eine Abhähnigkeit besteht.
# Falls in der Tabelle ein Wert kleiner als 5 ist liefert R ein Fehler
# Falls die Summe < 5 ist müsste man diese zusammenlegen oder die Stichprobe vergrössern
test <- chisq.test(tab)
# X-Squared ist Chi-quadrat

# Hier würde man sagen Ja!

q.chi <- qchisq(.95, df = 4)
# beide Werte sind nicht normiert!
test$statistic > q.chi
# Wenn das resultat qchisq <= als X-squared (Chi-quadrat) besteht kein Zusammenhang!

# Df (Freiheitsgrad) erhalte ich?


# ------------------------------------------------------------------------------
# Anpassungstest engl.(Goodness-of-fit-Test)
# ------------------------------------------
# Intervall | relative Häufigkeit | Erwartet

# Berechnen der Erwarteten Werte
# s = .5 | s^2 = 0.5
pnorm(999.5, 999.93, .5)
pnorm(1000, 999.93, .5)-pnorm(999.5, 999.93, 0.5)
pnorm(1000.5, 999.93, .5)-pnorm(1000, 999.93, 0.5)
pnorm(1000.5, 999.93, .5, lower.tail = FALSE)

chisq.test(c(20, 32, 34, 14), p=c(0.1949, 0.3608,0.3172,0.1271))

# Das Resultat wird verworfen

# ------------------------------------------------------------------------------

library(MASS)
faithful
# Berechnung der Korrelation in der Beschreibenden Statistik
cor(faithful$eruptions, faithful$waiting)

# Test ist ein Zusammenhang vorhanden
# H0 = 0 (haben wir als angenommen als keine Korrelation)
cor.test(faithful$eruptions, faithful$waiting)

# Der p Wert ist kleiner 0 also besteht kein zusammenhang.

