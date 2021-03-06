# Problem: Ein Hersteller von Gl�hbirnen behauptet eine
# Mindestlebensdauer von 10000 Stunden f�r seine Gl�hbirnen. Der
# Mittelwert einer Stichprobe aus 30 Gl�hbirnen ergab einen
# Stichprobenmittelwert von 9'900 Stunden. Die Standardabweichung
# der Population betr�gt 120 Stunden. K�nnen wir bei einem
# Signifikanzniveau von 5% die Behauptung des Herstellers verwerfen?

xbar <- 9900
mu0 <- 10000 # Standardabweichung
sigma <- 120
n <- 30
z <- (xbar-mu0)/(sigma/sqrt(n))
z

alpha <- 0.05
z.alpha <- qnorm(alpha)
z.alpha

pval <- pnorm(z) # p-Werte
pval

qnorm(0.05, 10000, 120/sqrt(30))

library(TeachingDemos)
z.test(9900, mu=10000, stdev = 120, n = 30, alternative = "less")

# In diesem Fall w�rden wir die Hypothese vom Hersteller Verwerfen.
