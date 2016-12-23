# Problem: Ein Hersteller von Glühbirnen behauptet eine
# Mindestlebensdauer von 10000 Stunden für seine Glühbirnen. Der
# Mittelwert einer Stichprobe aus 30 Glühbirnen ergab einen
# Stichprobenmittelwert von 9'900 Stunden. Die Standardabweichung
# der Population beträgt 120 Stunden. Können wir bei einem
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

# In diesem Fall würden wir die Hypothese vom Hersteller Verwerfen.

pnorm(2.1, mean=2, sd=0.25/sqrt(25), lower.tail = FALSE)
z.test(2.1, mu=2, stdev = 0.25, alternative = "greater", n=35)

qnorm(0.025, mean = 15.4, sd=2.5/sqrt(35))
2*pnorm(14.6, 15.4, sd=2.5/sqrt(35))
z.test(14.6, 15.4, 2.5, n=35)
qnorm(0.05, mean=10000, sd=120/sqrt(30))


pnorm(9900, mean = 10000, sd = 120/sqrt(30))
z.test(9900, mu=10000, stdev = 120, n = 30, alternative = "less")

