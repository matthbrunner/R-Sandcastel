library(MASS)
survey

# Wenn die Wahre standard Verteilung bekannt ist
survey$Height
t.test(survey$Height)
height.response <- na.omit(survey$Height)
zstar <- qnorm(0.975)
zstar
qnorm(0.025)
# bilden der stichprobe 
s <- sd(height.response)
# ist gegeben aus der Aufgabenstellung (Fehlerbereich)
E <- 1.2
# Berechnung des
zstar^2*s^2/E^2
# das resultat 258.695 es müssten als mindestens 259 Personen befragt werden

# 
gender.response <- na.omit(survey$Sex)
n <- length(gender.response)
k <- sum(gender.response == "Female")
k
n
pbar <- k/n
pbar

# Intervallschätzung Folie 32
SE <- sqrt(pbar * (1 - pbar)/n)
zstar <- qnorm(0.975)
E <- SE * zstar
E
pbar + c(-E, E)

# der conf.level ist im Standard 95% so gesetzt
prop.test(k, n, conf.level = 0.95)
prop.test(k, n, correct = FALSE)
p <- 0.5
E <- 0.05

zstar <- qnorm(0.975)

zstar^2*p*(1-p)/E^2


