#-------------------------------------------------------------------------------
# Name:        Matthias_Brunner_CAS_DA_R_Tests.R
# Purpose:     Aufgaben Verteilung
#
# Author:      Matthias Brunner
#
# Created:     12.01.2017
#-------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Aufgabe: Linksseitiger Test bei µ, ?? bekannt
# Problem: Die Datei "lightbulbs.txt" enthält eine neue Stichprobe des
# Glühbirnenherstellers. Laden Sie die Datei mit dem Befehl scan.
# Lässt sich aufgrund dieser Stichprobe die Behauptung des Herstellers,
# dass die Glühbirnen eine Mindestlebensdauer von 10000 Stunden
# besitzen, bei einem Signifikanzniveau von 1% verwerfen? Die
# Standardabweichung beträgt 120 Stunden.
# Löschen des Workspace
rm(list = ls())

setwd("C:/Workspace/Weiterbildung/Deskriptive Statistik/R-Unterrichtsdateien")

lightbulbs <- scan("lightbulbs.txt")
lightbulbs

# Definieren der Variablen
mu0 <- 10000 # Mittelwert
sigma <- 120 # Standardabweichung
size <- length(lightbulbs) # Anzahl Beobachtungen
sample.mean <- mean(lightbulbs) # Stichprobenmittelwert
alpha <- 0.01 # Signifikanzniveau

# Variante "step by step"
# Bestimmen kritischer Wert
z.alpha <- qnorm(alpha) 
z.alpha

z <- (sample.mean-mu0)/(sigma/sqrt(size))

z < z.alpha
# H0 wird verworfen

# Berechnen der Wahrscheinlichkeit
pval <- pnorm(z) # Bestimmen unterer p-Wert
pval

pval < alpha
# [1] 3.088019e-05
# Da der p-Wert kleiner ist als alpha (0.1) kann die 0-Hypothese verworfen werden.

# Variante mit TeachingDemos
library(TeachingDemos)
test <- z.test(lightbulbs, 
       mu=mu0, 
       stdev = sigma, 
       n = size, 
       alternative = "less", 
       conf.level = 1-alpha)
test$p.value < alpha
# [1] 3.088019e-05
# Da der p-Wert kleiner ist als alpha (0.1) kann die 0-Hypothese verworfen werden.



# ------------------------------------------------------------------------------
# Aufgabe: Rechtsseitiger Test bei µ, ?? bekannt
# Problem: Die Datei "cookies.txt" enthält eine neue Stichprobe des
# Keksherstellers. Laden Sie die Datei mit dem Befehl scan.
# Lässt sich aufgrund dieser Stichprobe die Behauptung des Herstellers,
# dass die Kekse einen maximalen Anteil von 2 g enthalten, bei einem
# Signifikanzniveau von 10% verwerfen? Die Standardabweichung
# beträgt 0.25 g.

# Löschen des Workspace
rm(list = ls())
setwd("C:/Workspace/Weiterbildung/Deskriptive Statistik/R-Unterrichtsdateien")
cookies <- scan("cookies.txt")
cookies

alpha <- 0.1
mu0 <- 2

n.cookies <- length(cookies)

# Variante mit TeachingDemos
test <- z.test(cookies, mu = 2, stdev = 0.25, conf.level = 1-alpha, alternative = "greater")

test$p.value > 1-alpha
# Die Angaben des Herstellers könne gehalten werden.



# ------------------------------------------------------------------------------
# Aufgabe: Zweiseitiger Test bei µ, ?? bekannt
# Problem: Die Datei "penguins.txt" enthält eine neue Zufallsstichprobe
# einer Pinguinkolonie. Laden Sie die Datei mit dem Befehl scan.
# Lässt sich aufgrund dieser Stichprobe die Behauptung, dass sich das
# Durchschnittsgewicht der Pinguine nicht verändert hat, bei einem
# Signifikanzniveau von 5% verwerfen? Die Standardabweichung beträgt
# 2.5 kg.

# Löschen des Workspace
rm(list = ls())
setwd("C:/Workspace/Weiterbildung/Deskriptive Statistik/R-Unterrichtsdateien")
penguins <- scan("penguins.txt")
penguins

sigma <- 2.5
alpha <- 0.05
# mu0 <- ?
n.penguins <- length(penguins)

z.test(penguins, stdev = 2.5, conf.level = alpha,  n = n.penguins, alternative = "two.sided")

# Müsste hier nicht das durchschnittliche Gewicht (mu0) noch angegeben sein, 
# damit eine Aussage getroffen werden kann?
# Falls das durchschnittliche Gewicht, wie in den Übungen im Unterricht 15.4 kg
z.test(penguins, mu = 15.4, stdev = 2.5, conf.level = alpha, n = n.penguins, alternative = "two.sided")


# ------------------------------------------------------------------------------
# Aufgabe: Linksseitiger Test bei µ, ?? unbekannt
# Problem: Die Datei "lightbulbs.txt" enthält eine neue Stichprobe des
# Glühbirnenherstellers. Laden Sie die Datei mit dem Befehl scan.
# Lässt sich aufgrund dieser Stichprobe die Behauptung des Herstellers,
# dass die Glühbirnen eine Mindestlebensdauer von 10000 Stunden
# besitzen, bei einem Signifikanzniveau von 1% verwerfen?

# Löschen des Workspace
rm(list = ls())
setwd("C:/Workspace/Weiterbildung/Deskriptive Statistik/R-Unterrichtsdateien")
lightbulbs <- scan("lightbulbs.txt")

mu <- 10000
alpha <- 0.01


test <- t.test(lightbulbs, mu = 10000, conf.level = 1-alpha, alternative = "less")

test$p.value > 1-alpha
# Der p-Wert (p-value = 1.592e-05) ist kleiner als das Signifikanzniveau, die Behauptung
# kann nicht gehalten werden.

# Bestimmen der Standardaweichung
n <- length(lightbulbs)
s <- sd(lightbulbs)
xbar <- mean(lightbulbs)
t.val = (sample.mean-mu)/(s/sqrt(n))

# Berechnen Kritischer Wert
t.alpha = qt(1-alpha, df=n-1)
-t.alpha

# Berechnen alternative Lösung
pval = pt(t.val, df=n-1)
pval

pval > 1-alpha
# Der p-Wert (p-value = 1.592e-05) ist kleiner als das Signifikanzniveau, die Behauptung
# kann nicht gehalten werden.

# ------------------------------------------------------------------------------
# Aufgabe: Rechtsseitiger Test bei µ, ?? unbekannt
# Problem: Die Datei "cookies.txt" enthält eine neue Stichprobe des
# Keksherstellers. Laden Sie die Datei mit dem Befehl scan.
# Lässt sich aufgrund dieser Stichprobe die Behauptung des Herstellers,
# dass die Kekse einen maximalen Anteil von 2 g enthalten, bei einem
# Signifikanzniveau von 10% verwerfen?

rm(list = ls())
setwd("C:/Workspace/Weiterbildung/Deskriptive Statistik/R-Unterrichtsdateien")
cookies <- scan("cookies.txt")

alpha <- 0.1

test <- t.test(cookies, mu = 2, conf.level = 0.9, alternative = "greater")
test$p.value > 1-alpha
# H0 wird nicht verworfen

# ------------------------------------------------------------------------------
# Aufgabe: Zweiseitiger Test bei µ, ?? unbekannt
# Problem: Die Datei "penguins.txt" enthält eine neue Zufallsstichprobe
# einer Pinguinkolonie. Laden Sie die Datei mit dem Befehl scan.
# Lässt sich aufgrund dieser Stichprobe die Behauptung, dass sich das
# Durchschnittsgewicht der Pinguine nicht verändert hat, bei einem
# Signifikanzniveau von 5% verwerfen?

# Löschen des Workspace
rm(list = ls())
setwd("C:/Workspace/Weiterbildung/Deskriptive Statistik/R-Unterrichtsdateien")
penguins <- scan("penguins.txt")

alpha <- 0.05

test <- t.test(penguins, conf.level = alpha, alternative = "two.sided")

# Müsste hier nicht das durchschnittliche Gewicht (mu0) noch angegeben sein, 
# damit eine Aussage getroffen werden kann?
# Falls das durchschnittliche Gewicht, wie in den Übungen im Unterricht 15.4 kg
test <- t.test(penguins, mu = 15.4, conf.level = alpha, alternative = "two.sided")
test$p.value

xbar <- mean(penguins)
mu0 <- 15.4
s <- sd(penguins)
n <- length(penguins)

t.val = (xbar-mu0)/(s/sqrt(n))
t.val

ta = qt(1-alpha/2, df=n-1)
c(-ta, ta)

test$p.value > 1-alpha/2
-test$p.value < 1- alpha/2
# H0 wird verworfen


# ------------------------------------------------------------------------------
# Problem: Die Datei "grocerystore.csv" enthält eine Zufallsstichprobe
# von Kunden einer Metzgerei. Neben dem Geschlecht der Kunden
# wurde auch deren Verweilzeit im Laden notiert. Importieren Sie die
# Datei mit dem Befehl read.csv.
# Lässt sich aufgrund dieser Stichprobe die Behauptung, dass die
# Metzgerei mehrheitlich von Frauen besucht wird, bei einem
# Signifikanzniveau von 5% verwerfen?

rm(list = ls())
setwd("C:/Workspace/Weiterbildung/Deskriptive Statistik/R-Unterrichtsdateien")
grocerystore = read.csv("grocerystore.csv", header = TRUE, sep = ";")

woman <- nrow(grocerystore[grocerystore$gender == "F", ])

p <- .05
n <- nrow(grocerystore)
test <- prop.test(woman, n, alternative = "less", correct = FALSE)

test$p.value < p
# H0 wird nicht verworfen


# ------------------------------------------------------------------------------
# Aufgabe: Rechtsseitiger Test des Populationsanteils p
# Problem: Um nicht in Schwierigkeiten zu geraten, darf der Anteil
# geplatzter Kredite einer Bank den Anteil von 12% nicht überschreiten.
# Die Datei "creditcards.csv" enthält die Ergebnisse einer Untersuchung
# unter 1000 Kunden der Bank. Importieren Sie die Datei mit dem Befehl
# read.csv.
# Kann die Bank, bei einem Signifikanzniveau von 5%, aufgrund dieser
# Stichprobe sicher sein, dass die geplatzten Kredite den Anteil von 12%
# nicht übersteigen?

rm(list = ls())
setwd("C:/Workspace/Weiterbildung/Deskriptive Statistik/R-Unterrichtsdateien")
creditcards = read.csv("creditcards.csv", header = TRUE, sep = ";")

bounced <- length(creditcards[creditcards$bounced == "Yes", ])

p <- .05
n <- nrow(creditcards)
test <- prop.test(bounced, n, p = .12, alternative = "greater", correct = FALSE)

test$p.value > p
# H0 wird verworfen


# ------------------------------------------------------------------------------
# Aufgabe: Zweiseitiger Test des Populationsanteils p
# Problem: Der Anteil der Rechtshänder unter den Studierenden von
# survey wird auf 90% geschätzt. Lässt sich diese Behauptung bei
# einem Signifikanzniveau von 1% verwerfen?
rm(list = ls())

library(MASS)
right.hand <- table(survey$W.Hnd)[2]

n <- sum(table(survey$W.Hnd))

p <- .90
alpha <- .01
test <- prop.test(right.hand, n, p, conf.level = 1 - alpha, alternative = "two.sided", correct = FALSE )

test$p.value < -1-alpha/2
test$p.value > 1-alpha/2
# H0 wird nicht verworfen
