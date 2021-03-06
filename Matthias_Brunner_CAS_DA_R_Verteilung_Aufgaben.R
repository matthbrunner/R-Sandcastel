#-------------------------------------------------------------------------------
# Name:        Matthias_Brunner_CAS_DA_R_Verteilung_Aufgaben.R
# Purpose:     Aufgaben Verteilung
#
# Author:      Matthias Brunner
#
# Created:     10.12.2016
#-------------------------------------------------------------------------------

# Binomialverteilung
# Problem: Die Wahrscheinlichkeit, dass man im Roulette bei
# einmaligem Setzen auf "rot" gewinnt, ist p = 18/37 = 0,486. Definieren wir
# mit x jene Anzahl der Spiele, bei denen man bei f�nfmaligem Setzen
# auf "rot" gewinnt.
# 1. Wie gross ist bei f�nfmaligem Setzen auf "rot" die
# Wahrscheinlichkeit, dass man �fter gewinnt als verliert?

1 - pbinom(q = 2, size = 5, p = 18/37)
# q = red or black = 2
# size = 5 Versuche
# p (prob) = m�glicher erfolg

# oder
pbinom(q = 2, size = 5, p = 18/37, lower.tail = FALSE)

# Frage warum 18/37 auf einem Rouletttisch hate es 36 felder mit Zahlen, dazu 
# kommen noch je ein Schwarz oder Rot Feld?

# 2. Welche Anzahl der Gewinne wird in 90% der F�lle h�chstens
# erreicht?
yprob <- dbinom(0:5, size = length(0:5)-1, prob = 18/37)
names(yprob) <- 0:5
barplot(yprob)

# Poissonverteilung
# Problem: Das Restaurant Fat's Pizza f�hrt Buch �ber die Anzahl an
# G�sten, die das Restaurant betreten. Laut der Aufzeichnungen ist der
# Erwartungswert m� = 12,1 zwischen 20:00 und 22:00 Uhr. Bestimmen
# Sie mit der Poisson-Verteilung die Wahrscheinlichkeit, dass zwischen
# 20 Uhr und 22 Uhr folgende Szenarien auftauchen:

# Es sind genau 8 G�ste im Restaurant.
ppois(8, lambda = 12.1)
# Es sind h�chstens 10 G�ste im Restaurant.
ppois(10, lambda = 12.1)
# Es sind zwischen 9 und 15 G�ste im Restaurant.
ppois(15, lambda = 12.1)-ppois(8, lambda = 12.1)
# Es sind mindestens 11 G�ste anwesend.
ppois(10, lambda = 12.1, lower.tail = FALSE)

# Stetige Gelichverteilung
# Problem: Sie haben heute um 9 Uhr einwichtige Meeting, aber Sie
# verschlafen und wachen erst um 8:30 Uhr auf. Um 8:40 rennen Sie
# aus der T�r, auf dem Weg ins B�ro.
# Sie brauchen 6 Minuten zur Bushaltestelle. Dann warten Sie auf den
# Bus, der morgens alle f�nf Minuten kommt, die Wartezeit in Minuten ist
# gleichverteilt zwischen 0 und 5. Je nach Verkehrslage braucht der Bus
# nun noch einmal (gleichverteilt) 10 bis 15 Minuten bis ins B�ro.

# 1. Welche Verteilung hat die Zufallsvariable X, welche die gesamte
# Pendelzeit von Haust�r bis ins B�ro beschreibt?
# L�sung
# min = 6 min Weg zur Bushaltestelle + 10 min Busfahrt + 0 min Wartezeit
# max = 6 min Weg zur Bushaltestelle + 15 min Busfahrt + 5 min Wartezeit
# q = 9:00 - 8:40

# 2. Mit welcher Wahrscheinlichkeit schaffen Sie es noch rechtzeitig
# ins B�ro?
punif(q = 20 ,min = 16, max = 26 )

# Problem: In einer vierw�chigen Datenerhebung missen Sie die L�nge
# der Telefongespr�che, die Sie auf Ihrem Handy f�hren. Sie finden
# heraus, dass die Dauer der Gespr�che (in Minuten) einer
# Exponentialverteilung folgt, und Ihre Gespr�che im Erwartungswert
# 3 Minuten lang sind.

# 1. Welche Verteilung hat die Zufallsvariable X, welche die Dauer der
# Telefongespr�che in Minuten beschreibt?
X = 1/3
# 2. Das Telefon klingelt. Wie gross ist die Wahrscheinlichkeit, dass
# dieses Gespr�ch h�chstens eine Minute dauert?
pexp(1, rate = 1/3)

# 3. Wie gross ist die Wahrscheinlichkeit, dass das Gespr�ch l�nger
# als eine Minute dauert?
pexp(1, rate = 1/3, lower.tail = FALSE)
1-pexp(1, rate = 1/3)

# 4. Mit welcher Wahrscheinlichkeit dauert das Gespr�ch zwischen
# einer und drei Minuten?
pexp(3, rate = 1/3)-pexp(1, rate = 1/3)

# 5. Berechnen und interpretieren Sie das 25%-Quantil dieser
# Verteilung.
qexp(p = 0.25, rate = 1/3)

# Normalverteilung
# Problem: In einer Fabrik werden T�ten mit Kartoffelchips bef�llt. Das
# durchschnittliche Gewicht der T�ten soll nach den Angaben des
# Werkes 200 g betragen. Da die T�ten maschinell bef�llt werden, wird
# dieser Wert nur mit einer Standardabweichung von 4 g eingehalten. Mit
# welcher Wahrscheinlichkeit werden T�ten abgef�llt, deren Gewicht...
# 1. um weniger als 2 g vom Mittelwert abweicht?
pnorm(q = 202, mean = 200, sd = 4) - pnorm(q = 198, mean = 200, sd = 4)

# 2. �ber 205 g liegt?
pnorm(q = 205, mean = 200, sd = 4, lower.tail = FALSE)
1 - pnorm(q = 205, mean = 200, sd = 4)

# 3. Welches Gewicht wird von 95% der T�ten �berschritten?
qnorm(p = 0.95, mean = 200, sd = 4)

# Chiquadrat-Verteilung
# Problem: Mit welcher Wahrscheinlichkeit liegt der Wert einer
# chi2-Verteilung mit df = 11 �ber 15?
pchisq(15, df=11, lower.tail = FALSE)


# t-Verteilung
# Problem: Mit welcher Wahrscheinlichkeit liegt der Wert der
# Studentschen t-Verteilung unter -0.????5, respektive unter 1? Der
# Freiheitsgrad sei 7.
pt(c(-0.5,1),df=7)



