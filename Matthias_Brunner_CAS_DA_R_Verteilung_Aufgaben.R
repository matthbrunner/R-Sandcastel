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
# mit x jene Anzahl der Spiele, bei denen man bei fünfmaligem Setzen
# auf "rot" gewinnt.
# 1. Wie gross ist bei fünfmaligem Setzen auf "rot" die
# Wahrscheinlichkeit, dass man öfter gewinnt als verliert?

1 - pbinom(q = 2, size = 5, p = 18/37)
# q = red or black = 2
# size = 5 Versuche
# p (prob) = möglicher erfolg

# oder
pbinom(q = 2, size = 5, p = 18/37, lower.tail = FALSE)

# Frage warum 18/37 auf einem Rouletttisch hate es 36 felder mit Zahlen, dazu 
# kommen noch je ein Schwarz oder Rot Feld?

# 2. Welche Anzahl der Gewinne wird in 90% der Fälle höchstens
# erreicht?
yprob <- dbinom(0:5, size = length(0:5)-1, prob = 18/37)
names(yprob) <- 0:5
barplot(yprob)

# Poissonverteilung
# Problem: Das Restaurant Fat's Pizza führt Buch über die Anzahl an
# Gästen, die das Restaurant betreten. Laut der Aufzeichnungen ist der
# Erwartungswert mü = 12,1 zwischen 20:00 und 22:00 Uhr. Bestimmen
# Sie mit der Poisson-Verteilung die Wahrscheinlichkeit, dass zwischen
# 20 Uhr und 22 Uhr folgende Szenarien auftauchen:

# Es sind genau 8 Gäste im Restaurant.
ppois(8, lambda = 12.1)
# Es sind höchstens 10 Gäste im Restaurant.
ppois(10, lambda = 12.1)
# Es sind zwischen 9 und 15 Gäste im Restaurant.
ppois(15, lambda = 12.1)-ppois(8, lambda = 12.1)
# Es sind mindestens 11 Gäste anwesend.
ppois(10, lambda = 12.1, lower.tail = FALSE)

# Stetige Gelichverteilung
# Problem: Sie haben heute um 9 Uhr einwichtige Meeting, aber Sie
# verschlafen und wachen erst um 8:30 Uhr auf. Um 8:40 rennen Sie
# aus der Tür, auf dem Weg ins Büro.
# Sie brauchen 6 Minuten zur Bushaltestelle. Dann warten Sie auf den
# Bus, der morgens alle fünf Minuten kommt, die Wartezeit in Minuten ist
# gleichverteilt zwischen 0 und 5. Je nach Verkehrslage braucht der Bus
# nun noch einmal (gleichverteilt) 10 bis 15 Minuten bis ins Büro.

# 1. Welche Verteilung hat die Zufallsvariable X, welche die gesamte
# Pendelzeit von Haustür bis ins Büro beschreibt?
# Lösung
# min = 6 min Weg zur Bushaltestelle + 10 min Busfahrt + 0 min Wartezeit
# max = 6 min Weg zur Bushaltestelle + 15 min Busfahrt + 5 min Wartezeit
# q = 9:00 - 8:40

# 2. Mit welcher Wahrscheinlichkeit schaffen Sie es noch rechtzeitig
# ins Büro?
punif(q = 20 ,min = 16, max = 26 )

# Problem: In einer vierwöchigen Datenerhebung missen Sie die Länge
# der Telefongespräche, die Sie auf Ihrem Handy führen. Sie finden
# heraus, dass die Dauer der Gespräche (in Minuten) einer
# Exponentialverteilung folgt, und Ihre Gespräche im Erwartungswert
# 3 Minuten lang sind.

# 1. Welche Verteilung hat die Zufallsvariable X, welche die Dauer der
# Telefongespräche in Minuten beschreibt?
X = 1/3
# 2. Das Telefon klingelt. Wie gross ist die Wahrscheinlichkeit, dass
# dieses Gespräch höchstens eine Minute dauert?
pexp(1, rate = 1/3)

# 3. Wie gross ist die Wahrscheinlichkeit, dass das Gespräch länger
# als eine Minute dauert?
pexp(1, rate = 1/3, lower.tail = FALSE)
1-pexp(1, rate = 1/3)

# 4. Mit welcher Wahrscheinlichkeit dauert das Gespräch zwischen
# einer und drei Minuten?
pexp(3, rate = 1/3)-pexp(1, rate = 1/3)

# 5. Berechnen und interpretieren Sie das 25%-Quantil dieser
# Verteilung.
qexp(p = 0.25, rate = 1/3)

# Normalverteilung
# Problem: In einer Fabrik werden Tüten mit Kartoffelchips befüllt. Das
# durchschnittliche Gewicht der Tüten soll nach den Angaben des
# Werkes 200 g betragen. Da die Tüten maschinell befüllt werden, wird
# dieser Wert nur mit einer Standardabweichung von 4 g eingehalten. Mit
# welcher Wahrscheinlichkeit werden Tüten abgefüllt, deren Gewicht...
# 1. um weniger als 2 g vom Mittelwert abweicht?
pnorm(q = 202, mean = 200, sd = 4) - pnorm(q = 198, mean = 200, sd = 4)

# 2. über 205 g liegt?
pnorm(q = 205, mean = 200, sd = 4, lower.tail = FALSE)
1 - pnorm(q = 205, mean = 200, sd = 4)

# 3. Welches Gewicht wird von 95% der Tüten überschritten?
qnorm(p = 0.95, mean = 200, sd = 4)

# Chiquadrat-Verteilung
# Problem: Mit welcher Wahrscheinlichkeit liegt der Wert einer
# chi2-Verteilung mit df = 11 über 15?
pchisq(15, df=11, lower.tail = FALSE)


# t-Verteilung
# Problem: Mit welcher Wahrscheinlichkeit liegt der Wert der
# Studentschen t-Verteilung unter -0.????5, respektive unter 1? Der
# Freiheitsgrad sei 7.
pt(c(-0.5,1),df=7)



