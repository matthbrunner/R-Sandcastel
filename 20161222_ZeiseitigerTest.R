# Problem: Das durchschnittliche Gewicht von antarktischen
# Königspinguinen einer bestimmten Kolonie betrug im letzten Jahr
# 15.4 kg. Eine Stichprobe von 35 Pinguinen derselben Kolonie zeigte
# ein Durchschnittsgewicht von 14.6 kg. Die Standardabweichung der
# Population beträgt 2.5 kg. Lässt sich die Behauptung, dass sich das
# Durchschnittsgewicht nicht verändert hat, bei einem Signifikanzniveau
# von 5% verwerfen?

qnorm(0.975, mean = 15.4, sd = 2.5/sqrt(35))
qnorm(0.025, mean = 15.4, sd = 2.5/sqrt(35))

# Berechnung des p-Wertes multipliziert mit 2, da der p-Wert auf beidenseiten
# liegt muss das ganze noch mit 2 Multipliziert werden
pnorm(14.6, 15.4, 2.5/sqrt(35)) * 2


z.test(14.6, mu = 15.4, stdev = 2.5, n = 35)
