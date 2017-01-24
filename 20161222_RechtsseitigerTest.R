# Problem: Ein Produzent von Keksen behauptet, dass seine Produkte
# ein Höchstanteil an gesättigten Fettsäuren von 2 g pro Keks enthalten.
# In einer Stichprobe von 35 Keksen wurde ein Mittelwert von 2.1 g
# gemessen. Nehmen Sie eine Standardabweichung von 0.25 g an.
# Kann die Behauptung bei einem Signifikanzniveau von 5% verworfen
# werden?
qnorm(0.95, mean = 2, sd = 0.25/sqrt(35))

pnorm(2.1, mean = 2.1, sd = 0.25/sqrt(35), lower.tail = FALSE)

z.test(2.1, mu = 2.1, stdev = 0.25, alternative = "greater", n = 35)
