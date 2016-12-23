mu0 <- 10000
xbar <- 9900
s = 120
n = 30
# n-1 = 29
qt(0.05, 29)
120/sqrt(30)
21.9*1.7
10000-37.23
qt(0.05, 29)*120/sqrt(30)
m0 + qt(0.05, 29)*120/sqrt(30)

x <- scan("C:/Workspace/Weiterbildung/Deskriptive Statistik/R-Unterrichtsdateien/lightbulbs.txt")
x
t.test(x, mu=10000, alternative = "less")

cookies <- scan("C:/Workspace/Weiterbildung/Deskriptive Statistik/R-Unterrichtsdateien/cookies.txt")
cookies
# mu wird mit 2 angegeben werden, da die Werte vom Hersteller kommt
t.test(cookies, mu=2, alternative = "greater")
# der P-Wert liegt


# Zweiseitiger Test
penguins <- scan("C:/Workspace/Weiterbildung/Deskriptive Statistik/R-Unterrichtsdateien/penguins.txt")
penguins

t.test(penguins, mu= 15.4, alternative = "two.sided")
15.4 - 13.86313

# Linksseitiger Populationstest
# Problem: Die Wahlbeteiligung an den letzten Wahlen betrug 60%. Eine
# telefonische Umfrage ergab, dass 85 von 148 Befragten angaben, an
# den kommenden Wahlen teilzunehmen. Lässt sich die Hypothese,
# dass die kommende Wahlbeteiligung über 60% liegt, bei einem
# Signifikanzniveau von 5% verwerfen?
# p = 60 also 0.6
# n = 148
# x = 85
prop.test(x=85, n=148, p= 0.6, alternative = "less", correct = FALSE)
# correct = FALSE R korrigiert das resultag wenn TRUE angegben ist. 
# Wenn das ganze nachgerechnet werden soll FALSE gesetzt werden damit die das 
# Resultat überüberprüft werden kann.
# Die Nullhypothese wird behalten.

library(MASS)
View(immer)

t.test(immer$Y1, immer$Y2, paired = TRUE)
# paired sagt, dass die beiden werte Y1 und Y2 von einander abhängig sind.
# der p-value ist sehr klein, so wird die Null hypothese verworfen

t.test(x= immer$Y1, y=immer$Y2)

# laden mtcars
View(mtcars)
# mit ~ (tilde) wird eine model gebildet, also abhängigkeiten bilden
# am = Transmission (0 = automatic, 1 = manual)
t.test(mpg ~ am, data = mtcars)
# hier würde die Hypothese verworfen.
# beim freiheitsgrad wird bei einem Wert n-1 angenomme beim mehreren werden 
# irgendwelche werte angenommen.


