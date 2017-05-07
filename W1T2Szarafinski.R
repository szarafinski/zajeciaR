dane <- c(7.33, 4.44, 4.12, 6.54, 4.92, 6.52, 3.86, 6.30, 4.10, 6.66, 4.75, 6.7)

#zadanie 1. NarysowaæK histogram liczebnoœci o jdnostkowej d³ugoœci przedzia³ów klasowych dla tych danych
hist(dane,
     main = paste("Histogram wielkoœci wynagrodzenia w grupie pracowników"),
     col = "green")

#zadanie 2. Oblicz kwartyle, w tym medianê; obszar zmiennoœci i odchylenie æwiartkowe
median(dane)
kwartyl <- quantile(dane, probs = c(25, 50, 75)/100, names = FALSE) #mediana to kwartyl 50%
#pierwszy kwartyl
kwartyl[1]
#trzeci kwartyl
kwartyl[3]
#obszar zmiennoœci
R <- max(dane) - min(dane)
#odchylenie æwiartkowe
Q <- (kwartyl[3]-kwartyl[1])/2

#zadanie 3. Obliczyæ wspó³czynnik zmiennoœci w realacji do mediany i asymeriê w realcji do mediany
V.Q <- Q/median(dane)
A.Q <- (kwartyl[3]-2*median(dane)+kwartyl[1])/(kwartyl[3]-kwartyl[1])

