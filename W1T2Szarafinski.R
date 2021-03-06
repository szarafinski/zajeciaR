dane <- c(7.33, 4.44, 4.12, 6.54, 4.92, 6.52, 3.86, 6.30, 4.10, 6.66, 4.75, 6.7)

#zadanie 1. Narysowa�K histogram liczebno�ci o jdnostkowej d�ugo�ci przedzia��w klasowych dla tych danych
hist(dane,
     main = paste("Histogram wielko�ci wynagrodzenia w grupie pracownik�w"),
     col = "green")

#zadanie 2. Oblicz kwartyle, w tym median�; obszar zmienno�ci i odchylenie �wiartkowe
median(dane)
kwartyl <- quantile(dane, probs = c(25, 50, 75)/100, names = FALSE) #mediana to kwartyl 50%
#pierwszy kwartyl
kwartyl[1]
#trzeci kwartyl
kwartyl[3]
#obszar zmienno�ci
R <- max(dane) - min(dane)
#odchylenie �wiartkowe
Q <- (kwartyl[3]-kwartyl[1])/2

#zadanie 3. Obliczy� wsp�czynnik zmienno�ci w realacji do mediany i asymeri� w realcji do mediany
V.Q <- Q/median(dane)
A.Q <- (kwartyl[3]-2*median(dane)+kwartyl[1])/(kwartyl[3]-kwartyl[1])

