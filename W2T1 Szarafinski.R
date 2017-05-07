dane <- c(6.57, 7.6, 3.54,7.34,6.85,3.21,7.27,3.59,3.3,6.56,4.99,4.33)
par(mfrow=c(2,1))

#zadanie 1:
#Narysowaæ histogram liczebnoœci o jednostkowej d³ugoœci przediza³ów klasowych dla danych
hist(dane, col=sample(rainbow(10)), main="Histogram wynagrodzeñ", ylab ="Czêstotliwoœæ")
rug(jitter(dane, factor=2), ticksize = 0.05, side=1)

#zadanie 2 Obliczyæ pierwszy kwartyl, medianê, trzeci kwartyl, obszar zmiennoœci i odchylenie æwiartkowe
Q1 <- quantile(dane, probs = 0.25, type =6)
Q2 <- quantile(dane, probs = 0.5, type =6)
Q3 <- quantile(dane, probs = 0.75, type =6)
mediana <- median(dane)
R <- max(dane)-min(dane)
Q <- 0.5 * (Q3-Q1)

#zad. 3 Obliczyæ wspó³czynnik zmiennoœci w realacji do mediany i asymetriê w relacji do mediany
V.Q <- Q / mediana * 100
A.Q <- (Q3 - 2 * mediana + Q1)/ (Q3-Q1)


#zad. 4 wpisaæ w oznaczone pola odpowiednie wartoœæi
zaokrag <- 2
wektor <- c(round(min(dane), zaokrag), 
            round(Q1,zaokrag),
            round(mediana, zaokrag), 
            round(Q3, zaokrag),
            round(max(dane), zaokrag))

plot(1:5, type="n", axes = FALSE, ann = FALSE)
axis(1, at = 1:5, labels = c("Min", 
                             "Q1",
                            "Me",
                             "Q3",
                             "Max"
)
)
text(1:5,rep(1), labels=wektor, col="black")

# minimalna kwota wynagrodzenia to 3,21 tys. PLN w zbiorze danych
# Kwartyl 1 wynosi 3,55 tys. PLN co oznacza, ¿e 25% obserwacji jest ni¿sza b¹dŸ równa wartoœci I-ego kwartyla, a 75% obserwacji jest równa b¹dŸ wiêksza ni¿ wartoœæ I-ego kwartyla
# Mediana wynosi 5,78 tys. PLN, co oznacza, ¿e 50% obserwacji jest ni¿sza , i 50% obserwacji wy¿sza ni¿ wartoœæ mediany
# Kwartyl 3 wynosi 7,16 tys. PLN co oznacza, ¿e 75% obserwacji jest ni¿sza b¹dŸ równa wartoœci III-ego kwartyla, a 25% obserwacji jest równa b¹dŸ wiêksza ni¿ wartoœæ III-ego kwartyla.
# maksymalna kwota wynagrodzenia w podanych danych to 7,6 tys. PLN
