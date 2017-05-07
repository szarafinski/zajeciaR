dane <- c(1.77, 1.44, 2.32, 3.09, 2.44)
#zadanie 1. Nanieœæ otrzymane dane w postaci punktów na uk³ad wspó³rzêdnych

plot(dane, xlab = "Frima", ylab = "Wartoœæ dochodu",  yaxt = 'n', xaxt = 'n' , ylim = c(0,4))
axis(1, at = 1:5,labels = c("AA", "BB", "CC", "DD", "EE"))
axis(2, at = 1:3,labels = c(1:3))

#zadanie 2. Obliczyæ wartoœæ œredni¹ z próby dla danych
srednia <- mean(dane)

# narysowaæ na wykresie liniê y=œrednia (linia pozioma)
abline(srednia,0)
#zaznaczyæ na wykresie ró¿nicê pomiêdzy wartoœci¹ pomiaru a œredni¹ z próby 
segments(1:5,srednia,1:5,dane)

#zadanie 3. Obliczyæ wariancjê zpróby, odchylenie standardowe z próby, wariancjê dla populacji oraz odchylenie standardowe dla populacji
#odchylenie standardowe z próby
odchylenie <- sd(dane) 
#wariancja z próby
var(dane)
#odchylenie standardowe z populacji
sd(dane) * (length(dane)-1)/length(dane)
#wariancja z populacji
var(dane) * (length(dane)-1)/length(dane)


#zadanie 4. Narysowaæ wykres z odpowiednimi wartoœciami i zinterpretowaæ otrzymany wynik
zaokrag <- 3
wektor <- c(round(srednia - 2* odchylenie, zaokrag), 
            round(srednia- odchylenie,zaokrag),
            round(srednia, zaokrag), 
            round(srednia + odchylenie, zaokrag),
            round(srednia + 2 * odchylenie, zaokrag))
par(ask = TRUE)
plot(1:5, type="n", axes = FALSE, ann = FALSE)
axis(1, at = 1:5, labels = c(expression(bar(x) - 2 * sigma), 
                             expression( bar(x) - sigma),
                             expression(bar(x)),
                             expression(bar(x) + sigma), 
                             expression( bar(x) + 2 * sigma)
                             )
     )
text(1:5,rep(1), labels=wektor)
# Przeciêtna wielkoœæ dochodu w piêciu ma³ych firmach wynosi³a 2 212 PLN. 
# Przeciêtne odchylenie pomiêdzy wartoœci¹ pomiaru a œredni¹ to 638 PLN. Zbiór danych nie jest mocno rozproszony wzglêdiem siebie.
# wartoœæ œredniej pomniejszonej o dwukrotn¹ wartoœæ odchylenia standardowego to 937 PLN
# wartoœæ œredniej pomniejszon¹ o wartoœæ odchylenia standardowego to 1 574 PLN
# wartoœ œredniej powiêkszon¹ o wartoœæ odchylenia standardowego to 2 850 PLN
# wartoœæ œredniej powiêkszona o dwrukotnoœæ wartoœæi odchylenia standardowego to 3 487 PLN

#5 
summary(dane)
