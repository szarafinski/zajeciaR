dane <- c(1.77, 1.44, 2.32, 3.09, 2.44)
#zadanie 1. Nanie�� otrzymane dane w postaci punkt�w na uk�ad wsp�rz�dnych

plot(dane, xlab = "Frima", ylab = "Warto�� dochodu",  yaxt = 'n', xaxt = 'n' , ylim = c(0,4))
axis(1, at = 1:5,labels = c("AA", "BB", "CC", "DD", "EE"))
axis(2, at = 1:3,labels = c(1:3))

#zadanie 2. Obliczy� warto�� �redni� z pr�by dla danych
srednia <- mean(dane)

# narysowa� na wykresie lini� y=�rednia (linia pozioma)
abline(srednia,0)
#zaznaczy� na wykresie r�nic� pomi�dzy warto�ci� pomiaru a �redni� z pr�by 
segments(1:5,srednia,1:5,dane)

#zadanie 3. Obliczy� wariancj� zpr�by, odchylenie standardowe z pr�by, wariancj� dla populacji oraz odchylenie standardowe dla populacji
#odchylenie standardowe z pr�by
odchylenie <- sd(dane) 
#wariancja z pr�by
var(dane)
#odchylenie standardowe z populacji
sd(dane) * (length(dane)-1)/length(dane)
#wariancja z populacji
var(dane) * (length(dane)-1)/length(dane)


#zadanie 4. Narysowa� wykres z odpowiednimi warto�ciami i zinterpretowa� otrzymany wynik
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
# Przeci�tna wielko�� dochodu w pi�ciu ma�ych firmach wynosi�a 2 212 PLN. 
# Przeci�tne odchylenie pomi�dzy warto�ci� pomiaru a �redni� to 638 PLN. Zbi�r danych nie jest mocno rozproszony wzgl�diem siebie.
# warto�� �redniej pomniejszonej o dwukrotn� warto�� odchylenia standardowego to 937 PLN
# warto�� �redniej pomniejszon� o warto�� odchylenia standardowego to 1 574 PLN
# warto� �redniej powi�kszon� o warto�� odchylenia standardowego to 2 850 PLN
# warto�� �redniej powi�kszona o dwrukotno�� warto��i odchylenia standardowego to 3 487 PLN

#5 
summary(dane)
