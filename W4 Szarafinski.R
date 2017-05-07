# zadanie 1
# Wczytaj dane z pliku "olimpiad.csv" i utworz z pliku zmienna "olimpiada".
olimpiada <- read.csv("olimpiad.csv", sep=";", head=TRUE, encoding="UTF-8") #kodowanie

#zadanie 2
# Z nazw zmiennej "olimpiada" usun kropki, a nastepnie zastap nazwy zmiennych
# X1,..., X6 na Zad. 1, ... , Zad. 6
names(olimpiada) <- gsub("\\.", " ", names(olimpiada))
for (i in c(1:6)){
  names(olimpiada)[i+1] <- paste("Zad. ",i)
}


#zadanie 3
#utworz nowa kolumne, w ktorej znajdzie sie suma uzyskanych punktow za wszystkich zadan.
# nazwij ja "suma uzyskanych punktow"

#dodanie nowej kolumny o nazwie suma_uzyskanych_punktow
#olimpiada <- data.frame(olimpiada, suma_uzyskanych_punktow = 0)
olimpiada$suma_uzyskanych_punktow = 0 
# zamiana wszystkich wartosci NA w kolumnach od X1:X6 na wartosc 0
for (i in c(1:6)){
  olimpiada[is.na(olimpiada[,i+1]),i+1] <- 0
}
# wyliczenie sumy uzyskanych punktow dla danego wiersza oraz zapisanie tej wartosci do ostatniej kolumny
for (i in c(1:6)){
  olimpiada[,ncol(olimpiada)] = olimpiada[,ncol(olimpiada)]+olimpiada[,i+1]
}

#zadanie 4
#Kolumna wiek jest zmienna typu factor. Zamien ja na zmienna numeryczna.
olimpiada$wiek <- gsub("-latek","",olimpiada$wiek)
olimpiada$wiek <- as.numeric(olimpiada$wiek)

#zadanie 5
# oblicz srednia, minimum, maksimum, i trzy kwartyle sumy uzyskanych punktow
srednia_punktow <- mean(olimpiada$suma_uzyskanych_punktow)
minimum_punktow <- min(olimpiada$suma_uzyskanych_punktow)
maksimum_punktow <- max(olimpiada$suma_uzyskanych_punktow)
kwartyl_pierwszy <- quantile(olimpiada$suma_uzyskanych_punktow, type = 6, prob = 0.25)
kwartyl_drugi <- quantile(olimpiada$suma_uzyskanych_punktow, type = 6, prob = 0.5)
mediana <- median(olimpiada$suma_uzyskanych_punktow)
kwartyl_trzeci <- quantile(olimpiada$suma_uzyskanych_punktow, type = 6, prob = 0.75)
tabela <- data.frame("wartosci dla zmiennej suma_uzyskanych_punktow" = c(minimum_punktow, 
                                                                         kwartyl_pierwszy, 
                                                                         kwartyl_drugi, 
                                                                         kwartyl_trzeci, 
                                                                         maksimum_punktow),
                     row.names = c("minimum", "Q1", "Q2-mediana", "Q3", "maksimum"))

#zadanie 6
# przedstaw wykres zaleznosci "sumy uzyskanych punktow" w zaleznosci od:
# a) klasy
#metoda imputacji brakow danych - zastapienie pustych komorek losowa wartoscia
olimpiada$klasa[olimpiada[,10]==""] = olimpiada$klasa[sample(1:length(olimpiada$klasa[olimpiada[,10]!=""]),3)]

boxplot(olimpiada$suma_uzyskanych_punktow ~ olimpiada$klasa, 
        col="green", 
        main="Wykres pudelkowy sumy uzyskanych punktow z podzialem klase",
        xlab = "Klasa",
        ylab = "suma punktow")
# b) szkoly
levels(olimpiada$szko³a) <- sub("^III LO im.*", "III LO im. Mar.Woj RP", levels(olimpiada$szko³a))
boxplot(olimpiada$suma_uzyskanych_punktow ~ olimpiada$szko³a)
# c) miasta
boxplot(olimpiada$suma_uzyskanych_punktow ~ olimpiada$miasto)

