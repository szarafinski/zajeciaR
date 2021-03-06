# czyszczenie WorkSpace
rm(list = ls())

# zadanie 1 
# Wybieramy katalog. Nale�y z katalogu odczyta� wszystkie pliki z rozszerzeniem 
# csv. Tym z nich, kt�re zawieraj� wi�cej ni� 1000 wierszy zmieniamy nazw� dodaj�c
# na pocz�tku nazwy pliku �1000+�.

zadanie1 <- function() {
  cat("Twoj obecny folder roboczy to: ", getwd(),"\n")
  cat("By go zmienic na inny, wybrany przez siebie wpisz ZMIEN, 
      w przeciwnym wypadku naci�nij ENTER")
  zmien <- readline()
  if (toupper(zmien) == "ZMIEN") {
    cat("Podaj sciezke do wybranego folderu:")
    setwd(readline())
  }
  pliki <- list.files(pattern = ".csv")
  for (i in pliki){
    if (nrow(read.csv(i)) > 1000){
      file.rename(i,paste0("1000+",i))
    }
  }
}

# zadanie 2
# Nale�y wczyta� z klawiatury pewien ci�g liczb. Nast�pnie nale�y przeprowadzi�
# 10000 losowa� pi�ciu z tych liczb (dopuszczalne powt�rzenia), dla ka�dego losowania 
# obliczy� �redni� i zapami�ta� j�. Po wszystkim nale�y wy�wietli� histogram
# uzyskanych �rednich.

zadanie2 <- function() {
  cat("Podaj ciag liczb naturalnych. Kazda liczbe oddziel spacja \n")
  liczby <- as.integer(strsplit(readline(), " ")[[1]])
  srednie <- c()
  for (i in c(1:10000)){
    losowanie <- sample(liczby,5, replace = TRUE)
    srednie <- c(srednie, mean(losowanie))
    #cat(losowanie, "\n")
  }
  hist(srednie,
       main = "histogram srednich wartosci 10 000 losowan liczb z zadanego zbioru przez uzytkownika", 
       col = rainbow(5),
       ylab = "czestotliwosc")
}

# zadanie 3
# Nale�y wczyta� z klawiatury ci�g 10 liczb i wygenerowa� wszystkie mo�liwe ci�gi
# 4-elementowe z�o�one z wczytanych liczb (dopuszczalne powt�rzenia). Dla ka�dego
# ci�gu nale�y obliczy� �redni� i zapami�ta� j� a nast�pnie �rednie te nale�y pokaza�
# na histogramie.

zadanie3 <- function() {
  cat("Podaj ciag 10 liczb naturalnych. Kazda liczbe oddziel spacja \n")
  liczby <- as.integer(strsplit(readline(), " ")[[1]])
  if (length(liczby) == 10){
    srednie <- c()
    kombinacje <- expand.grid(rep(list(liczby), 4)) 
    for (i in c(1:10000)){
      suma <- 0 
      for (j in c(1:4)){
        suma <- suma + kombinacje[i,j]
      }
      srednie <- c(srednie, suma / 4)
    }
    hist(srednie,
         main = "histogram srednich wartosci permutacji z powtorzeniami zbioru liczb uzytkownika", 
         col = rainbow(5),
         ylab = "czestotliwosc")
    
    
  } else {
    cat("podano za duzo lub za malo liczb")
  }
}

# zadanie 5 
# Wylosowa� 11 warto�ci z rozk�adu normalnego N(0, 1) i zapami�ta� je w wektorze
# W . Wykona� 10000 razy eksperyment polegaj�cy na wylosowaniu 11-elementowego
# ci�gu o warto�ciach z wektora W (powt�rzenia dopuszczalne). Dla ka�dego losowania
# nale�y wyliczy� jego median�. Zapami�tane mediany nale�y zaprezentowa�
# na histogramie.

zadanie4 <- function() {
  mediana <- c()
  for (i in c(1:10000)){
    W <- rnorm(11)
    mediana <- c(mediana, median(W))
  }
  hist(mediana,
       main = "histogram mediany 11 wylosowanych liczb ze zbioru cechujacego sie rozkladem normalnym", 
       col = rainbow(5),
       ylab = "czestotliwosc")
}

# zadanie 7
# W bibliotece MASS znajduje si� zmienna cats. Poszczeg�lne kolumny tej zmiennej
# zawieraj� informacje: o p�ci (F,M), wadze (w kg) oraz wadze serca (w g). Populacja
# kot�w zosta�a podzielona na dwie grupy przez median� z wagi ich serc. Por�wna�
# wsp�czynniki regresji liniowej (a, b) dla obu podpopulacji kot�w.

zadanie5 <- function(){
  library(MASS)
  grupa1 <- c()
  grupa2 <- c()
  mediana <- median(cats$Hwt)
  for (i in c(1:length(cats$Hwt))){
    if (cats$Hwt[i]<= mediana){
      grupa1 <- c(grupa1, cats$Hwt[i])
    } else {
      grupa2 <- c(grupa2, cats$Hwt[i])
    }
  }
  regresja1 <- lm(grupa1~c(1:length(grupa1)))
  regresja2 <- lm(grupa2~c(1:length(grupa2)))
  
  par(mfrow = c(1,2))
  plot(grupa1,
       main = "Populacja kotow z waga serca mniejsza lub rowna medianie",
       col = "blue",
       xlab = "indeks",
       pch = 21,
       bg = "gray60")
  abline(reg = regresja1)
  
  plot(grupa2,
       main = "Populacja kotow z waga serca wieksza niz mediania",
       col = "red",
       xlab = "indeks",
       pch = 24,
       bg = "gray60")
  abline(reg = regresja2)
  par(mfrow = c(1,1))
  
  wynik <- data.frame(a = c(regresja1$coefficients[2],regresja2$coefficients[2]),
                      b = c(regresja1$coefficients[1],regresja2$coefficients[1]),
                      wspolczynnik.R.kwadrat = c(summary(regresja1)$r.squared,summary(regresja2)$r.squared),
                      row.names = c("model1", "model2"))
  return(wynik)
}

# zadanie 8 
# Wykorzystuj�c dane ze zmiennej anscombe zademonstrowa� obliczanie parametr�w regresji liniowej.
zadanie6 <- function(){
  dane <- anscombe
  cat("1. mamy nastepujace dane \n")
  print(dane)
  cat("\n2. obliczamy srednia wartosc dla kazdej kolumny \n")
  srednie <- apply(dane,2,mean)
  print(srednie)
  cat("\n3. obliczamy roznice pomiedzy wartoscia a srednia z danej kolumny \n")
  roznice <- sweep(dane,2,srednie)
  print(roznice)
  cat("\n4. obliczamy kwadart roznicy wartosci a sredniej dla danej kolumny. Tylko dla zmiennych X \n")
  iksy <- apply(roznice[,1:4],2,function(x) x^2)
  print(iksy)
  cat("\n Suma dla kazdej kolumny to: \n")
  suma.kwadratow <- apply(iksy,2,sum)
  print(suma.kwadratow)
  cat("\n5. obliczamy iloczyn roznicy wartosci Xi od sredniej wartosci Xi\n
      oraz roznicy wartosci Yi od sredniej wartosci Yi\n
      Otrzymujemy cztery kolumny: \n")
  iloczyn <- roznice[,1:4] * roznice[,5:8]
  colnames(iloczyn) <- c("X1*Y1", "X2*Y2", "X3*Y3", "X4*Y4")
  print(iloczyn)
  cat("\n Suma dla kazdej kolumny to: \n")
  suma.iloczynow <- apply(iloczyn,2,sum)
  print(suma.iloczynow)
  cat("\n6. obliczamy parametr B dla kazdej zmiennej X\n")
  parametr.b <- suma.iloczynow / suma.kwadratow
  names(parametr.b) <- c("B dla X1", "B dla X2", "B dla X3", "B dla X4")
  print(parametr.b)
  cat("\n7. obliczamy wyraz wolny\n")
  wyraz.a <- srednie[5:8] - parametr.b * srednie[1:4]
  names(wyraz.a) <- c("A dla X1", "A dla X2", "A dla X3", "A dla X4")
  print(wyraz.a)
  cat("\n podsumowanie: \n")
  podsumowanie <- data.frame(A = wyraz.a, B = parametr.b, row.names = c("model 1: ", "model 2:", "model 3:", "model 4:"))
  print(podsumowanie)
}

# zadanie 9
# Wygenerowa� funkcj�, kt�ra generuje trajektori� procesu b��dzenia losowego na
# prostej Sn = Pn k=1 Xk, gdzie zmienne Xk maj� rozk�ad normalny N(0, 1). 
# Parametrem wywo�ania funkcji powinna by� liczba krok�w n. Narysowa� wykres czterech
# przyk�adowych trajektorii. Wygenerowa� 1000 losowych trajektorii i dla ka�dego
# z krok�w procesu (k = 1, 2, ..., n) policzy� warto�� �redni�.
zadanie7 <- function(kroki = 100){
  kolory <- c("red", "green", "blue", "black")
  par(mfrow = c(2,2))
  for (s in c(1:4)){
    Ki <- 0
    for (i in c(1:kroki)){
      Ki <- c(Ki, rnorm(1))
    }
    plot(Ki, type = "l", col = kolory[s], ylab = paste0("K",s))
  }
  mtext("Przykladowe trajektorie bladzenia losowego", side = 3, line = -2, outer = TRUE)
  par(mfrow = c(1,1))
  
  # generowanie 1000 trajektorii
  srednie <- c()
  for (s in c(1:1000)){
    Ki <- 0
    for (i in c(1:kroki)){
      Ki <- c(Ki, rnorm(1))
    }
    srednie <- rbind(srednie, Ki)
  }
  srednie <- srednie[,-1]
  return(list(srednie.dla.danego.kroku = apply(srednie,2,mean)))
}

# zadanie 11
# Symulacja ruiny gracza w ruletk�. Zak�adamy, �e gracz obstawia jedynie czarne
# lub czerwone i stawka (oraz wygrana) maj� warto�� 1. Na wej�ciu mamy kapita�
# K gracza (b�d�cy liczb� ca�kowit�). Symulacja ko�czy si� w sytuacji gdy gracz
# przegra wszystko lub osi�gnie podany cel Kmax. Cel domy�lnie r�wny jest 2K.
# Przy pomocy zbudowanej funkcji zademonstrowa� przyk�adow� trajektori� oraz
# wykona� 1000 symulacji pokazuj�c liczb� pora�ek oraz liczb� zwyci�stw wraz ze
# �rednim czasem do pora�ki lub wygranej.

zadanie8 <- function(kasa = 20, Kmax = kasa * 2) {
  p.wygranej <-
    18 / 37 #18 pol jest czerwonych lub czarnych co daje 36 pol lacznie, plus pole "0" bez koloru
  stawka <- 1
  cel <- Kmax
  
  ile.krokow.wygrana <- c()
  ile.krokow.przegrana <- c()
  for (i in c(1:1000)) {
  wygrana <- 0
  przegrana <- 0
  krok <- 0
  kapital <- kasa
    while (kapital > 0) {
      krok <- krok + 1
      if (runif(1) >= p.wygranej) {
          kapital <- kapital + stawka
        } else {
          kapital <- kapital - stawka
        }
      
      if (kapital == cel) {
        wygrana <- wygrana + 1
        ile.krokow.wygrana <- c(ile.krokow.wygrana, krok)
        break
      }
    }
    if (kapital < 1 ){  
    przegrana <- przegrana + 1
    ile.krokow.przegrana <- c(ile.krokow.przegrana, krok)
    }
  }
  return(list(przegrana = ile.krokow.przegrana, 
              wygrana = ile.krokow.wygrana,
              statystyki.przegranej = data.frame(sr.liczba.krokow = mean(ile.krokow.przegrana),
                                               minimalna.liczba.krokow = min(ile.krokow.przegrana),
                                               maksymalna.liczba.krokow = max(ile.krokow.przegrana),
                                               mediana.krokow = median(ile.krokow.przegrana)),
              statystyki.wygranej = data.frame(sr.liczba.krokow = mean(ile.krokow.wygrana),
                                                 minimalna.liczba.krokow = min(ile.krokow.wygrana),
                                                 maksymalna.liczba.krokow = max(ile.krokow.wygrana),
                                               mediana.krokow = median(ile.krokow.wygrana)),
              liczba.porazek = length(ile.krokow.przegrana),
              liczba.zwyciestw = length(ile.krokow.wygrana)))
}
