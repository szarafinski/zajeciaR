#wyklad 5
prosta <- function(b,a,v,...){ #dodanie trzykropka jako dodatkowy argument powoduje mozliwosc zmiany
  plot(0, xlim=c(-10,10), type="n", ylab="",...) # np wykresu o jego kolor lub grubosc lini itp.
  if (v==FALSE){ #dla wartosci FALSE bedzie rysyowac prosta o parametrach y=a*x + b
  abline(b,a,col="blue")
  } else { #dla wartosci TRUE bedzie rysowac prosta o parametrze x = b
    abline(v=b,col="blue")
  }
}

prosta(5,4,TRUE)
prosta(-0.5,0.1,FALSE)
prosta(0.5,0,FALSE)
prosta(4,2,TRUE, xlab="nazwa dodana jako nowy argument")

narysujNajmniejsze <- function(wektor, ile=3,...){
  posortowane <- sort(wektor)
  plot(posortowane[1:ile],...)
}
narysujNajmniejsze(c(1:3,0,10,-2,2:4), ile = 20, lwd=3, type="b", col="blue")

#apply stosuje funkcje 1 na wierszach 2 na kolumnach
#tapply stosuje funkcje FUN z podzialem przez narzucony wektor. np.  macierz danych i podajemy wektor z podzialem na plec to wypluwa tablice z podzialem na plec

dane <- read.table("miasta.txt", sep="\t", na.strings="?", row.names = 1, head= TRUE)
srednia <- apply(dane, 2, mean, na.rm=TRUE)

srednia2 <- mapply(mean, na.rm=TRUE, dane)
odchylenie <- apply(dane,2, sd, na.rm=TRUE)

odchylenie.wartosci.od.sredniej <- sweep(dane,2, srednia)
standaryzacja.danych <- sweep(odchylenie.wartosci.od.sredniej, 2, FUN="/", odchylenie)
# standaryzacja to (x-srednia)/odchylenie.standardowe


#sprawdŸ informacje o rozk³adach i centralnym twierdzeniu granicznym
