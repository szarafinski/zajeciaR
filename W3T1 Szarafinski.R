# Zadanie 1. Skonstruuj wektor kwadrat�w liczb od 1 do 50. Nast�pnie u�ywaj�c operatora
# dzielenia modulo i funkcji factor() zlicz, kt�re cyfry i jak czesto wystepuj� na pozycji jedno�ci
# w wyznaczonych kwadratach.
wektor <- c(1:50)
wektor <- factor(wektor^2 %% 10, levels = c(0:9))
tablica <- table(wektor)
data.frame(Cyfra=names(tablica),Ilo��=as.vector(tablica))

# Zadanie 2. Skonstuuj tablice trygonometryczne.
wektor <- c(0:90)
wektor_radianow <-  pi/180*wektor
sinus <- round(sin(wektor_radianow), digits = 4)
cosinus <- round(cos(wektor_radianow), digits = 4)
tangens <- format(round(tan(wektor_radianow), digits = 4), scientific=FALSE)
cotangens <- format(round(1/tan(wektor_radianow), digits = 4), scientific=FALSE)
print(data.frame(k�t = wektor, SIN = sinus, COS = cosinus, TG = tangens, CTG = cotangens), row.names = FALSE)

#Zadanie 3. Przygotuj wektor 30 �a�cuch�w znak�w nast�puj�cej postaci: liczba.litera, gdzie
# liczby to kolejne liczby od 1 do 30, a litera to trzy du�e litery A,B,C wyst�puj�ce cyklicznie.
lancuch <- paste(c(1:30), ".", rep(LETTERS[1:3], length.out=30), sep="")

# Zadanie 4. Wczytaj zbi�r danych daneO.csv i napisz p�tl� sprawdzaj�c� typ i klas� ka�dej
# kolumny.
dane <- read.csv("daneO.csv",header=TRUE, sep= ",")

dane.tablica <- data.frame(klasa = class(dane[,1]), typ1 = typeof(dane[,1]), typ2 = mode(dane[,1]))
for (i in c(2:ncol(dane))){
  dane.tablica <- rbind(dane.tablica, data.frame(klasa = class(dane[,i]), typ1 = typeof(dane[,i]), typ2 = mode(dane[,i])))
}
row.names(dane.tablica) <- colnames(dane)

#klasa_i_typ = function(dane){
#  wynik <- cbind(klasa = sapply(dane, class), typ1 =  sapply(dane, typeof), typ2 = sapply(dane, mode))
#  return(wynik)
#}
#klasa_i_typ(dane)

# Zadanie 5. Z ramki daneO wy�wietl tylko dane z wierszy o parzystych indeksach.
parzyste <- rep(c(1:2), length.out = nrow(dane))
dane.parzyste <- dane[parzyste==2,]

# Zadanie 6. U�ywaj�c operator�w logicznych, wy�wietl ze zbioru olimpiad.csv tylko osoby z
# III LO im. Marynarki Wojennej.
olimpiada <- read.csv("olimpiad.csv",header=TRUE, sep= ";")
#olimpiada.IIILO <- olimpiada[grep("Woj", olimpiada$szko�.a),]
sprawdz <- function(tekst){
  tekst.do.sprawdzenia = "Mar.Woj"
  znaleziono = FALSE 
  for(i in c(0:nchar(tekst))) {
    odpowiednik = FALSE
    for(j in c(0:nchar(tekst.do.sprawdzenia))){
      odpowiednik = FALSE;
        for (pozycja in c(0:nchar(tekst.do.sprawdzenia)-1)){
          if (substr(tekst, i, i + pozycja) == substr(tekst.do.sprawdzenia, j, j + pozycja)) {
            odpowiednik = TRUE
          } else {
            odpowiednik = FALSE
            #break;
          }
        }
        if (odpowiednik == TRUE) {
          znaleziono = TRUE
        }
      }
  } 
  
return(znaleziono)
}
znaleziono <- sprawdz(as.character(olimpiada$szko�.a[1]))
for(wiersz in c(2:nrow(olimpiada)))
  znaleziono <- rbind(znaleziono, sprawdz(as.character(olimpiada$szko�.a[wiersz])))

olimpiada.IIILO <- olimpiada[znaleziono==TRUE, ]


# Zadanie 7. Wczytaj dane olimpiad.csv. Utw�rz wektor nazw. Policz ile jest nazw. Policz
# �redni� wieku. - czy chodzi tutaj o nazwy kolumn? czy nazwy czego?
nazwy <- colnames(olimpiada)
liczba.nazw <- ncol(olimpiada)
srednia.wieku <- mean(strtoi(substr(olimpiada$wiek, 1, 2)))
