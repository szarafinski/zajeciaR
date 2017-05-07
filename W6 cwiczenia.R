daneSoc <- read.csv("http://www.biecek.pl/R/dane/daneSoc.csv", sep= ";")
attach(daneSoc) # powoduje ze nazwy kolumny z daneSoc bedzie mozna przywolac bez pisania
# daneSoc$nazwaKolumny

#srednia wazona
# weighted.mean()

#maciesz kowariancji
kowariancja <- cov(cisnienie.rozkurczowe, cisnienie.skurczowe)
# korelacja jest to kowariancja skorygowana o odchylenie standardowe (przedzial od -1 do 1)
korelacja <- cor(cisnienie.rozkurczowe, cisnienie.skurczowe)

# medianowe odchylenie bezwzgledne, czyli jak bardzo przecietnie jednostki sa oddalone od mediany 
mad(cisnienie.rozkurczowe)
median(cisnienie.rozkurczowe)

#install.packages("e1071")
#install.packeges("dprep")
#install.packeges("psych")
#install.packeges("dprep")

library("e1071")
library("dprep")
library("psych")
library("dprep")

# kurtoza, miara koncentracji
e1071::kurtosis(cisnienie.rozkurczowe)

#skosnosc miara asymetri
e1071::skewness(wiek)
#srednia geometryczna lub harmoniczna
psych::harmonic.mean(wiek)
psych::geometric.mean(wiek)
#moda, domintanta w probie
dprep::moda(wiek)

#do wyznaczania liczebnosci wystepowania kombinacji poziomow zmiennych jakosciowych uzywamy 
table(wyksztalcenie,praca)

#funkcja gestosci, wyznacza gestosc w danym punkcie na podstawie koncentracji obserwacji 
#w okolicy danego punktu
density(wiek, bw="nrd0", adjust = 1, n=512, kernel = "gaussian")

par(mfrow=c(2,2))
hist(faithful[,1],xlab="czas trwania erupcji", main="Old Faithul Geyser Data", label=TRUE)
#dodawanie pionowych kresek i definiowanie rozmiaru kresek oraz jego koloru
rug(faithful[,1], ticksize=0.08, col= "blue")
hist(faithful[,2], xlab = "czas oczekiwania na erupcj", main="Old Faithful Geyser Data", 
     label=TRUE)
rug(faithful[,2], ticksize=0.08, col="blue")

density(faithful[,1])
plot(density(faithful[,1]), main= "gêstoœæ dla faithful[,1]")
density(faithful[,2])
plot(density(faithful[,2]), main= "gêstoœæ dla faithful[,2]")

# savePlot("nazwa")
par(mfrow=c(2,2))
hist(cisnienie.rozkurczowe,xlab="wartoœci ciœnienia rozkurczowego", 
     main="Ciœnienie rozkurczowe", label=TRUE)
#dodawanie pionowych kresek i definiowanie rozmiaru kresek oraz jego koloru
rug(cisnienie.rozkurczowe, ticksize=0.08, col= "blue")
hist(cisnienie.skurczowe, xlab = "ciœnienie skurczowe", 
     main="ciœnienie skurczowe", label=TRUE)
rug(cisnienie.skurczowe, ticksize=0.08, col="blue")

density(cisnienie.rozkurczowe)
plot(density(cisnienie.rozkurczowe), main= "gêstoœæ dla cisnienia rozkurczowego")
density(cisnienie.skurczowe)
plot(density(cisnienie.skurczowe), main= "gêstoœæ dla cisnienia skurczowego")


#rysowanie dystrybuanty dla wieku
par(mfrow = c(1,1))
plot(ecdf(wiek))

#rysowanie boxplot dla wyksztalcenia podzial na klasy
par(mfrow = c(2,1))
boxplot(wiek~wyksztalcenie, col="gray")
boxplot(wiek, col="gray")
kropki <- jitter(rep(1.3, length(wiek)), factor = 3)
points(kropki, wiek, col="red", pch=21)

#wykresy skrzypcowe
#install.packages("vioplot")
library("vioplot")
par(mfrow= c(1,1))
vioplot(wiek[plec=="mezczyzna"], wiek[plec=="kobieta"], wiek,
             names=c("wiek Mez.", "wiek Kob.", "wiek Ali"))
#install.packages("car")
library("car")
# nie da sie rysowac tego wykresu na jednym oknie, nie dziala par(mfrow = c(2,2))
scatterplot(cisnienie.rozkurczowe, cisnienie.skurczowe, groups=plec)
scatterplot(cisnienie.rozkurczowe, cisnienie.skurczowe, groups=plec,
            smooth=FALSE, lwd =3, pch=c(20,21), cex = 1.5)
scatterplot(cisnienie.rozkurczowe, cisnienie.skurczowe, groups=plec,
            reg.line = FALSE)
scatterplot(cisnienie.rozkurczowe, cisnienie.skurczowe, groups=wyksztalcenie,
            reg.line = FALSE, smooth = FALSE)

#wykres mozaikowy to odpowiednik wykresu rozrzutu dla zmiennych typu wyliczeniowego.
#sluzy do graficznego przedstawianai tablic wielodzielczych
mosaicplot(~praca + wyksztalcenie+plec, data = daneSoc,
           main="Wykres mozaikowy", col = c("lightblue","lightgreen"))
mosaicplot(table(praca, wyksztalcenie, plec), main = "Wykres mozaikowy 2",
           col = c("lightblue", "lightgreen"))

# zadanie domowe
# napisz funkcjê: 
#1. jednoargumentow¹, która tworzy i zapisóuje tabele do pliku, w której znajduj¹ siê:
# N - wielkosc próby wektora X
# min, Q1, mediana, Q3, max, srednia i odchylenie standardowe oraz
# tworzy plik (png lub jpeg) na którym bêdzie histogram, gêstosc oraz drugi plik z boxplot
#2. dwuargumentowa funkcja która przyjmuje argumenty: X (zmienna liczbowa) i Y (zmienna kategoryzujaca) 
# funkcja wykonywaæ ma wszystko jak wy¿ej tylko z podzialem na grupy okreœlone przez wektor Y
#
# jako argument mozna podaæ te¿ nazwe pliku do jakiego ma byc zapisane ale to nie jest konieczne