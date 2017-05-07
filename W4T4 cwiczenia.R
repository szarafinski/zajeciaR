x <- c(1:10,20:35,1000,1001)
y <- c(15:37)
boxplot(x) # generuje wykres wraz z elementami odstaj¹cymi na wykresie
boxplot(x,y, outline=FALSE) #generuje wykres z pominiêciem elementów odstaj¹cych
#x - zmienna typu liczbowego
#y - zmienna mo¿e byæ typu wektor lub faktor. W przypadku tego drugiego to pokaze siê
#    wiêcej wykresów pude³kowych dyla ka¿dej cechy z obiektu factor.
boxplot(x, plot=FALSE)

getwd() #podaje œcie¿kê do katalogu roboczego
#operacje na plikach:

# dir.create(sciezka, showWarnings=T,recursive=F) tworzy katalog
file.create("nazwaPliku")
file.exists("nazwaPliku")
file.rename("nazwaPliku", "nowaNazwaPliku")
file.remove("nowaNazwaPliku")
# file.choose() = wybiera plik nie w katalogu roboczym, ale na dysku 


#wczytywanie tabeli o strukturze ramki danych. 
dane <- read.csv("daneO.csv")
tabela <- read.table("daneO.csv", header=TRUE, sep=",", dec=".", nrows=25, na.strings = "(-)")
names(tabela)[7] <- "Pora¿ki"
head(tabela)
tail(tabela)
# gsub("//.")

dane.tablica <- data.frame(klasa = class(dane[,1]), typ1 = typeof(dane[,1]), typ2 = mode(dane[,1]))
for (i in c(2:ncol(dane))){
  dane.tablica <- rbind(dane.tablica, data.frame(klasa = class(dane[,i]), typ1 = typeof(dane[,i]), typ2 = mode(dane[,i])))
}
row.names(dane.tablica) <- colnames(dane)

levels(dane$Niepowodzenia)

obiektX <- data.frame(id = c(1:10), 
                      plec = rep(c("Kobieta", "Facet"), length.out = 10), 
                      wiek = sample(20:40, 10),
                      ocena = sample(2:5, 10, replace=TRUE, prob=c(0.2,0.4,0.3,0.1)))
write.table(obiektX, file="tabela.csv", sep=";", quote=FALSE)
class(obiektX$plec)
write("Jaka to melodia", file="napis.txt")
tekstowe <- scan("", what=character(),2)
numeryczne <- scan("", what=numeric(), 2)

