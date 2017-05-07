plot(c(3,2,1,4),t="l",lwd=32)
text(2,1.5,"TEKST")
plot(c(3,2,1,1,4),t="l",lwd=3,col="red",col.lab="blue",font.lab=4)
plot(c(3,2,1,1,4),lwd=30,col="red",col.lab="blue",font.lab=3, pch=14)
# umieszcza sie wykresy w równomiernej siatce o w wierszach i k kolumnach uzupelniajac najpierw wiersze
# par(mfrow=c(w,k)) - uzupelnia najpierw wiersze
# par(mfcol=c(w,k)) - uzupelnia najpierw kolumny
par(mfrow=c(2,4))
plot(c(3,2,1,1,4),t="p",lwd=3,col.lab="blue", main="typ = 'points'")
plot(c(3,2,1,1,4),t="l",lwd=3,col.lab="green", main="typ = 'lines'")
plot(c(3,2,1,1,4),t="b",lwd=3,col.lab="red", main="typ = 'both', kropki i linie")
plot(c(3,2,1,1,4),t="s",lwd=3,col.lab="orange", main="typ = 'schody, steps'")
plot(c(3,2,1,1,4),t="h",lwd=3,col.lab="black", main="typ = 'histogram'")
plot(c(3,2,1,1,4),t="o",lwd=3,col.lab="pink", main="typ = 'overplottet', kropki na liniach")
plot(c(3,2,1,1,4),t="c",lwd=3,col.lab="purple", main="typ = 'linie bez punktow'")
plot(c(3,2,1,1,4),t="S",lwd=3,col.lab="gray", main="typ = 'inne schody'")


par(mfrow=c(1,1)) #uniewaznia par() i powoduje, ze kolejny wykres bedzie na pelnym ekranie
macierz <- matrix(c(4,4,1,2,2,3),2,3, byrow=TRUE)
layout(macierz, widths = c(2,1), heights = c(2,1))
plot(c(3,2,1,1,4),t="h",lwd=3,col="blue", main="typ = 'histogram'")
plot(c(3,2,1,1,4),t="o",lwd=3,col="pink", main="typ = 'overplottet', kropki na liniach")
plot(c(3,2,1,1,4),t="c",lwd=3,col="purple", main="typ = 'linie bez punktow'")
plot(c(3,2,1,1,4),t="S",lwd=3,col="green", main="typ = 'inne schody'")
