jeden<-c(1,2,3,4,5,6,7,8,9,10)
                   dwa <- c(2,3,7,10,11)
                   trzy <-c (1,2,3,6,8,10,12,15)

roznica.symetryczna <- function ( ...){
  lista <- list( ...)
  wynik <- c()
  robocza <- c()
  for (i in 1:length(lista)){
    for (v in lista[i]){
      robocza <- c(robocza, v)
    }
  }
  
  robocza <- sort(robocza)
  indeksy <- c(robocza[1])
  temp <- robocza[1]
  for (i in 2:length(robocza)){
    if (robocza[i] > temp){
      indeksy <- c(indeksy, robocza[i])
      temp <- robocza[i]
    }
  }
  
  for (i in 1:length(indeksy)){
    licznik <- 0
    for (j in 1:length(robocza)){
      if (robocza[j] == indeksy[i]){
        licznik <- licznik + 1
      }
    }
    if (licznik %% 2 == 1){
      wynik <- c(wynik,indeksy[i])
    }
  }  
  return (wynik)
}