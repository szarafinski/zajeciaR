zadanie1 <- function(x, ...) {
  if (anyNA(x)){
    cat("Dane zawieraja puste elementy, brak mozliwosci wyliczenia")
  } else {
  tabela <- data.frame(
    "Podsumowanie" = c(
      min(x),
      max(x),
      quantile(x,type = 6, names = TRUE, probs = c(0.25, 0.5, 0.75)),
      length(x),
      mean(x),
      sd(x)
    ),
    row.names = c(
      "minimum",
      "maksimum",
      "Q1",
      "mediana",
      "Q3",
      "liczebnoœæ",
      "œrednia",
      "odchylenie standardowe"
    )
  )
  write.csv2(tabela, file = "tabela1.csv",
             quote = FALSE, row.names = TRUE)
  
  
  jpeg(filename = "histogram.jpg", width = 1500, height = 800)
  
  par(mfrow = c(1,2))
  hist(
    x,
    main = paste("Histogram danych zmiennej: ", deparse(substitute(x))),
    xlab = deparse(substitute(x)),
    ylab = "Czêstoœæ",
    label = TRUE,
    col = rainbow(7),
    ...
  )
  rug(x, ticksize = 0.08, col = "black")
  plot(
    density(x),
    main = paste("Wykres gêstoœci dla zmiennej: ", deparse(substitute(x))),
    type = "h",
    ylab = "Gêstoœæ",
    col = "green",
    ...
  )
  par(mfrow = c(1,1))
  dev.off()
  
  jpeg(
    filename = "wykres pudelkowy.jpg", width = 1500, height = 800, res = 100
  )
  
  par(mfrow = c(1,1))
  boxplot(
    x,
    col = "gray",
    main = paste("Wykres pude³kowy danych zmiennej: ", deparse(substitute(x))),
    xlab = deparse(substitute(x)),
    outline = TRUE
  )
  kropki <- jitter(rep(1.3, length(x)), factor = 3)
  points(kropki, x, col = "red", pch = 21, bg = "yellow")
  
  dev.off()
  }
}




zadanie2 <- function(x,y,...) {
  if (anyNA(x)) {
    cat("Dane zawieraja puste elementy, brak mozliwosci wyliczenia")
  } else {
    tabela <- data.frame(
      "Minimum" = tapply(x, y, FUN = "min"),
      "Maksimum" = tapply(x, y, FUN = "max"),
      "Q1" = tapply(x, y, FUN = "quantile", type = 6, names = TRUE, probs = c(1 / 4)),
      "Mediana"  = tapply(x, y, FUN = "median"),
      "Q3" = tapply(x,y,FUN = "quantile", type = 6, names = TRUE, probs = c(3 / 4)),
      "Liczebnosc" = tapply(x, y, FUN = "length"),
      "srednia" = tapply(x, y, FUN = "mean"),
      "odchylenie standardowe" = tapply(x, y, FUN = "sd")
    )
    
    write.csv2(tabela, file = "tabela2.csv",
               quote = FALSE, row.names = TRUE)
    jpeg(
      filename = "wykres pudelkowy2.jpg", width = 1500, height = 800, res = 100
    )
    
    par(mfrow = c(1,1))
    boxplot(
      x ~ y,
      col = "gray",
      main = paste(
        "Wykres pude³kowy danych zmiennej: ",
        deparse(substitute(x)),
        "z uwzglêdnieniem podzia³u w zmiennej: ",
        deparse(substitute(y))
      ),
      xlab = deparse(substitute(x)),
      outlne = TRUE,
      ...
    )
    for (i in 1:length(levels(y))) {
      kropki <-jitter(rep((i + 0.3), length(x[y == levels(y)[i]])), factor = 3)
      points(kropki, x[y == levels(y)[i]], col = "red", pch = 21, bg = "yellow")
    }
    dev.off()
  }
}