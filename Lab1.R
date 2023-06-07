# biblioteki
library(GA)
library(globalOptTests)
library(plot3D)
library(ggplot2)
# wykresy funkcji Ackleya
draw_ackley <- function()
{
  ackley <- function(x, y) 
  {
    -20*exp(-0.2*sqrt(0.5*(x^2 + y^2))) - exp(0.5*(cos(2*pi*x) + cos(2*pi*y))) + exp(1) + 20
  }
  x <- y <- seq(-3, 3, by = 0.1)
  z <- outer(x, y, ackley)
  persp3D(x,y,z, theta = 50, phi = 20,ticktype = "detailed", main='Wykres 3D Funkcji Ackleya')
  points3D(0,0,0, add=TRUE, col='black', bg='white',pch=23, cex=1)
  filled.contour(x, y, z, main='Wykres temperaturowy funkcji Ackleya', 
                  plot.axes = {axis(1);axis(2);points(0,0,col='black', 
                  bg='white',pch=23, cex=1)})
}

# wykresy funkcji Rastrigina
draw_rastrigin <-function()
{
  Rastrigin <- function(x, y)
  {
  20 + x^2 + y^2 - 10*(cos(2*pi*x) + cos(2*pi*y))
  }
  x <- y <- seq(-3, 3, by = 0.1)
  z <- outer(x, y, Rastrigin)
  persp3D(x, y, z, theta = 50, phi = -20,ticktype = "detailed", main='Wykres 3D funkcji Rastrigina')
  points3D(0,0,0, add=TRUE, col='black', bg='white',pch=23, cex=1)
  filled.contour(x, y, z, main='Wykres temperaturowy funkcji Rastrigina',
                  plot.axes = {axis(1);axis(2);points(0,0,col='black', 
                  bg='white',pch=23, cex=1)})
}

#wykonanie algorytmu genetycznego dla funkcji Ackleya
ga_ackley <- function(){
  bounds_a <- getDefaultBounds("Ackleys")
  ackley_test <- function(x) {-goTest(par = x, fnName='Ackleys')}
  smallest_iter <- 1000
  list_fitnes <- list()
  list_mean <- list()
  list_median <- list()
  for(x in 1:10){
  GA1 <- ga(type = "real-valued",
            fitness = ackley_test, lower = c(bounds_a$lower) ,
            upper = c(bounds_a$upper), popSize = 1000, maxiter = 5, run = 5)
  list_fitnes[[x]] <- c(GA1@summary[1:GA1@iter])
  list_mean[[x]] <- c(GA1@summary[1:GA1@iter, 2])
  list_median[[x]] <- c(GA1@summary[1:GA1@iter, 4])
  }
  # Ucięcie wektorów do długości najkrótszego
  min_length <- min(sapply(list_fitnes, length))
  list_fitnes_cut <- lapply(list_fitnes, function(x) x[1:min_length])
  list_mean_cut <- lapply(list_mean, function(x) x[1:min_length])
  list_median_cut <- lapply(list_median, function(x) x[1:min_length])
  
  # Konwersja do data frame
  df_fitnes <- data.frame(list_fitnes_cut)
  df_mean <- data.frame(list_mean_cut)
  df_median <- data.frame(list_median_cut)
  
  # Obliczenie średniej dla każdej kolumny
  fitnes_means <- rowMeans(df_fitnes)
  mean_means <- rowMeans(df_mean)
  median_means <-rowMeans(df_median)
  
  # Wykres dla wyników
  df_mean_values <- data.frame(
    "Generation" = 1:length(fitnes_means),
    "Best" = fitnes_means,
    "Mean" = mean_means,
    "Median" = median_means
  )
  ggplot(df_mean_values, aes(x = Generation)) +
    geom_line(aes(y = Best, color = "Best"), linetype = "solid",linewidth=1.5) +
    geom_line(aes(y = Mean, color = "Mean"), linetype = "dotted", linewidth=1.5) +
    geom_line(aes(y= Median, color = "Median"), linetype = "dotted",linewidth=1.5) +
    labs(title = "Średnie wartości funkcji fitness dla Ackleya", x = "Pokolenie", y = "Wartość funkcji fitnes", color="Legenda") +
    scale_color_manual(values = c("Best" = "blue", "Mean" = "red", "Median" = "green"))
  }

#wykonanie algorytmu genetycznego dla funkcji Rastrigina
ga_rastrigin <-function(){
  bounds_r <- getDefaultBounds("Rastrigin")
  rastrigin_test <- function(x){-goTest(par = x, fnName="Rastrigin")}
  smallest_iter <- 1000
  list_fitnes <- list()
  list_mean <- list()
  list_median <- list()
  for (x in 1:10) {
    GA2 <- ga(type = "real-valued",
              fitness = rastrigin_test, lower = c(bounds_r$lower),
              upper = c(bounds_r$upper), popSize = 1000, maxiter = 5, run=5)
    
    list_fitnes[[x]] <- c(GA2@summary[1:GA2@iter])
    list_mean[[x]] <- c(GA2@summary[1:GA2@iter, 2])
    list_median[[x]] <- c(GA2@summary[1:GA2@iter, 4])
  }
  
  # Ucięcie wektorów do długości najkrótszego
  min_length <- min(sapply(list_fitnes, length))
  list_fitnes_cut <- lapply(list_fitnes, function(x) x[1:min_length])
  list_mean_cut <- lapply(list_mean, function(x) x[1:min_length])
  list_median_cut <- lapply(list_median, function(x) x[1:min_length])
  
  # Konwersja do data frame
  df_fitnes <- data.frame(list_fitnes_cut)
  df_mean <- data.frame(list_mean_cut)
  df_median <- data.frame(list_median_cut)
  
  # Obliczenie średniej dla każdej kolumny
  fitnes_means <- rowMeans(df_fitnes)
  mean_means <- rowMeans(df_mean)
  median_means <-rowMeans(df_median)
  
  # Wykres dla wyników
  df_mean_values <- data.frame(
    "Generation" = 1:length(fitnes_means),
    "Best" = fitnes_means,
    "Mean" = mean_means,
    "Median" = median_means
  )
  ggplot(df_mean_values, aes(x = Generation)) +
    geom_line(aes(y = Best, color = "Best"), linetype = "solid",linewidth=1.5) +
    geom_line(aes(y = Mean, color = "Mean"), linetype = "dotted", linewidth=1.5) +
    geom_line(aes(y= Median, color = "Median"), linetype = "dotted",linewidth=1.5) +
    labs(title = "Średnie wartości funkcji fitness dla Rastrigana", x = "Pokolenie", y = "Wartość funkcji fitnes", color="Legenda") +
    scale_color_manual(values = c("Best" = "blue", "Mean" = "red", "Median" = "green"))
  
}
#draw_ackley()
#draw_rastrigin()
ga_ackley()
#ga_rastrigin()







