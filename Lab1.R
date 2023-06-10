# biblioteki
library(GA)
library(globalOptTests)
library(plot3D)
library(ggplot2)

#Funkcje
ackley <- function(x, y) 
{
  -20*exp(-0.2*sqrt(0.5*(x^2 + y^2))) - exp(0.5*(cos(2*pi*x) + cos(2*pi*y))) + exp(1) + 20
}
Rastrigin <- function(x, y)
{
  20 + x^2 + y^2 - 10*(cos(2*pi*x) + cos(2*pi*y))
}

# wykresy funkcji Ackleya
draw_ackley <- function(theta=50, phi=20)
{
  x <- y <- seq(-3, 3, by = 0.1)
  z <- outer(x, y, ackley)
  persp3D(x,y,z, theta = theta, phi = phi,ticktype = "detailed", main='Wykres 3D Funkcji Ackleya')
  filled.contour(x, y, z, main='Wykres temperaturowy funkcji Ackleya', 
                 plot.axes = {axis(1);axis(2);points(0,0,col='black', 
                                                     bg='white',pch=23, cex=1)})
}

# wykresy funkcji Rastrigina
draw_rastrigin <-function(theta=50, phi=20)
{
  x <- y <- seq(-3, 3, by = 0.1)
  z <- outer(x, y, Rastrigin)
  persp3D(x, y, z, theta = theta, phi = phi ,ticktype = "detailed", main='Wykres 3D funkcji Rastrigina')
  filled.contour(x, y, z, main='Wykres temperaturowy funkcji Rastrigina',
                 plot.axes = {axis(1);axis(2);points(0,0,col='black',
                                                     bg='white',pch=23, cex=1)})
}

#wykonanie algorytmu genetycznego dla funkcji Ackleya
ga_ackley <- function(popSize = 20,maxiter = 5, run = 5, 
                      crossover = 0.8, mutation=0.1, elitism =0.05)
  {
  bounds_a <- getDefaultBounds("Ackleys")
  #ackley_test <- function(x1, x2) {-goTest(par = c(x1,x2), fnName='Ackleys')}
  list_fitnes <- list()
  list_mean <- list()
  list_median <- list()
  list_population_x <-list()
  list_population_y <-list()
  list_fitnes_value <-list()
  for(x in 1:10){
    GA1 <- ga(type = "real-valued",
              fitness = function(z) -ackley(z[1],z[2]), lower = c(bounds_a$lower) ,
              upper = c(bounds_a$upper), popSize = popSize, maxiter = maxiter, 
              run=run, pcrossover = crossover, pmutation = mutation, elitism = elitism,
              monitor = FALSE
              )
    list_fitnes_value[[x]]<-GA1@fitnessValue
    list_population_x[[x]]<-GA1@population[1:GA1@iter]
    list_population_y[[x]]<-GA1@population[2:GA1@iter]
    list_fitnes[[x]] <- c(GA1@summary[1:GA1@iter])
    list_mean[[x]] <- c(GA1@summary[1:GA1@iter, 2])
    list_median[[x]] <- c(GA1@summary[1:GA1@iter, 4])
  }
  # Ucięcie wektorów do długości najkrótszego
  min_length <- min(sapply(list_fitnes, length))
  list_fitnes_cut <- lapply(list_fitnes, function(x) x[1:min_length])
  list_mean_cut <- lapply(list_mean, function(x) x[1:min_length])
  list_median_cut <- lapply(list_median, function(x) x[1:min_length])
  list_population_x_cut <- lapply(list_population_x, function(x) x[1:min_length])
  list_population_y_cut <- lapply(list_population_y, function(x) x[1:min_length])
  
  
  # Konwersja do data frame
  df_fitnes <- data.frame(list_fitnes_cut)
  df_mean <- data.frame(list_mean_cut)
  df_median <- data.frame(list_median_cut)
  df_population_x <-data.frame(list_population_x_cut)
  df_population_y <-data.frame(list_population_y_cut)
  
  # Obliczenie średniej dla każdej kolumny
  fitnes_means <- rowMeans(df_fitnes)
  mean_means <- rowMeans(df_mean)
  median_means <-rowMeans(df_median)
  population_x_means <-rowMeans(df_population_x)
  population_y_means <-rowMeans(df_population_y)
  
  # Wykres dla wyników
  df_mean_values <- data.frame(
    "Generation" = 1:length(fitnes_means),
    "Best" = fitnes_means,
    "Mean" = mean_means,
    "Median" = median_means
  )
  ggplot(df_mean_values, aes(x = Generation)) +
    geom_line(aes(y = Best, color = "Best"), linetype = "dotted",linewidth=1.5) +
    geom_line(aes(y = Mean, color = "Mean"), linetype = "dotted", linewidth=1.5) +
    geom_line(aes(y= Median, color = "Median"), linetype = "dotted",linewidth=1.5) +
    labs(title = "Średnie wartości funkcji fitness dla Ackleya", x = "Pokolenie", y = "Wartość funkcji fitnes", color="Legenda") +
    scale_color_manual(values = c("Best" = "blue", "Mean" = "red", "Median" = "green"))
  list_fitnes <-list()
  list_mean <-list()
  list_median <-list()
  list_population_x <-list()
  list_population_y <-list()
}

#wykonanie algorytmu genetycznego dla funkcji Rastrigina
ga_rastrigin <-function(popSize = 10,maxiter = 20, run = 10, 
                        crossover=0.8, mutation=0.1, elitism =0.05, 
                        wykres_średnich=FALSE, wykres_populacji=FALSE, wykres_krzyżowy=FALSE 
                        )
  {
  cross_prob_range <- seq(0, crossover, 0.1)
  mut_prob_range <- seq(0, mutation, 0.1)
  bounds_r <- getDefaultBounds("Rastrigin")
  #rastrigin_test <- function(x){-goTest(par = x, fnName="Rastrigin")}
  mean_fitnes_matrix = matrix(0,nrow=10,ncol=10)
  list_fitnes <- list()
  list_mean <- list()
  list_median <- list()
  list_population_x <-list()
  list_population_y <-list()
  m<-1
  for (cross_prob in cross_prob_range){
    n<-1
    for (mut_prob in mut_prob_range) {
      list_fitnes_value <-list()
      
      for (x in 1:10){
      GA1 <- ga(type = "real-valued",
                fitness = function(z) -Rastrigin(z[1],z[2]), lower = c(bounds_r$lower),
                upper = c(bounds_r$upper), popSize = popSize, maxiter = maxiter, 
                run=run, pcrossover = cross_prob, pmutation = cross_prob, elitism = elitism,
                monitor=FALSE
                )
      list_fitnes_value <- append(list_fitnes_value, abs(GA1@fitnessValue))
      list_population_x[[x]]<-GA1@population[1:popSize]
      list_population_y[[x]]<-GA1@population[2:popSize]
      list_fitnes[[x]] <- c(GA1@summary[1:GA1@iter])
      list_mean[[x]] <- c(GA1@summary[1:GA1@iter, 2])
      list_median[[x]] <- c(GA1@summary[1:GA1@iter, 4])
      }
      
      mean_fitnes_matrix[m,n] <- mean(unlist(list_fitnes_value))
      n<-n+1
      
      if(wykres_średnich==TRUE){
        
        # Ucięcie wektorów do długości najkrótszego
        min_length <- min(sapply(list_fitnes, length))
        list_fitnes_cut <- lapply(list_fitnes, function(x) x[1:min_length])
        list_mean_cut <- lapply(list_mean, function(x) x[1:min_length])
        list_median_cut <- lapply(list_median, function(x) x[1:min_length])
        
        # Konwersja do data frame
        df_fitnes <- data.frame(list_fitnes_cut)
        df_mean <- data.frame(list_mean_cut)
        df_median <- data.frame(list_median_cut)
        
        # Obliczenie średniej dla każdego wiersza
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
          geom_line(aes(y = Best, color = "Best"), linetype = "dotted",linewidth=1.5) +
          geom_line(aes(y = Mean, color = "Mean"), linetype = "dotted", linewidth=1.5) +
          geom_line(aes(y= Median, color = "Median"), linetype = "dotted",linewidth=1.5) +
          labs(title = "Średnie wartości funkcji fitness dla Ackleya", x = "Pokolenie", y = "Wartość funkcji fitnes", color="Legenda") +
          scale_color_manual(values = c("Best" = "blue", "Mean" = "red", "Median" = "green"))
      }
      
      if(wykres_populacji==TRUE){
        
        # Konwersja do data frame
        df_population_x <-data.frame(list_population_x)
        df_population_y <-data.frame(list_population_y)
        
        # Obliczenie średniej dla każdego wiersza
        population_x_means <-rowMeans(df_population_x)
        population_y_means <-rowMeans(df_population_y)
        
        population_x_means <- as.list(population_x_means)
        population_y_means <- as.list(population_y_means)
        print(GA1@population)
        print(length(df_population_x))
        print(length(list_population_y[[1]]))
        print(length(population_x_means))
        print(length(population_y_means))
        x1 <- x2 <- seq(-35, 30, by = 2)
        f <- outer(x1, x2, Rastrigin)
        filled.contour(x1, x2, f, main=dynamic_title,
                       plot.axes = {axis(1);axis(2);
                         points(x=population_x_means,
                                y=population_y_means, 
                                pch = 20, col = "forestgreen")})
      }
    }
    m <- m+1
  }
print(mean_fitnes_matrix)
}

#draw_ackley(theta = 50, phi = 20)
#draw_rastrigin(theta = 50, phi = 20)
# ga_ackley(popSize = 50, maxiter = 100, run=50,
#           elitism = 0.05, crossover = 0.8, mutation = 0.1
#           )
ga_rastrigin(popSize = 200, maxiter = 250, run=50,
             elitism = 0.05, crossover = 0.1, mutation = 0.1, 
             wykres_populacji=TRUE
              )

