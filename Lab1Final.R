# biblioteki

library(GA)
library(globalOptTests)
library(plot3D)
library(ggplot2)

#Funkcje

Ackley <- function(x, y) 
{
  -20*exp(-0.2*sqrt(0.5*(x^2 + y^2))) - exp(0.5*(cos(2*pi*x) + cos(2*pi*y))) + exp(1) + 20
}

Rastrigin <- function(x, y)
{
  20 + x^2 + y^2 - 10*(cos(2*pi*x) + cos(2*pi*y))
}


# pokaż czysty wykres 3D funkcji Ackleya
draw_ackley_3D <- function(theta=50, phi=20)
{
  x <- y <- seq(-10, 10, by = 0.2)
  z <- outer(x, y, ackley)
  persp3D(x,y,z, theta = theta, phi = phi,ticktype = "detailed", main='Wykres 3D Funkcji Ackleya')
}

# pokaż czysty wykres 3D funkcji Rastrigina
draw_rastrigin_3D <-function(theta=50, phi=20)
{
  x <- y <- seq(-10, 10, by = 0.2)
  z <- outer(x, y, Rastrigin)
  persp3D(x, y, z, theta = theta, phi = phi ,ticktype = "detailed", main='Wykres 3D funkcji Rastrigina')
}

# Pokaż uśredniony wynik GA dla Ackleya z domyślną mutacją, elityzmem i krzyżowaniem
ga_mean_ackley <- function(popSize = 50,maxiter = 150, run = 20, elitism=1){
  list_fitnes <- list()
  list_mean <- list()
  list_median <- list()
  bounds <- getDefaultBounds("Ackleys")
  for (x in 1:10){
    GA <- ga(type = "real-valued",
              fitness = function(s) -Ackley(s[1],s[2]), lower = c(bounds$lower),
              upper = c(bounds$upper), popSize = popSize, maxiter = maxiter, 
              run = run,elitism = base::max(1, round(popSize * (elitism/100))), monitor=FALSE
    )
    list_fitnes[[x]] <- c(GA@summary[1:GA@iter])
    list_mean[[x]] <- c(GA@summary[1:GA@iter, 2])
    list_median[[x]] <- c(GA@summary[1:GA@iter, 4])
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
    
    # Obliczenie średniej dla każdego wiersza
    fitnes_means <- rowMeans(df_fitnes)
    mean_means <- rowMeans(df_mean)
    median_means <-rowMeans(df_median)
    
    # Wykres dla wyników
    df_mean_values <- data.frame(
      "Generation" = 1:length(fitnes_means),
      "Najlepszy_wynik" = fitnes_means,
      "Średnia" = mean_means,
      "Mediana" = median_means
    )
    
    dynamic_title <- sprintf("Średnie wartości funkcji fitness dla Ackleya\npopSize = %d, maxiter = %d, run = %d, elityzm = %.1f", 
                             popSize, maxiter, run, elitism/100)
    
    print(ggplot(df_mean_values, aes(x = Generation)) +
            geom_line(aes(y = Najlepszy_wynik, color = "Najlepszy wynik"), linetype = "dotted",linewidth=1.25) +
            geom_line(aes(y = Średnia, color = "Średnia"), linetype = "dotted", linewidth=1.25) +
            geom_line(aes(y= Mediana, color = "Mediana"), linetype = "dotted",linewidth=1.25) +
            labs(title = dynamic_title, x = "Pokolenie", y = "Wartość funkcji fitnes", color="Legenda") +
            scale_color_manual(values = c("Najlepszy wynik" = "blue", "Średnia" = "red", "Mediana" = "green"))+
            theme(plot.title = element_text(hjust = 0.5)))
    
    list_fitnes <- list()
    list_mean <- list()
    list_median <- list()
}
  

# Pokaż uśredniony wynik GA dla Rastrigina z domyślną mutacją, krzyżowaniem
ga_mean_rastrigin <- function(popSize = 50,maxiter = 150, run = 20, elitism=1){
  list_fitnes <- list()
  list_mean <- list()
  list_median <- list()
  bounds <- getDefaultBounds("Rastrigin")
  for (x in 1:10){
    GA <- ga(type = "real-valued",
             fitness = function(s) -Rastrigin(s[1],s[2]), lower = c(bounds$lower),
             upper = c(bounds$upper), popSize = popSize, maxiter = maxiter, 
             run=run,elitism = base::max(1, round(popSize * (elitism/100))), monitor=FALSE
    )
    list_fitnes[[x]] <- c(GA@summary[1:GA@iter])
    list_mean[[x]] <- c(GA@summary[1:GA@iter, 2])
    list_median[[x]] <- c(GA@summary[1:GA@iter, 4])
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
  
  # Obliczenie średniej dla każdego wiersza
  fitnes_means <- rowMeans(df_fitnes)
  mean_means <- rowMeans(df_mean)
  median_means <-rowMeans(df_median)
  
  # Wykres dla wyników
  df_mean_values <- data.frame(
    "Generation" = 1:length(fitnes_means),
    "Najlepszy_wynik" = fitnes_means,
    "Średnia" = mean_means,
    "Mediana" = median_means
  )
  dynamic_title <- sprintf("Średnie wartości funkcji fitness dla Rastrigina\ndla popSize = %d, maxiter = %d, run = %d, elityzm =%.1f", 
                           popSize, maxiter, run, elitism/100)
  
  print(ggplot(df_mean_values, aes(x = Generation)) +
    geom_line(aes(y = Najlepszy_wynik, color = "Najlepszy wynik"), linetype = "dotted",linewidth=1.25) +
    geom_line(aes(y = Średnia, color = "Średnia"), linetype = "dotted", linewidth=1.25) +
    geom_line(aes(y= Mediana, color = "Mediana"), linetype = "dotted",linewidth=1.25) +
    labs(title = dynamic_title, x = "Pokolenie", y = "Wartość funkcji fitnes", color="Legenda") +
    scale_color_manual(values = c("Najlepszy wynik" = "blue", "Średnia" = "red", "Mediana" = "green"))+
    theme(plot.title = element_text(hjust = 0.5)))
  
  list_fitnes <- list()
  list_mean <- list()
  list_median <- list()
}

combined_mu_cr_ackley <- function(popSize = 200,maxiter = 5, run = 5){
  bounds <- getDefaultBounds("Ackleys")
  cross_prob_range <- seq(0, 1, 0.1)
  mut_prob_range <- seq(0, 1, 0.1)
  results <- list()
  results_matrix <- matrix(0,length(cross_prob_range),length(mut_prob_range))
  m  <- 1
  for(cros in cross_prob_range){
    n <- 1  
    for(mut in mut_prob_range){
          for(i in 1:10) {
            GA <- ga(type = "real-valued",
                      fitness = function(s) -Ackley(s[1],s[2]), lower = c(bounds$lower),
                      upper = c(bounds$upper), popSize = popSize, maxiter = maxiter, 
                      run=run, pcrossover = cros, pmutation = mut, monitor=FALSE
            )
            results <- append(results, GA@fitnessValue)
          }
        results_matrix[m,n] <- mean(unlist(results))
        n <- n + 1
    }
    m <- m + 1
  }
  print(results_matrix)
  dynamic_title = sprintf('Wyniki GA funkcji Ackleya przy różnych wartościach kryżowania i mutacji \n Populacja: %d, Maxiter: %d, Run: %d', popSize, maxiter, run)
  filled.contour(cross_prob_range, mut_prob_range, results_matrix,
                main= dynamic_title, 
                color.palette = bl2gr.colors, plot.axes = {axis(1);axis(2)})
  title(xlab = "Prawdopodobieństwo krzyżowania", ylab = "Pradopodobieństwo mutacji")
}

combined_mu_cr_rastrigin <- function(popSize = 200,maxiter = 5, run = 5){
  bounds <- getDefaultBounds("Rastrigin")
  cross_prob_range <- seq(0, 1, 0.1)
  mut_prob_range <- seq(0, 1, 0.1)
  results <- list()
  results_matrix <- matrix(0,length(cross_prob_range),length(mut_prob_range))
  m  <- 1
  for(cros in cross_prob_range){
    n <- 1  
    for(mut in mut_prob_range){
      for(i in 1:10) {
        GA <- ga(type = "real-valued",
                 fitness = function(s) -Rastrigin(s[1],s[2]), lower = c(bounds$lower),
                 upper = c(bounds$upper), popSize = popSize, maxiter = maxiter, 
                 run=run, pcrossover = cros, pmutation = mut, monitor=FALSE
        )
        results <- append(results, GA@fitnessValue)
      }
      results_matrix[m,n] <- mean(unlist(results))
      n <- n + 1
    }
    m <- m + 1
  }
  print(results_matrix)
  dynamic_title = sprintf('Wyniki GA funkcji Rastrigina przy różnych wartościach kryżowania i mutacji \n Populacja: %d, Maxiter: %d, Run: %d', popSize, maxiter, run)
  filled.contour(cross_prob_range, mut_prob_range, results_matrix,
                 main= dynamic_title, 
                 color.palette = bl2gr.colors, plot.axes = {axis(1);axis(2)})
  title(xlab = "Prawdopodobieństwo krzyżowania", ylab = "Pradopodobieństwo mutacji")
}

df <- data.frame(
  elityzm = c(10, 10, 10, 10, 10, 10, 10, 10, 
              50, 50, 50, 50, 50, 50, 50, 50,
              90, 90, 90, 90, 90, 90, 90, 90),
  maxIter = c(300, 750, 300, 750, 300, 750, 300, 750, 
              300, 750, 300, 750, 300, 750, 300, 750, 
              300, 750, 300, 750, 300, 750, 300, 750),
  popSize = c(50, 50, 250, 250, 50, 50, 250, 250,
              50, 50, 250, 250, 50, 50, 250, 250, 
              50, 50, 250, 250, 50, 50, 250, 250),
  run = c(50, 50, 50, 50, 150, 150, 150, 150, 
          50, 50, 50, 50, 150, 150, 150, 150, 
          50, 50, 50, 50, 150, 150, 150, 150)
)


for(i in 1:nrow(df)){
   print(df[i,])
   ga_mean_ackley(df[i,3], df[i,2], df[i,4],df[i,1])
}

# for(i in 1:nrow(df)){
#   print(df[i,])
#   ga_mean_rastrigin(df[i,3], df[i,2], df[i,4],df[i,1])
# }

#combined_mu_cr_ackley(popSize = 100, maxiter = 200, run = 50)
#combined_mu_cr_rastrigin(popSize = 100, maxiter = 200, run = 50)



