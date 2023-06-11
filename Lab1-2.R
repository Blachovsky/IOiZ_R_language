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
  x <- y <- seq(-35, 35, by = 1)
  z <- outer(x, y, Ackley)
  file_title = '3d_ackley'
  dev.next()
  png(file = paste0("./wyniki/3d/ackley/",file_title,".png"), height = 600,width = 800)
  persp3D(x,y,z, theta = theta, phi = phi,ticktype = "detailed", main='Wykres 3D Funkcji Ackleya')
  dev.off();
  }

# pokaż czysty wykres 3D funkcji Rastrigina
draw_rastrigin_3D <-function(theta=50, phi=20)
{
  x <- y <- seq(-500, 500, by = 10)
  z <- outer(x, y, Rastrigin)
  file_title = '3d_rastrigin'
  dev.next()
  png(file = paste0("./wyniki/3d/rastrigin/",file_title,".png"), height = 600,width = 800)
  persp3D(x, y, z, theta = theta, phi = phi ,ticktype = "detailed", main='Wykres 3D funkcji Rastrigina')
  dev.off();
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
    file_title <- sprintf("Średnie_wartości_dla_Ackleya_popSize=%d_maxiter=%d_run=%d_elityzm=%.1f", 
                          popSize, maxiter, run, elitism/100)
    dynamic_title <- sprintf("Średnie wartości funkcji fitness dla Ackleya\npopSize = %d, maxiter = %d, run = %d, elityzm = %.1f", 
                             popSize, maxiter, run, elitism/100)
    dev.next()
    png(file = paste0("./wyniki/średnie/ackley/",file_title,".png"), height = 600,width = 800)
    print(ggplot(df_mean_values, aes(x = Generation)) +
            geom_line(aes(y = Najlepszy_wynik, color = "Najlepszy wynik"), linetype = "dotted",linewidth=1.25) +
            geom_line(aes(y = Średnia, color = "Średnia"), linetype = "dotted", linewidth=1.25) +
            geom_line(aes(y= Mediana, color = "Mediana"), linetype = "dotted",linewidth=1.25) +
            labs(title = dynamic_title, x = "Pokolenie", y = "Wartość funkcji fitnes", color="Legenda") +
            scale_color_manual(values = c("Najlepszy wynik" = "blue", "Średnia" = "red", "Mediana" = "green"))+
            theme(plot.title = element_text(hjust = 0.5)))
    dev.off();
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
  file_title <- sprintf("Średnie_wartości_dla_Rastrigina_popSize=%d_maxiter=%d_run=%d_elityzm=%.1f", 
                           popSize, maxiter, run, elitism/100)
  
  dynamic_title <- sprintf("Średnie wartości funkcji fitness dla Rastrigina\ndla popSize = %d, maxiter = %d, run = %d, elityzm =%.1f", 
                           popSize, maxiter, run, elitism/100)
  dev.next()
  png(file = paste0("./wyniki/średnie/rastrigin/",file_title,".png"), height = 600,width = 800)
  print(ggplot(df_mean_values, aes(x = Generation)) +
    geom_line(aes(y = Najlepszy_wynik, color = "Najlepszy wynik"), linetype = "dotted",linewidth=1.25) +
    geom_line(aes(y = Średnia, color = "Średnia"), linetype = "dotted", linewidth=1.25) +
    geom_line(aes(y= Mediana, color = "Mediana"), linetype = "dotted",linewidth=1.25) +
    labs(title = dynamic_title, x = "Pokolenie", y = "Wartość funkcji fitnes", color="Legenda") +
    scale_color_manual(values = c("Najlepszy wynik" = "blue", "Średnia" = "red", "Mediana" = "green"))+
    theme(plot.title = element_text(hjust = 0.5)))
  dev.off();
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
  file_title = sprintf('Wyniki_GA-funkcji_Ackleya_Populacja:%d_Maxiter:%d_Run:%d', popSize, maxiter, run)
  dynamic_title = sprintf('Wyniki GA funkcji Ackleya przy różnych wartościach kryżowania i mutacji \n Populacja: %d, Maxiter: %d, Run: %d', popSize, maxiter, run)
  dev.next()
  png(file = paste0("./wyniki/wynik_krzyzowania/ackley/",file_title,".png"), height = 600,width = 800)
  filled.contour(cross_prob_range, mut_prob_range, results_matrix,
                main= dynamic_title, 
                color.palette = bl2gr.colors, plot.axes = {axis(1);axis(2)})
  title(xlab = "Prawdopodobieństwo krzyżowania", ylab = "Pradopodobieństwo mutacji")
  dev.off();
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
  file_title = sprintf('Wyniki_GA-funkcji_Rastrigina_Populacja:%d_Maxiter:%d_Run:%d', popSize, maxiter, run)
  dynamic_title = sprintf('Wyniki GA funkcji Rastrigina przy różnych wartościach kryżowania i mutacji \n Populacja: %d, Maxiter: %d, Run: %d', popSize, maxiter, run)
  dev.next()
  png(file = paste0("./wyniki/wynik_krzyzowania/rastrigin/",file_title,".png"), height = 600,width = 800)
  filled.contour(cross_prob_range, mut_prob_range, results_matrix,
                 main= dynamic_title, 
                 color.palette = bl2gr.colors, plot.axes = {axis(1);axis(2)})
  title(xlab = "Prawdopodobieństwo krzyżowania", ylab = "Pradopodobieństwo mutacji")
  def.off();
}

draw_pop_graph_ackley <- function(list_population_x, list_population_y, cros, mut, pop){
  # Konwersja do data frame
  df_population_x <-data.frame(list_population_x)
  df_population_y <-data.frame(list_population_y)
  
  # Obliczenie średniej dla każdego wiersza
  population_x_means <-rowMeans(df_population_x)
  population_y_means <-rowMeans(df_population_y)
  
  population_x_means <- as.list(population_x_means)
  population_y_means <- as.list(population_y_means)
  
  dynamic_title <- sprintf("Wykres populacji %d dla funckji Ackleya\nmutacja = %.1f, krzyżowanie = %.1f", 
                           pop, mut, cros)
  file_title = sprintf("Wykres_populacji_%d_dla_Ackleya_mutacja=%.1f_krzyżowanie=%.1f",pop, mut, cros)
  x1 <- x2 <- seq(-35, 30, by = 1)
  f <- outer(x1, x2, Ackley)
  dev.next()
  png(file = paste0("./wyniki/populacja/ackley/",file_title,".png"), height = 600,width = 800)
  filled.contour(x1, x2, f, main=dynamic_title,
                 plot.axes = {axis(1);axis(2);
                   points(x=population_x_means,
                          y=population_y_means, 
                          pch = 20, col = "forestgreen")})
  dev.off();
}

draw_pop_graph_rastrigin <- function(list_population_x, list_population_y, cros, mut, pop){
  # Konwersja do data frame
  df_population_x <-data.frame(list_population_x)
  df_population_y <-data.frame(list_population_y)
  
  # Obliczenie średniej dla każdego wiersza
  population_x_means <-rowMeans(df_population_x)
  population_y_means <-rowMeans(df_population_y)
  
  population_x_means <- as.list(population_x_means)
  population_y_means <- as.list(population_y_means)
  
  file_title = sprintf("Wykres_populacji_%d_dla_Rastrigina_mutacja=%.1f_krzyżowanie=%.1f",pop, mut, cros)
  dynamic_title <- sprintf("Wykres populacji %d dla funckji Rastrigina\nmutacja = %.1f, krzyżowanie = %.1f", 
                           pop, mut, cros)
  x1 <- x2 <- seq(-500, 500, by = 10)
  f <- outer(x1, x2, Rastrigin)
  dev.next()
  png(file = paste0("./wyniki/populacja/rastrigin/",file_title,".png"), height = 600,width = 800)
  filled.contour(x1, x2, f, main=dynamic_title,
                 plot.axes = {axis(1);axis(2);
                   points(x=population_x_means,
                          y=population_y_means, 
                          pch = 20, col = "forestgreen")})
  dev.off();
}

combined_mu_cr_ackley_pop <- function(popSize = 200,maxiter = 5, run = 5){
  bounds <- getDefaultBounds("Ackleys")
  cross_prob_range <- seq(0, 1, 0.1)
  mut_prob_range <- seq(0, 1, 0.1)
  list_population_x <- list()
  list_population_y <- list()
  for(cros in cross_prob_range){
    for(mut in mut_prob_range){
      for(i in 1:10) {
        GA <- ga(type = "real-valued",
                 fitness = function(s) -Ackley(s[1],s[2]), lower = c(bounds$lower),
                 upper = c(bounds$upper), popSize = popSize, maxiter = maxiter, 
                 run=run, pcrossover = cros, pmutation = mut, monitor=FALSE )
        list_population_x[[i]]<-GA@population[,1]
        list_population_y[[i]]<-GA@population[,2]
        }
      draw_pop_graph_ackley(list_population_x, list_population_y, cros, mut, popSize)
      list_population_x <- list()
      list_population_y <- list()
      }
  }
}

combined_mu_cr_rastrigin_pop <- function(popSize = 200,maxiter = 5, run = 5){
  bounds <- getDefaultBounds("Rastrigin")
  cross_prob_range <- seq(0, 1, 0.1)
  mut_prob_range <- seq(0, 1, 0.1)
  list_population_x <- list()
  list_population_y <- list()
  for(cros in cross_prob_range){
    for(mut in mut_prob_range){
      for(i in 1:10) {
        GA <- ga(type = "real-valued",
                 fitness = function(s) -Rastrigin(s[1],s[2]), lower = c(bounds$lower),
                 upper = c(bounds$upper), popSize = popSize, maxiter = maxiter, 
                 run=run, pcrossover = cros, pmutation = mut, monitor=FALSE )
        list_population_x[[i]]<-GA@population[,1]
        list_population_y[[i]]<-GA@population[,2]
      }
      draw_pop_graph_rastrigin(list_population_x, list_population_y, cros, mut, popSize)
      list_population_x <- list()
      list_population_y <- list()
    }
  }
}

df <- data.frame(
  elityzm = c(1, 1, 1, 1, 1, 1, 1, 1, 
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


# for(i in 1:nrow(df)){
#    print(df[i,])
#   ga_mean_ackley(df[i,3], df[i,2], df[i,4],df[i,1])
# }
# 
# for(i in 1:nrow(df)){
#   print(df[i,])
#   ga_mean_rastrigin(df[i,3], df[i,2], df[i,4],df[i,1])
# }
draw_ackley_3D()
draw_rastrigin_3D()
# combined_mu_cr_ackley(popSize = 50, maxiter = 5, run = 5)
# combined_mu_cr_rastrigin(popSize = 100, maxiter = 200, run = 50)
# combined_mu_cr_ackley_pop(popSize = 50, maxiter = 5, run = 5)
#combined_mu_cr_rastrigin_pop(popSize = 50, maxiter = 5, run = 5)


