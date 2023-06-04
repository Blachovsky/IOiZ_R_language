# funkcja Ackleya
ackley <- function(x, y) {
  -20*exp(-0.2*sqrt(0.5*(x^2 + y^2))) - exp(0.5*(cos(2*pi*x) + cos(2*pi*y))) + exp(1) + 20
}
ackley_ga <- function(x) {-goTest(par = x, fnName='Ackleys')}
bounds_a <- getDefaultBounds("Ackleys")
extrema_a <- getGlobalOpt("Ackleys")

# Wykresy funkcji
x <- y <- seq(-3, 3, by = 0.1)
z <- outer(x, y, ackley)
persp3D(x,y,z, theta = 50, phi = 20,ticktype = "detailed", main='Wykres 3D Funkcji Ackleya')
points3D(0,0,0, add=TRUE, col='black', bg='white',pch=23, cex=1)
filled.contour(x, y, z, main='Wykres temperaturowy funkcji Ackleya', 
               plot.axes = {axis(1);axis(2);points(0,0,col='black', bg='white',pch=23, cex=1)})

# funkcja Rastrigina
Rastrigin <- function(x, y)
{
  20 + x^2 + y^2 - 10*(cos(2*pi*x) + cos(2*pi*y))
}
bounds_r <- getDefaultBounds("Rastrigin")
extrema_r <- getGlobalOpt("Rastrigin")
rastrigin_ga <- function(x){-goTest(par = x, fnName="Rastrigin")}

# Wykresy funkcji
x <- y <- seq(-3, 3, by = 0.1)
z <- outer(x, y, Rastrigin)
persp3D(x, y, z, theta = 50, phi = -20,ticktype = "detailed", main='Wykres 3D funkcji Rastrigina')
points3D(0,0,0, add=TRUE, col='black', bg='white',pch=23, cex=1)
filled.contour(x, y, z, main='Wykres temperaturowy funkcji Rastrigina',
               plot.axes = {axis(1);axis(2);points(0,0,col='black', bg='white',pch=23, cex=1)})

# #algorytmy genetyczne
# GA1 <- ga(type = "real-valued",
#           fitness = ackley_ga, lower = c(bounds_a$lower) ,
#           upper = c(bounds_a$upper), popSize = 200, maxiter = 5000, run = 400,
#           parallel = "snow")
# 
# GA2 <- ga(type = "real-valued",
#           fitness = rastrigin_ga, lower = c(bounds_r$lower),
#           upper = c(bounds_r$upper), popSize = 200, maxiter = 5000, run=400,
#           parallel = "snow")
# 
# 
# summary(GA1)
# summary(GA2)

