Rastrigin <- function(x1, x2)
{
  20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
}

postfit <- function(object, ...)
{
  pop <- object@population
  # update info
  if(!exists(".pop", envir = globalenv()))
    assign(".pop", NULL, envir = globalenv())
  .pop <- get(".pop", envir = globalenv())
  assign(".pop", append(.pop, list(pop)), envir = globalenv()) 
  # output the input ga object (this is needed!!)
  object 
}

GA <- ga(type = "real-valued",
         fitness =  function(x) -Rastrigin(x[1], x[2]),
         lower = c(-35, -35), upper = c(30, 30),
         popSize = 200, maxiter = 100, seed = 1,
         postFitness = postfit)

str(.pop, max.level = 1, list.len = 5)

x1 <- x2 <- seq(-35, 30, by = 2)
f <- outer(x1, x2, Rastrigin)
iter_to_show = c(1,5,10,20,50,100)
for(i in seq(iter_to_show))
{
  dynamic_title <- sprintf("Wykres temperaturowy funkcji Rastrigina iteracja = %d", iter_to_show[i])
  filled.contour(x1, x2, f, main=dynamic_title,
                 plot.axes = {axis(1);axis(2);points(.pop[[iter_to_show[i]]], pch = 20, col = "forestgreen")})
}