# Definiowanie list
elityzm <- c(0.15, 0.30)
maxIter <- c(300, 750)
popSize <- c(50, 250)
run <- c(200, 500)

# Tworzenie wszystkich kombinacji
combinations <- expand.grid(elityzm=elityzm, maxIter=maxIter, popSize=popSize, run=run)

# Wyświetlanie wyników
print(combinations)
