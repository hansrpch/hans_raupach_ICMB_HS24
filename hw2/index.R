?rnorm
## Create distribution
N <- 5000 # 5000 actors in the populatiom
MU <- 100 # mean wealth = 100
dis <- rnorm(N, mean = MU, sd = MU/5) # distribution 

## Gini coefficient
y <- sort(dis) # sort dis vector for the Gini coefficient 
n <- length(y) # n = number of observations in vector y
for(i in 1:length(y)) {    
  numer <- 2 * sum((1:i) * y)
}
denom <- n * sum(y)
gini <- (numer / denom) - ((n + 1) / n)

## Histogram
hist(dis, xlab = "wealth", ylab = "count", breaks = 30, xlim = c(0, 200))

## Transactions, random split (used ChatGPT for help)
random_split <- function(A, B) {
  # Gesamtbetrag im Topf
  pot <- A + B
  
  # Zufällige Aufteilung des Topfes
  share <- runif(1, 0, pot) # 1 Zufallszahl erzeugen, 0 = Minumum, pot = Maximum
  
  # Rückgabe der beiden Anteile
  return(c(share, pot - share))
}
random_split(100, 100)

## Interactions
sample(length(y), 2) # random sample from the distribution (index number)

## Simulation
sapply(y, random_split)



