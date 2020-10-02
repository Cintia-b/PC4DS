## File: pc4ds-lab1.r
## Description: Lab 1 of Parallel Computing for Data Science
## Date: 7/10/2016 by jc

## Exercice 1 ####

is.integer(2)

if(sqrt(2) * sqrt(2) != 2) print("what ?!")

if(0.1 + 0.2 == 0.3) print("result is ok")

if(0.1 + 0.2 != 0.3) print("no way !!!!")

## Exercice 2 ####
# 1.
f <- function(x) sin(x) ^ 2 + sqrt(abs(x - 3))
f2 <- function(x) -f(x)
# 2.
curve(f, from = -6, to = 4, lwd = 2)
grid(lty = 1)
# 3.
integrate(f, -6, 4)
# 4.
mn <- optimize(f, interval = c(-6, 4))
# 5.
Mx <- optimize(f, interval = c(-6, 4), 
               maximum = TRUE)

## MAIS!!!!
abline(h = mn$objective, col = "red")
abline(h = Mx$objective, col = "blue")

## Exercice 3 (Problème) : première version ####

rm(list = ls())
# 1.
simuData <- function(n) rnorm(n)
# 2.
perte <- function(s, y, p) (sum(abs(s - y) ^ p)) ^ (1 / p)
# 3.
y <- simuData(20)

for(p in c(1, 2, 5, 1/2)) {
  print(paste("Begin computation for p = ", p))
  print(optimise(f = perte, interval = range(y), y = y, p = p))
}


## Exercice 3 (Problème) : deuxième version ####

rm(list = ls())
# 1.
simuData <- function(n) rnorm(n)
# 2.
perte <- function(s, y, p) (sum(abs(s - y) ^ p)) ^ (1 / p)
# 3.
y <- simuData(20)

ps <- c(1, 2, 5, 1/2)
lps <- length(ps)
results <- data.frame(p         = numeric(lps),
                      minimum   = numeric(lps),
                      objective = numeric(lps))

for (p in seq_along(ps)) {
  print(paste("Begin computation for p = ", ps[p]))
  res <- optimise(f = perte, interval = range(y), y = y, p = ps[p])
  results[p, ] <- c(ps[p], res)
}

results 
median(y) # should be minimum for p = 1
mean(y)   # should be minimum for p = 2

