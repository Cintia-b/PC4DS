## Devoir Obligatoire 2
## By Cintia Bru

#On veut simuler selon le vecteur aléatoire (u,v) 
#Il suit une distribution uniformee sur le domaine
# Dk= {(u,v)->R^2 : u^1/k + v^1/k <= 1 et u,v >= 0}
rm(list = ls())
#On commence par le cas simple ou k = 1
k = 1

#fonction qui genere des vecteurs aleatoires
#les vecteurs sont définis sur [0-1]

u = runif(min = 0, max = 1, n = 100)
v = runif(min = 0, max = 1, n = 100)
points <- data.frame(u,v)

plot(points,main="points des vecteurs aléatoires",xlab="u",ylab="v")

#On crée la fonction du rejet
#Prends en entree une matrice avec les vecteurs
#Et la valeur de k
#return vrai si dans la zone, faux sinon
f_Reject <- function(data,k){
  for (i in 1:100){
      if (((data[i,1]^(1/k)) && (data[i,2]^(1/k)) <=1 ) && (data[i,1] && data[i,2])>=0) {
        not_rejected <- rbind(not_rejected,c(data[i,1],data[i,2]))
      }
  } 
  return(not_rejected)
}

# f_Reject2 <- function(data,k){
#   while((u[i]^(1/k) & v[i]^(1/k))<=1){
#     u = runif(min = 0, max = 1, n = 100)
#     v = runif(min = 0, max = 1, n = 100)
#   }
#   return(c(u,v))
# }

results <- f_Reject(points,1)

nreps <- 1000
Observations <- numeric(nreps)
for (i in seq_along(Observations)) 
  Observations[i] <- f_Reject(points,k)

hist(Observations)


debug(f_Reject)
