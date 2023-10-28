library(ExtDist)
library(SuppDists)

larvae1SB <- function(aps, eff) {
  ms1 <- dJohnsonSB(seq(180, 800), 
                    params = list(gamma = 1.092, delta = 1.376, xi = 173.590, 
                                  lambda = 644.440))
  ms1a <- matrix(NA, length(aps), 113)
  
  for(i in 1:length(aps)){
    ms1a[i, ] <- dJohnsonSB(seq(aps[[i]][1], aps[[i]][2]), 
                            params = list(gamma = 1.092, delta = 1.376, xi = 173.590, 
                                          lambda = 644.440)) * (1 - eff)
  }
  
  prps <- matrix(NA, 2, 2)
  
  for(i in 1:length(aps)) {
    prps[i, ] <- pJohnsonSB(c(aps[[i]][1], aps[[i]][2]), 
                            params = list(gamma = 1.092, delta = 1.376, xi = 173.590, 
                                          lambda = 644.440))
  }
  
  ind <- function(x) {x - 179}
  
  control <- ms1
  
  for(i in 1:length(aps)) {
    control[ind(aps[[i]][1]):ind(aps[[i]][2])] <- ms1a[i, ]
  }
  
  dds <- seq(180, 800)
  ref <- ms1
  
  list(data.frame(dds = dds, ref = ref, control = control), prps)
}


test <- larvae1SB(aps = list(a = c(235, 347), b = c(371, 483)), 0.90)


plot(test[[1]]$dds, test[[1]]$ref, type = "l")
plot(test[[1]]$dds, test[[1]]$control, type = "l")


plot(test[[1]]$dds, cumsum(test[[1]]$ref), type = "l")
lines(test[[1]]$dds, cumsum(test[[1]]$control))




aps2 <- round(qJohnsonSB(c(test[[2]][1, ], test[[2]][2, ]), 
           params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                         lambda = 825.6111)))



ms1 <- dJohnsonSB(seq(500, 1200), 
                  params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                lambda = 825.6111))

plot(seq(500, 1200), ms1, type = "l")



ms1a <- dJohnsonSB(seq(aps2[1], aps2[2]), 
                   params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                 lambda = 825.6111)) * (1 - 0.9)

ms1b <- dJohnsonSB(seq(aps2[3], aps2[4]), 
                   params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                 lambda = 825.6111)) * (1 - 0.9)


control <- ms1

control[(aps2[1] - 499):(aps2[2] - 499)] <- ms1a
control[(aps2[3] - 499):(aps2[4] - 499)] <- ms1b


plot(seq(500, 1200), control, type = "l")


plot(seq(500, 1200), cumsum(ms1), type = "l")
lines(seq(500, 1200), cumsum(control))



