library(ExtDist)
library(SuppDists)



larvae1SB <- function(a, eff) {
  ms1 <- dJohnsonSB(seq(180, 800), 
                    params = list(gamma = 1.092, delta = 1.376, xi = 173.590, 
                                  lambda = 644.440))
  ms1a <- dJohnsonSB(seq(a[1], a[2]), 
                     params = list(gamma = 1.092, delta = 1.376, xi = 173.590, 
                                   lambda = 644.440)) * (1 - eff)
  ind <- function(x) {x - 169}
  
  c(ms1[ind(180): ind(a[1] - 1)], ms1a, ms1[ind(a[2] + 1): ind(800)])
}


plot(seq(180, 800), larvae1SB(c(235, 275), 0.90), type = "l")







adults2SB <- function(a, eff) {
  p <- rep(NA, 2)
  p[1] <- pJohnsonSB(a[1], 
                     params = list(gamma = 1.114, delta = 1.311, xi = 108.639, 
                                   lambda = 615.277))
  p[2] <- pJohnsonSB(a[2], 
                     params = list(gamma = 1.114, delta = 1.311, xi = 108.639, 
                                   lambda = 615.277))
  
  b <- rep(NA, 2)
  b[1] <- round(qJohnsonSB(p[1], 
                           params = list(gamma = 0.3968, delta = 1.4682, xi = FDD_CDD(890.67), 
                                         lambda = FDD_CDD(1486.1))))
  b[2] <- round(qJohnsonSB(p[2], 
                           params = list(gamma = 0.3968, delta = 1.4682, xi = FDD_CDD(890.67), 
                                         lambda = FDD_CDD(1486.1))))
  
  ms1 <- dJohnsonSB(seq(70, 2000), 
                    params = list(gamma = 0.3968, delta = 1.4682, xi = FDD_CDD(890.67), 
                                  lambda = FDD_CDD(1486.1)))
  ms1a <- dJohnsonSB(seq(b[1], b[2]), 
                     params = list(gamma = 0.3968, delta = 1.4682, xi = FDD_CDD(890.67), 
                                   lambda = FDD_CDD(1486.1))) * (1 - eff)
  ind <- function(x) {x - 69}
  
  c(ms1[ind(70): ind(b[1] - 1)], ms1a, ms1[ind(b[2] + 1): ind(2000)])
}




adults2SBa <- function(a, eff) {
  p <- rep(NA, 2)
  p[1] <- pJohnsonSB(a[1], 
                     params = list(gamma = 1.114, delta = 1.311, xi = 108.639, 
                                   lambda = 615.277))
  p[2] <- pJohnsonSB(a[2], 
                     params = list(gamma = 1.114, delta = 1.311, xi = 108.639, 
                                   lambda = 615.277))
  
  b <- rep(NA, 2)
  b[1] <- round(qJohnsonSB(p[1], 
                           params = list(gamma = 0.3968, delta = 1.4682, xi = FDD_CDD(890.67), 
                                         lambda = FDD_CDD(1486.1))))
  b[2] <- round(qJohnsonSB(p[2], 
                           params = list(gamma = 0.3968, delta = 1.4682, xi = FDD_CDD(890.67), 
                                         lambda = FDD_CDD(1486.1))))
  
  ms1 <- dJohnsonSB(seq(1, 2000), 
                    params = list(gamma = 0.3968, delta = 1.4682, xi = FDD_CDD(890.67), 
                                  lambda = FDD_CDD(1486.1)))
  ms1a <- dJohnsonSB(seq(b[1], b[2]), 
                     params = list(gamma = 0.3968, delta = 1.4682, xi = FDD_CDD(890.67), 
                                   lambda = FDD_CDD(1486.1))) * (1 - eff)
  
  c(ms1[1: (b[1] - 1)], ms1a, ms1[(b[2] + 1): 2000])
}




aa1 <- eggs1SB(a = c(250, 350), eff = 0.8)
bb1 <- adults2SB(a = c(250, 350), eff = 0.8)

par(mar = c(5, 5, 4, 2) + 0.1)
plot(seq(70, 2000), aa1, col = "blue", type = "l", xlim = c(70, 1500), ylim = c(0, 0.005), #c(0, ms_up[length(ms_up)]),
     xlab = "Cumulative degree-days", ylab = "", cex.lab = 2, cex.axis = 1.8, lwd = 2, yaxt = "n")

axis(2, seq(0, 0.005, 0.002), cex.axis = 1.8, las = 2)

lines(seq(70, 2000), bb1, col = "brown", type = "l", lwd = 2)


par(mar = c(5, 5, 4, 2) + 0.1)
plot(seq(70, 2000), cumsum(bb1), col = "blue", type = "l", xlim = c(550, 1800), ylim = c(0, 1), #c(0, ms_up[length(ms_up)]),
     xlab = "Cumulative degree-days", ylab = "Cumulative average counts", cex.lab = 2, cex.axis = 1.8, lwd = 2, yaxt = "n")

lines(seq(70, 2000), ms1a, col = "brown", type = "l", lwd = 2)

axis(2, seq(0, 1, 0.1), cex.axis = 1.8, las = 2)




