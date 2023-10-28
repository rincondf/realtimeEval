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
test2 <- larvae1SB(aps = list(a = c(290, 402), b = c(426, 538)), 0.90)

plot(test[[1]]$dds, test[[1]]$ref, type = "l")
plot(test[[1]]$dds, test[[1]]$control, type = "l")

plot(test2[[1]]$dds, test2[[1]]$ref, type = "l")
plot(test2[[1]]$dds, test2[[1]]$control, type = "l")




plot(test[[1]]$dds, cumsum(test[[1]]$ref), type = "l")
lines(test[[1]]$dds, cumsum(test[[1]]$control))

plot(test2[[1]]$dds, cumsum(test2[[1]]$ref), type = "l")
lines(test2[[1]]$dds, cumsum(test2[[1]]$control))



controlP <- function(eff) {
  aps2 <- round(qJohnsonSB(c(test[[2]][1, ], test[[2]][2, ]), 
                           params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                         lambda = 825.6111)))
  
  ms1 <- dJohnsonSB(seq(500, 1200), 
                    params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                  lambda = 825.6111))
  
  ms1a <- dJohnsonSB(seq(aps2[1], aps2[2]), 
                     params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                   lambda = 825.6111)) * (1 - eff)
  
  ms1b <- dJohnsonSB(seq(aps2[3], aps2[4]), 
                     params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                   lambda = 825.6111)) * (1 - eff)
  
  control <- ms1
  
  control[(aps2[1] - 499):(aps2[2] - 499)] <- ms1a
  control[(aps2[3] - 499):(aps2[4] - 499)] <- ms1b
  control
}

controlP2 <- function(eff) {
  aps2 <- round(qJohnsonSB(c(test2[[2]][1, ], test2[[2]][2, ]), 
                           params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                         lambda = 825.6111)))
  
  ms1 <- dJohnsonSB(seq(500, 1200), 
                    params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                  lambda = 825.6111))
  
  ms1a <- dJohnsonSB(seq(aps2[1], aps2[2]), 
                     params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                   lambda = 825.6111)) * (1 - eff)
  
  ms1b <- dJohnsonSB(seq(aps2[3], aps2[4]), 
                     params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                   lambda = 825.6111)) * (1 - eff)
  
  control <- ms1
  
  control[(aps2[1] - 499):(aps2[2] - 499)] <- ms1a
  control[(aps2[3] - 499):(aps2[4] - 499)] <- ms1b
  control
}


control0 <- controlP(eff = 0.90)
control02 <- controlP2(eff = 0.90)


dJohnsonSB(c(235, 347), 
           params = list(gamma = 1.092, delta = 1.376, xi = 173.590, 
                         lambda = 644.440))


plot(seq(500, 1200), control, type = "l")
lines(seq(500, 1200), control0, type = "l", col = "red")
lines(seq(500, 1200), nocont, type = "l", col = "brown")

plot(test[[1]]$dds, test[[1]]$control, type = "l")


mycol <- t_col("pink", perc = 50, name = "lt.pink")


plot(test[[1]]$dds, test[[1]]$control, type = "l", xlim = c(180, 1200), ylim = c(0, 0.0045), yaxt = "n", xlab = "Degree-days since Jan 1", 
     cex.lab = 2, cex.axis = 2, ylab = "")
axis(2, at = seq(0, 0.0045, 0.001), labels = F)

polygon(test[[1]]$dds, test[[1]]$ref, col = mycol, border = NA)
polygon(test[[1]]$dds, test[[1]]$control, col= "brown")

title(ylab = "Relative number", cex.lab = 2, line = 2)

polygon(seq(500, 1201), c(nocont, 0), col = mycol, border = NA)
polygon(seq(500, 1201), c(control0, 0), col= "brown")

arrows(x0 = c(235, 371), y0 = c(0.001324087 + 0.0002, 0.004161011 + 0.0002), x1 = c(235, 371), y1 = c(0.001324087 + 0.0001, 0.004161011 + 0.0001),
       lwd = 2, length = 0.05)

arrows(x0 = c(629.4195, 847.6741), y0 = c(0.0006908572 + 0.0002, 0.002875899 + 0.0002), 
       x1 = c(629.4195, 847.6741), y1 = c(0.0006908572 + 0.0001, 0.002875899 + 0.0001),
       lwd = 2, length = 0.05, col = "red")

par(mar = c(5, 6, 4, 2) + 0.1)
plot(seq(500, 1200), cumsum(nocont), type = "l", lwd = 2, xlab = "Cumulative degree-days", ylab = "", cex.lab = 2, 
     cex.axis = 1.8, yaxt = "n")
lines(seq(500, 1200), cumsum(control0), type = "l", col = "brown", lwd = 2)
axis(2, seq(0, 1, 0.1), cex.axis = 1.8, las = 2)
title(ylab = "Cumulative proportion", cex.lab = 2, line = 4)







plot(test2[[1]]$dds, test2[[1]]$control, type = "l", xlim = c(180, 1200), ylim = c(0, 0.0045), yaxt = "n", xlab = "Degree-days since Jan 1", 
     cex.lab = 2, cex.axis = 2, ylab = "")
axis(2, at = seq(0, 0.0045, 0.001), labels = F)

polygon(test2[[1]]$dds, test2[[1]]$ref, col = mycol, border = NA)
polygon(test2[[1]]$dds, test2[[1]]$control, col= "brown")

title(ylab = "Relative number", cex.lab = 2, line = 2)

polygon(seq(500, 1201), c(nocont, 0), col = mycol, border = NA)
polygon(seq(500, 1201), c(control02, 0), col= "brown")

arrows(x0 = c(290, 426), y0 = c(0.003530652 + 0.0002, 0.003176581 + 0.0002), x1 = c(290, 426), y1 = c(0.003530652 + 0.0001, 0.003176581 + 0.0001),
       lwd = 2, length = 0.05)

arrows(x0 = c(726.1351, 920.1822), y0 = c(0.002156236 + 0.0002, 0.002525333 + 0.0002), 
       x1 = c(726.1351, 920.1822), y1 = c(0.002156236 + 0.0001, 0.002525333 + 0.0001),
       lwd = 2, length = 0.05, col = "red")

par(mar = c(5, 6, 4, 2) + 0.1)
plot(seq(500, 1200), cumsum(nocont), type = "l", lwd = 2, xlab = "Cumulative degree-days", ylab = "", cex.lab = 2, 
     cex.axis = 1.8, yaxt = "n")
lines(seq(500, 1200), cumsum(control02), type = "l", col = "brown", lwd = 2)
axis(2, seq(0, 1, 0.1), cex.axis = 1.8, las = 2)
title(ylab = "Cumulative proportion", cex.lab = 2, line = 4)










