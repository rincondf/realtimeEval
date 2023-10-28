
upper_t <- function(t, mi) {
  
  prop <- pJohnsonSB(800, 
                     params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                   lambda = 825.6111))
  
  
  c = control * mi / prop
  d = dJohnsonSB(seq(500, 1200), 
                 params = list(gamma = 0.3968, delta = 1.4682, xi = FDD_CDD(890.67), 
                               lambda = FDD_CDD(1486.1))) * mi / prop
  k = 0.3
  ns = 10
  log((1 - 0.01)/(0.01)) + (k * cumsum(ns * log((k + d[t - 499]) / (k + c[t - 499]))))
}


lower_t <- function(t, mi) {
  
  prop <- pJohnsonSB(800, 
                     params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                   lambda = 825.6111))
  
  
  c = control * mi / prop
  d = dJohnsonSB(seq(500, 1200), 
                 params = list(gamma = 0.3968, delta = 1.4682, xi = FDD_CDD(890.67), 
                               lambda = FDD_CDD(1486.1))) * mi / prop
  k = 0.3
  ns = 10
  log((0.01)/(1 - 0.01)) + (k * cumsum(ns * log((k + d[t - 499]) / (k + c[t - 499]))))
}


par(mar = c(5, 5, 4, 2) + 0.1)
plot(seq(630, 1200), upper_t(seq(630, 1200), 5), col = "blue", type = "l", xlim = c(600, 1000),# ylim = c(0, 500), #c(0, ms_up[length(ms_up)]),
     xlab = "Cumulative degree-days", ylab = "Cumulative average counts", cex.lab = 2, cex.axis = 1.8, lwd = 2, yaxt = "n")

lines(seq(630, 1200), lower_t(seq(630, 1200), 5), col = "brown", type = "l", lwd = 2)

axis(2, seq(0, 1000, 10), cex.axis = 1.8, las = 2)


prop <- pJohnsonSB(900, 
                   params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                 lambda = 825.6111))

mi = 5
c = control * mi / prop
d = pJohnsonSB(seq(500, 1200), 
               params = list(gamma = 0.3968, delta = 1.4682, xi = FDD_CDD(890.67), 
                             lambda = FDD_CDD(1486.1))) * mi / prop

lines(seq(500, 1200), d, type = "l")
lines(seq(500, 1200), cumsum(c))

