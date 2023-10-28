control0 <- controlP(eff = 0)
control <- controlP(eff = 0.90)
nocont <- dJohnsonSB(seq(500, 1200), 
                     params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                   lambda = 825.6111))

pred = 700

prod_obs0 <- function(weeks, ns, m) {
  
  obstw <- rep(NA, length(weeks) - 1)
  porps <- rep(NA, (length(weeks) - 1))
  
  for(i in 1:(length(weeks) - 1)) {
    porps[i] <- sum(control0[(weeks[i]:weeks[i + 1]) - 499])
  }
  
  for(i in 1:(length(weeks) - 1)) {
    obstw[i] <- mean(sample(rnbinom(100000, size = key(porps[i]*m), 
                                    mu = porps[i]*m), ns))
  }
  resp <- data.frame(DDs = weeks[2: length(weeks)], moths = obstw, traps = rep(ns, length(obstw)))
  resp
}

t0 <- prod_obs0(seq(550, 1200, 50), ns = 20, m = 20)
mi = cumsum(t0$moths)[which(t0$DDs == pred)]



####

prop <- pJohnsonSB(pred, 
                   params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                 lambda = 825.6111))
steps <- seq(550, 1200, 50)

a1 <- rep(NA, (length(steps) - 1))

for(i in 1:(length(steps) - 1)) {
  a1[i] <- sum(nocont[(steps[i]:steps[i + 1]) - 499])
}

a0 <- rep(NA, (length(steps) - 1))

for(i in 1:(length(steps) - 1)) {
  a0[i] <- sum(control[(steps[i]:steps[i + 1]) - 499])
}


m1 = a1 * mi / prop
m0 = a0 * mi / prop
DDs = steps[-1]


###

cU <- upper(DDs = DDs, m1 = m1, m0 = m0, ns = 20)

cL <- lower(DDs = DDs, m1 = m1, m0 = m0, ns = 20)

ini <- (cU[which(t0$DDs == pred)] + cL[which(t0$DDs == pred)]) / 2

conts0 <- c(ini, cont(t0$moths[-(1:which(t0$DDs == pred))], t0$DDs[-(1:which(t0$DDs == pred))], ns = 20, m1 = m1, m0 = m0, DDs = DDs))

par(mar = c(5, 6, 4, 2) + 0.1)
plot(DDs, cU, type = "l", lwd = 2, xlab = "Cumulative degree-days", ylab = "", cex.lab = 2, 
     cex.axis = 1.8)
lines(DDs, cL, lwd = 2)

title(ylab = "Weigthed cumulative counts", cex.lab = 2, line = 4)

points(c(pred, t0$DDs[-(1:which(t0$DDs == pred))]), cumsum(conts0), type = "o")

plot(DDs, cumsum(m1), type = "o", col = "red")
points(DDs, cumsum(m0), type = "o", col = "red")
points(t0$DDs, cumsum(t0$moths), type = "o")
