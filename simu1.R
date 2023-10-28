#t1 <- prod_obs(seq(500, 1200, 50), 10, 40)
t0 <- prod_obs0(seq(550, 1200, 50), 20, 20)

pred = 700
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

a0 <- rep(NA, (length(a) - 1))

for(i in 1:(length(a) - 1)) {
  a0[i] <- sum(control[(steps[i]:steps[i + 1]) - 499])
}


a02 <- rep(NA, (length(a) - 1))

for(i in 1:(length(a) - 1)) {
  a02[i] <- sum(control02[(steps[i]:steps[i + 1]) - 499])
}

m1 = a1 * mi / prop
m0 = a0 * mi / prop
m02 = a02 * mi / prop
DDs = steps[-1]

plot(DDs, m1, type = "l")
lines(DDs, m0, col = "blue")
lines(DDs, m02, col = "red")

###


cU <- upper(DDs = DDs, m1 = m1, m0 = m0, ns = 20)
cU2 <- upper(DDs = DDs, m1 = m1, m0 = m02, ns = 20)

cL <- lower(DDs = DDs, m1 = m1, m0 = m0, ns = 20)
cL2 <- lower(DDs = DDs, m1 = m1, m0 = m02, ns = 20)

plot(DDs, cU, type = "l")
lines(DDs, cL)

plot(DDs, cU2, type = "l")
lines(DDs, cL2)




ini <- (cU[which(t0$DDs == pred)] + cL[which(t0$DDs == pred)]) / 2

#conts1 <- c(ini, cont(t1$moths[-(1:6)], t1$DDs[-(1:6)], ns = 10, m1 = m1, m0 = m0, DDs = DDs))
conts0 <- c(ini, cont(t0$moths[-(1:which(t0$DDs == pred))], t0$DDs[-(1:which(t0$DDs == pred))], ns = 20, m1 = m1, m0 = m0, DDs = DDs))

#points(seq(pred, 1200, 50), cumsum(conts1), type = "o")
points(seq(pred, 1200, 50), cumsum(conts0), type = "o", col = "red")

#plot(t0$DDs, cumsum(t0$moths))

abo <- which(cumsum(conts0) > cU[which(t0$DDs == pred): length(cU)])
belo <- which(cumsum(conts0) < cL[which(t0$DDs == pred): length(cU)])

if(is.na(abo[1]) & !is.na(belo[1])) resp <- 1 else resp <- 0

resp
belo[1]




par(mar = c(5, 6, 4, 2) + 0.1)
plot(DDs, cU, type = "l", lwd = 2, xlab = "Cumulative degree-days", ylab = "", cex.lab = 2, 
     cex.axis = 1.8, yaxt = "n", ylim = c(0, 65))
lines(DDs, cL, lwd = 2)
axis(2, seq(0, 65, 10), cex.axis = 1.8, las = 2)
title(ylab = "Weigthed cumulative counts", cex.lab = 2, line = 4)



par(mar = c(5, 6, 4, 2) + 0.1)
plot(DDs, cU2, type = "l", lwd = 2, xlab = "Cumulative degree-days", ylab = "", cex.lab = 2, 
     cex.axis = 1.8, yaxt = "n", ylim = c(0, 65))
lines(DDs, cL2, lwd = 2)
axis(2, seq(0, 65, 10), cex.axis = 1.8, las = 2)
title(ylab = "Weigthed cumulative counts", cex.lab = 2, line = 4)
