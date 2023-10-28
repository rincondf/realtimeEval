ped0 <- c(2, 3, 4, 7, 8, 6, 3, 2, 1) * 1
ped1 <- c(4, 5, 16, 18, 23, 38, 34, 26, 25) * 1




plot(ped1, type = "l")
lines(ped0)

k = 1.16

a <- log((1 - 0.01)/(0.01)) + (k *
                                 cumsum(10 * log((k + ped1) / 
                                                   (k + ped0))))



b <- log((0.01)/(1 - 0.01)) + (k *
                                 cumsum(10 * log((k + ped1) / 
                                                   (k + ped0))))
lines(a, type = "l", ylim = c(0, 100), xlim = c(0, 8), col  = "brown")
lines(b, col  = "brown")

t = 4

(log((key1(ped0[t]) * ((ped1[t] * key1(ped1[t]) * key1(ped0[t])) + (ped1[t] * ped0[t] * key1(ped1[t])))) / 
       (key1(ped1[t]) * ((ped0[t] * key1(ped0[t]) * key1(ped1[t])) + (ped1[t] * ped0[t] * key1(ped0[t]))))))


6 * 10 * (log(ped1[3] / ped0[3]) - log((k + ped1[3]) / (k + ped0[3])))

####



den <- dJohnsonSB(pred, 
                  params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                lambda = 825.6111))


sum(dJohnsonSB(seq(750, 800), 
          params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                        lambda = 825.6111)))

sum(nocont[(750:800) - 499])

m0 = (control) * miN / den
m1 = (nocont) * miN / den

plot(seq(500, 1200), (m1), type = "l")
lines(seq(500, 1200), (m0))
points(800, miN)


cum_counts(miu  = 7, ns = 10, t = 800, mi = 7, pred = 800)

points(800, a[800 - 499])
points(800, b[800 - 499])

corr <- abs((b[800 - 499] + (a[800 - 499] - b[800 - 499]) / 2) - cum_counts(miu  = 7, ns = 10, t = 800, mi = 7, pred = 800))




k = 0.3

a <- log((1 - 0.01)/(0.01)) + (k *
                                 cumsum(10 * log((k + m1) / 
                                                   (k + m0))))



b <- log((0.01)/(1 - 0.01)) + (k *
                                 cumsum(10 * log((k + m1) / 
                                                   (k + m0))))
plot(seq(500, 1200), a + corr, type = "l")
lines(seq(500, 1200), b + corr)
points(800, cum_counts(miu  = 7, ns = 10, t = 800, mi = 7, pred = 800))
points(850, cum_counts(miu  = 7.5, ns = 10, t = 800, mi = 7, pred = 800))

cum_counts(miu  = miN, ns = 10, t = 800, mi = 7, pred = 800)

