pred = 800
mi = 7

nocont <- dJohnsonSB(seq(500, 1200), 
                     params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                   lambda = 825.6111))
####

prop <- pJohnsonSB(pred, 
                   params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                 lambda = 825.6111))
den <- dJohnsonSB(pred, 
                  params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                lambda = 825.6111))

miN <- mi * den / prop


f0 = control[pred - 499]
f1 = nocont[pred - 499]

dia <- ((f1 - f0) / 1.5)

m0 = (control + dia) * miN / den
m1 = (nocont + dia) * miN / den




plot(seq(500, 1200), m1, type = "l")
lines(seq(500, 1200), m0)
points(800, mi)


m1 = diff(m1c)
m0 = diff(m0c)


plot(seq(501, 1200), m1, type = "l")
lines(seq(501, 1200), m0)
points(800, miN)




ns = 10

k = 0.5

#a <- log((1 - 0.01)/(0.01)) + (k *
                            #cumsum(ns * log((key1(m0[t - 499]) * (key1(m1[t - 499]) + m1[t - 499])) / 
                             #           (key1(m1[t - 499]) * (key1(m0[t - 499]) + m0[t - 499])))))

a <- log((1 - 0.01)/(0.01)) + (k *
                                 cumsum(10 * log((k + m1[t - 498]) / 
                                                   (k + m0[t -498]))))


#b <- log((0.01)/(1 - 0.01)) + (k *
                            #cumsum(ns * log((key1(m0[t - 499]) * (key1(m1[t - 499]) + m1[t - 499])) / 
                             #           (key1(m1[t - 499]) * (key1(m0[t - 499]) + m0[t - 499])))))

b <- log((0.01)/(1 - 0.01)) + (k *
                                 cumsum(10 * log((k + m1[t - 498]) / 
                                                   (k + m0[t - 498]))))


par(mar = c(5, 5, 4, 2) + 0.1)
plot(seq(500, 1200), a, col = "blue", type = "l", xlim = c(800, 900),# ylim = c(0, 500), #c(0, ms_up[length(ms_up)]),
     xlab = "Cumulative degree-days", ylab = "Cumulative average counts", cex.lab = 2, cex.axis = 1.8, lwd = 2, yaxt = "n")
lines(seq(500, 1200), b, col = "brown", type = "l", lwd = 2)

axis(2, seq(0, 1000, 10), cex.axis = 1.8, las = 2)

points(800, cum_counts(miu  = 7, ns = 10, t = 800, mi = 7, pred = 800))

plot(mid, key1(mid))


plot(seq(500, 1200), (pJohnsonSB(seq(500, 1200), 
                                 params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                               lambda = 825.6111)) * mi / prop) + ((f1 - f0) / 2), type = "l")

lines(seq(500, 1200), (cumsum(control) * mi / prop) + ((f1  - f0) / 2))
points(800, 7)
points(800, m0[pred-499])
points(800, m1[pred-499])


plot(cumsum(m1))
lines(cumsum(m0))
 
diff <- nocont[(800 - 499)] - (control)[(800 - 499)]




plot(seq(500, 1200), ((nocont[(500 - 499) : (1200 - 499)] * mi / prop) + (diff / 2) * mi / prop), type = "l")
lines(seq(500, 1200), ((control[(500 - 499) : (1200 - 499)] * mi / prop) + (diff / 2) * mi / prop), col  = "red")


points(800, nocont[(800 - 499)] * mi / prop)








plot(seq(500, 1200), cumsum((nocont[(500 - 499) : (1200 - 499)] + (diff / 2)) * mi / prop), type = "l")
lines(seq(500, 1200), cumsum((control[(500 - 499) : (1200 - 499)])) * mi / prop, col  = "red")
points(800, mi)


points(800, (cumsum((control[(500 - 499) : (1200 - 499)])) * mi / prop)[800-499])
points(800, cumsum((nocont[(500 - 499) : (1200 - 499)] + (diff / 2)) * mi / prop)[800-499])
