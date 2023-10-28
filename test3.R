

a <- seq(550, 1200, 50)

b <- rep(NA, (length(a) - 1))

for(i in 1:(length(a) - 1)) {
  b[i] <- sum(nocont[(a[i]:a[i + 1]) - 499])
}

b1 <- rep(NA, (length(a) - 1))

for(i in 1:(length(a) - 1)) {
  b1[i] <- sum(control[(a[i]:a[i + 1]) - 499])
}


m1 = b * mi / prop
m0 = b1 * mi / prop

plot(a[-1], m1, type = "l")
lines(a[-1], m0)


k = 0.3

c <- log((1 - 0.01)/(0.01)) + (k *
                                 cumsum(10 * log((k + m1) / 
                                                   (k + m0))))



d <- log((0.01)/(1 - 0.01)) + (k *
                                 cumsum(10 * log((k + m1) / 
                                                   (k + m0))))
lines(a[-1], c, type = "l", ylim = c(0, 65), xlim = c(500, 1200), col  ="red")
lines(a[-1], d, col = "red")

fst <- (c[1] + d[1]) / 2

points(600, fst)

sec <- 6 * 10 * (log(m1[3] / m0[3]) - log((k + m1[3]) / (k + m0[3])))


tir <- 4* 10 * (log(m1[10] / m0[10]) - log((k + m1[10]) / (k + m0[10])))

points(700, fst + sec)
points(1050, fst + sec + tir)


#####






c1 <- log((1 - 0.01)/(0.01)) + (0.5 *
                                  cumsum(10 * log((key1(m0) * (key1(m1) + m1)) / 
                                                    (key1(m1) * (key1(m0) + m0)))))


d1 <- log((0.01)/(1 - 0.01)) + (0.5 *
                                  cumsum(10 * log((key1(m0) * (key1(m1) + m1)) / 
                                                    (key1(m1) * (key1(m0) + m0)))))



plot(a[-1], c1, type = "l", ylim = c(0, 65), xlim = c(600, 1200))
lines(a[-1], d1)
fst <- (c1[1] + d1[1]) / 2

points(600, fst)

t = 2

p1 <- 6 * 10 * (log((key1(m0[t]) * ((m1[t] * key1(m1[t]) * key1(m0[t])) + (m1[t] * m0[t] * key1(m1[t])))) / 
                        (key1(m1[t]) * ((m0[t] * key1(m0[t]) * key1(m1[t])) + (m1[t] * m0[t] * key1(m0[t]))))))

points(650, fst + p1)

t = 5
p2 <- 10 * 10 * (log((key1(m0[t]) * ((m1[t] * key1(m1[t]) * key1(m0[t])) + (m1[t] * m0[t] * key1(m1[t])))) / 
                      (key1(m1[t]) * ((m0[t] * key1(m0[t]) * key1(m1[t])) + (m1[t] * m0[t] * key1(m0[t]))))))

points(800, fst + p1 + p2)


