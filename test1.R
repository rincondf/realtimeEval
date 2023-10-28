cum_counts <- function(miu, ns, t, mi, pred) {
  
  prop <- pJohnsonSB(pred, 
                     params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                   lambda = 825.6111))
  
  
  prop <- pJohnsonSB(pred, 
                     params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                   lambda = 825.6111))
  
  
  f0 = control[pred - 499] * mi / prop
  f1 = dJohnsonSB(seq(500, 1200), 
                  params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                lambda = 825.6111))[pred - 499] * mi / prop
  
  
  m0 = (control * mi / prop) + ((diff / 2) * mi / prop)
  m1 = (dJohnsonSB(seq(500, 1200), 
                   params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                 lambda = 825.6111)) * mi / prop) + ((diff / 2) * mi / prop)
  
  miu * ns * (log((key1(m0[t - 499]) * ((m1[t - 499] * key1(m1[t - 499]) * key1(m0[t - 499])) + (m1[t - 499] * m0[t - 499]))) / 
                    (key1(m1[t - 499]) * ((m0[t - 499] * key1(m0[t - 499]) * key1(m1[t - 499])) + (m1[t - 499] * m0[t - 499])))))
}


cum_counts <- function(miu, ns, t, mi, pred) {
  k = 0.3
  
  prop <- pJohnsonSB(pred, 
                     params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                   lambda = 825.6111))
  den <- dJohnsonSB(pred, 
                    params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                  lambda = 825.6111))
  
  miN <- mi * den / prop
  
  m0 = (control) * miN / den
  m1 = (nocont) * miN / den
  
  
  miu * ns * (log((key1(m0[t - 499]) * ((m1[t - 499] * key1(m1[t - 499]) * key1(m0[t - 499])) + (m1[t - 499] * m0[t - 499] * key1(m1[t - 499])))) / 
                    (key1(m1[t - 499]) * ((m0[t - 499] * key1(m0[t - 499]) * key1(m1[t - 499])) + (m1[t - 499] * m0[t - 499] * key1(m0[t - 499]))))))
  
  
}

t = 800

(log((key1(m0[t - 499]) * ((m1[t - 499] * key1(m1[t - 499]) * key1(m0[t - 499])) + (m1[t - 499] * m0[t - 499] * key1(m1[t - 499])))) / 
       (key1(m1[t - 499]) * ((m0[t - 499] * key1(m0[t - 499]) * key1(m1[t - 499])) + (m1[t - 499] * m0[t - 499] * key1(m0[t - 499]))))))

7* 10 * (log(m1[t - 499] / m0[t - 499]) - log((k + m1[t - 499]) / (k + m0[t - 499])))



mi = 7
pred = 800

prop <- pJohnsonSB(pred, 
                   params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                 lambda = 825.6111))



ms0 <- (control * mi / prop) 
ms1 <- (dJohnsonSB(seq(500, 1200), 
           params = list(gamma = 0.3968, delta = 1.4682, xi = FDD_CDD(890.67), 
                         lambda = FDD_CDD(1486.1))) * mi / prop)

ms0a <- cumsum(ms0) + ((cumsum(ms1)[pred - 499] + cumsum(ms0)[pred - 499]) / 2)
ms1a <- cumsum(ms1) + ((cumsum(ms1)[pred - 499] + cumsum(ms0)[pred - 499]) / 2)


plot(seq(500, 1200), ms1a, type = "l")
lines(seq(500, 1200), ms0a, type = "l")
points(pred, 7)



points(pred, cum_counts(miu  = 7, ns = 10, t = 800, mi = 7, pred = 800))

