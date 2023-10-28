

simuOC <- function(den = 20, n = 20, pred = 700, eff = 0.90) {
  
  control0 <- controlP(eff = eff)
  control <- controlP(eff = 0.90)
  nocont <- dJohnsonSB(seq(500, 1200), 
                       params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                     lambda = 825.6111))
  
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
  
  mi = 0
  
  while(mi == 0) {
    t0 <- prod_obs0(seq(550, 1200, 50), ns = n, m = den)
    mi = cumsum(t0$moths)[which(t0$DDs == pred)]
  }
  
  
  
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
  
  cU <- upper(DDs = DDs, m1 = m1, m0 = m0, ns = n)
  
  cL <- lower(DDs = DDs, m1 = m1, m0 = m0, ns = n)
  
  ini <- (cU[which(t0$DDs == pred)] + cL[which(t0$DDs == pred)]) / 2
  
  conts0 <- c(ini, cont(t0$moths[-(1:which(t0$DDs == pred))], t0$DDs[-(1:which(t0$DDs == pred))], ns = n, m1 = m1, m0 = m0, DDs = DDs))
  
  abo <- which(cumsum(conts0) > cU[which(t0$DDs == pred): length(cU)])
  belo <- which(cumsum(conts0) < cL[which(t0$DDs == pred): length(cU)])
  
  if(is.na(abo[1]) & !is.na(belo[1])) resp <- 1
  
  if(!is.na(abo[1]) & is.na(belo[1])) resp <- 0
  
  if(is.na(abo[1]) & is.na(belo[1])) resp <- 0
  
  if(!is.na(abo[1]) & !is.na(belo[1])) {
    if(length(abo) < length(belo)) resp <- 1 else resp <- 0
  }
  
  resp
}




jj <- replicate(100, simuOC(den = 20, n = 20, eff = 0))

sum(jj) / 100

