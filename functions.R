FDD_CDD <- function(x) {
  (5/9) * x
}


key1 <- function(miu) {
  respu <- rep(NA, length(miu))
  
  for(i in 1: length(miu)) {
    if(miu[i] <= 0.07902071) {
      a <- 2.057018
      b <- 1.160769
    } else {
      a <- 6.411532
      b <- 1.608689
    }
    
    respu[i] <- (miu[i]^2) / ((a * miu[i]^b) - miu[i])
    if(respu[i] <= 0.15) respu[i] <- 0.15
  }
  
  respu
  
}

key <- function(miu) {
  respu <- rep(NA, length(miu))
  
  for(i in 1: length(miu)) {
    if(miu[i] <= 0.07902071) {
      a <- 2.057018
      b <- 1.160769
      err <- 0.2939689
    } else {
      a <- 6.411532
      b <- 1.608689
      err <- 0.5904043
    }
    
    if(miu[i] <= 0) {
      respu[i] <- 0
    } else {
      respu[i] <- (miu[i]^2) / ((a * miu[i]^b * exp(rnorm(1, mean = 0, sd = err))) - miu[i])
      if(respu[i] <= 0) respu[i] <- 1e-10
    }
    
  }
  
  respu
  
}


prod_obs <- function(weeks, ns, m) {
  
  obstw <- rep(NA, length(weeks) - 1)
  porps <- rep(NA, (length(weeks) - 1))
  
  for(i in 1:(length(weeks) - 1)) {
    porps[i] <- sum(dJohnsonSB(seq(weeks[i], weeks[i + 1], 1), 
                               params = list(gamma = 0.3968, delta = 1.4682, xi = 494.8167, 
                                             lambda = 825.6111)))
  }
  
  for(i in 1:(length(weeks) - 1)) {
    obstw[i] <- mean(sample(rnbinom(100000, size = key(porps[i]*m), 
                                    mu = porps[i]*m), ns))
  }
  resp <- data.frame(DDs = weeks[2: length(weeks)], moths = obstw, traps = rep(ns, length(obstw)))
  resp
}


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


upper <- function(DDs, m1, m0, ns) {
  log((1 - 0.00001)/(0.01)) + (0.5 *
                              cumsum(ns * log((key1(m0) * (key1(m1) + m1)) / 
                                                (key1(m1) * (key1(m0) + m0)))))
}


lower <- function(DDs, m1, m0, ns) {
  log((0.00001)/(1 - 0.01)) + (0.5 *
                              cumsum(ns * log((key1(m0) * (key1(m1) + m1)) / 
                                                (key1(m1) * (key1(m0) + m0)))))
}

cont <- function(x, DD, ns, m1, m0, DDs) {
  resp <- rep(NA, length(x))
  
  for(i in 1:length(x)) {
    resp[i] <- x[i] * ns * (log((key1(m0[which(DDs == DD[i])]) * ((m1[which(DDs == DD[i])] * key1(m1[which(DDs == DD[i])]) * key1(m0[which(DDs == DD[i])])) + (m1[which(DDs == DD[i])] * m0[which(DDs == DD[i])] * key1(m1[which(DDs == DD[i])])))) / 
                                  (key1(m1[which(DDs == DD[i])]) * ((m0[which(DDs == DD[i])] * key1(m0[which(DDs == DD[i])]) * key1(m1[which(DDs == DD[i])])) + (m1[which(DDs == DD[i])] * m0[which(DDs == DD[i])] * key1(m0[which(DDs == DD[i])]))))))
  }
  
  resp
  
}

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
